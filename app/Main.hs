{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}

module Main where

import           Control.Lens
import           Control.Monad
import           Data.Aeson                 as A
import           Data.Aeson.Lens
import           Data.Either
import           Data.Foldable (traverse_)
import           Data.List (sortOn)
import           Data.Map (Map)
import           Data.Ord (Down(..))
import           Data.Set (Set)
import           Data.Time
import           Development.Shake
import           Development.Shake.Classes
import           Development.Shake.Forward
import           Development.Shake.FilePath
import           GHC.Generics               (Generic)
import           Slick
import           Slick.Pandoc
import           Skylighting.Loader
import           Skylighting.Syntax
import           Text.Pandoc.Options

import qualified Data.HashMap.Lazy as HML
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text                  as T

---Config-----------------------------------------------------------------------

siteMeta :: SiteMeta
siteMeta =
    SiteMeta { siteAuthor = "Luc Tielen"
             , baseUrl = "https://luctielen.com"
             , siteTitle = "FP -> Compilers -> Logic -> Blog"
             , twitterHandle = Just "luctielen"
             , twitchHandle = Just "luctielen"
             , youtubeHandle = Just "UCeMz1NwTQlkhQvIFYMZoAJQ"
             , githubUser = Just "luc-tielen"
             }

outputFolder :: FilePath
outputFolder = "build/"

--Data models-------------------------------------------------------------------

withSiteMeta :: Value -> Value
withSiteMeta (Object obj) = Object $ HML.union obj siteMetaObj
  where
    Object siteMetaObj = toJSON siteMeta
withSiteMeta _ = error "only add site meta to objects"

data SiteMeta
  = SiteMeta
  { siteAuthor    :: String
  , baseUrl       :: String -- e.g. https://example.ca
  , siteTitle     :: String
  , twitterHandle :: Maybe String -- Without @
  , githubUser    :: Maybe String
  , twitchHandle  :: Maybe String
  , youtubeHandle  :: Maybe String
  } deriving (Generic, Eq, Ord, Show, ToJSON)

-- | Data for the (posts) index page
data IndexInfo
  = IndexInfo
  { posts :: [Post]
  , allTags :: Set Tag
  } deriving (Generic, Show, FromJSON, ToJSON)

data IndexWithTagInfo
  = IndexWithTagInfo
  { tag :: Tag
  , posts :: [Post]
  } deriving (Generic, Show, FromJSON, ToJSON)

type Tag = String

-- | Data for a blog post
data Post
  = Post
  { title       :: String
  , author      :: String
  , content     :: String
  , url         :: String
  , postDate    :: String
  , tags        :: [Tag]
  , image       :: Maybe String
  } deriving (Generic, Eq, Ord, Show, FromJSON, ToJSON, Binary)

-- | Data for a video post
data Video
  = Video
  { title :: String
  , author :: String
  , content :: String
  , url :: String
  , videoDate :: String
  , youtubeUrl :: String
  } deriving (Generic, Eq, Ord, Show, FromJSON, ToJSON, Binary)

-- | Data for the (videos) index page
newtype VideoIndexInfo = VideoIndexInfo { videos :: [Video] }
  deriving (Generic, Show, FromJSON, ToJSON)

data AtomData
  = AtomData
  { title        :: String
  , domain       :: String
  , author       :: String
  , posts        :: [Post]
  , currentTime  :: String
  , atomUrl      :: String
  } deriving (Generic, ToJSON, Eq, Ord, Show)

-- | Given a list of posts this will build a table of contents
buildPostsIndex :: [Post] -> Set Tag -> Action ()
buildPostsIndex posts' tags' = do
  indexT <- compileTemplate' "site/templates/index.html"
  let indexInfo = IndexInfo {posts = posts', allTags = tags'}
      html = T.unpack $ substitute indexT (withSiteMeta $ toJSON indexInfo)
  writeFile' (outputFolder </> "index.html") html

-- | Build the tagged index pages
buildTaggedPostsIndex :: Set Tag -> [Post] -> Action ()
buildTaggedPostsIndex tags =
  traverse_ (uncurry buildTaggedIndex) . Map.toList . groupPostsByTag tags
  where
    buildTaggedIndex tag' posts' = do
      indexT <- compileTemplate' "site/templates/posts-with-tag.html"
      let info = IndexWithTagInfo {tag = tag', posts = posts'}
          html = T.unpack $ substitute indexT (withSiteMeta $ toJSON info)
      writeFile' (outputFolder </> "tag" </> tag' </> "index.html") html

groupPostsByTag :: Set Tag -> [Post] -> Map Tag [Post]
groupPostsByTag tags' posts' =
  Map.fromList [(t, ps) | t <- Set.toList tags', let ps = filter (matchesTag t) posts']
  where matchesTag t p = t `elem` tags p

-- | Build the video index page
buildVideosIndex :: [Video] -> Action ()
buildVideosIndex videos' = do
  videoIndexT <- compileTemplate' "site/templates/videos.html"
  let indexInfo = VideoIndexInfo { videos = videos' }
      html = T.unpack $ substitute videoIndexT $ withSiteMeta $ toJSON indexInfo
  writeFile' (outputFolder </> "videos" </> "index.html") html

-- | Build the about page
buildAbout :: Action ()
buildAbout = do
  aboutT <- compileTemplate' "site/templates/about.html"
  let html = T.unpack $ substitute aboutT $ toJSON siteMeta
  writeFile' (outputFolder </> "about" </> "index.html") html

-- | Find and build all posts
buildPosts :: Action [Post]
buildPosts = buildX "posts" buildPost

-- | Find and build all videos
buildVideos :: Action [Video]
buildVideos = buildX "videos" buildVideo

buildX :: String -> (FilePath -> Action a) -> Action [a]
buildX x f = do
  pPaths <- getDirectoryFiles "." ["site" </> x </> "*.md"]
  forP pPaths f

-- | Load a post, process metadata, write it to output, then return the post object
-- Detects changes to either post content or template
buildPost :: FilePath -> Action Post
buildPost srcPath = cacheAction ("build" :: T.Text, srcPath) $ do
  liftIO . putStrLn $ "Rebuilding blog post: " <> srcPath
  postContent <- readFile' srcPath
  -- load post content and metadata as JSON blob
  postData <- mdToHTML . T.pack $ postContent
  let postUrl = T.pack $ dropDirectory1 $ dropExtension srcPath
      withPostUrl = _Object . at "url" ?~ String ("/" <> postUrl)
  -- Add additional metadata we've been able to compute
  let fullPostData = withSiteMeta . withPostUrl $ postData
  template <- compileTemplate' "site/templates/post.html"
  writeFile' (outputFolder </> T.unpack postUrl </> "index.html") . T.unpack $ substitute template fullPostData
  convert fullPostData

buildVideo :: FilePath -> Action Video
buildVideo srcPath = cacheAction ("build" :: T.Text, srcPath) $ do
  liftIO . putStrLn $ "Rebuilding video post: " <> srcPath
  videoPostContent <- readFile' srcPath
  -- load post content and metadata as JSON blob
  videoPostData <- mdToHTML . T.pack $ videoPostContent
  let videoPostUrl = T.pack $ dropDirectory1 $ dropExtension srcPath
      withVideoPostUrl = _Object . at "url" ?~ String ("/" <> videoPostUrl)
  -- Add additional metadata we've been able to compute
  let fullVideoPostData = withSiteMeta . withVideoPostUrl $ videoPostData
  template <- compileTemplate' "site/templates/video.html"
  let renderedTemplate = substitute template fullVideoPostData
      videoHtmlFile = outputFolder </> T.unpack videoPostUrl </> "index.html"
  writeFile' videoHtmlFile . T.unpack $ renderedTemplate
  convert fullVideoPostData

-- | Copy all static files from the listed folders to their destination
copyStaticFiles :: Action ()
copyStaticFiles = do
  filepaths <- getDirectoryFiles "./site/" ["images//*", "css//*", "js//*"]
  void $ forP filepaths $ \filepath ->
    copyFileChanged ("site" </> filepath) (outputFolder </> filepath)

class HasDate a where
  date :: a -> String

instance HasDate Post where
  date = postDate

instance HasDate Video where
  date = videoDate

sortByDate :: HasDate a => [a] -> [a]
sortByDate = sortOn descendingPostDate where
  descendingPostDate = Down . parseDate . date

formatDate :: String -> String
formatDate = toIsoDate . parseDate

parseDate :: String -> UTCTime
parseDate = parseTimeOrError True defaultTimeLocale "%b %e, %Y"

rfc3339 :: Maybe String
rfc3339 = Just "%H:%M:%SZ"

toIsoDate :: UTCTime -> String
toIsoDate = formatTime defaultTimeLocale (iso8601DateFormat rfc3339)

buildFeed :: [Post] -> Action ()
buildFeed posts = do
  now <- liftIO getCurrentTime
  let atomData =
        AtomData
          { title = siteTitle siteMeta
          , domain = baseUrl siteMeta
          , author = siteAuthor siteMeta
          , posts = mkAtomPost <$> posts
          , currentTime = toIsoDate now
          , atomUrl = "/atom.xml"
          }
  atomTempl <- compileTemplate' "site/templates/atom.xml"
  writeFile' (outputFolder </> "atom.xml") . T.unpack $ substitute atomTempl (toJSON atomData)
  where
    mkAtomPost :: Post -> Post
    mkAtomPost p = p { postDate = formatDate $ postDate p }

extractUniqueTags :: [Post] -> Set Tag
extractUniqueTags posts' = Set.fromList [t | p <- posts', t <- tags p]

-- | Specific build rules for the Shake system
--   defines workflow to build the website
buildRules :: Action ()
buildRules = do
  allPosts <- sortByDate <$> buildPosts
  allVideos <- sortByDate <$> buildVideos
  let allTags = extractUniqueTags allPosts
  buildPostsIndex allPosts allTags
  buildTaggedPostsIndex allTags allPosts
  buildVideosIndex allVideos
  buildFeed allPosts  -- TODO add videos?
  buildAbout
  copyStaticFiles

mdToHTML :: T.Text -> Action Value
mdToHTML txt = do
  extraSyntaxes <- liftIO $ loadSyntaxesFromDir "app/syntax"
  let extraSyntaxes' = fromRight defaultSyntaxMap extraSyntaxes
      htmlOpts = defaultHtml5Options
      syntaxMap = writerSyntaxMap htmlOpts <> extraSyntaxes'
      htmlOptions = htmlOpts { writerSyntaxMap = syntaxMap }
  markdownToHTMLWithOpts defaultMarkdownOptions htmlOptions txt

main :: IO ()
main = do
  let shOpts = shakeOptions { shakeVerbosity = Chatty, shakeLintInside = ["\\"]}
  shakeArgsForward shOpts buildRules

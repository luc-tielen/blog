{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TupleSections         #-}

module Main where

import           Control.Lens
import           Control.Monad
import           Data.Aeson                 as A
import           Data.Aeson.Lens
import           Data.Foldable (traverse_)
import           Data.Map (Map)
import           Data.Set (Set)
import           Data.Time
import           Development.Shake
import           Development.Shake.Classes
import           Development.Shake.Forward
import           Development.Shake.FilePath
import           GHC.Generics               (Generic)
import           Slick

import qualified Data.HashMap.Lazy as HML
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text                  as T

---Config-----------------------------------------------------------------------

siteMeta :: SiteMeta
siteMeta =
    SiteMeta { siteAuthor = "Luc Tielen"
             , baseUrl = "https://luctielen.com"
             , siteTitle = "Luc's personal blog"
             , twitterHandle = Just "luctielen"
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
  = SiteMeta { siteAuthor    :: String
  , baseUrl       :: String -- e.g. https://example.ca
  , siteTitle     :: String
  , twitterHandle :: Maybe String -- Without @
  , githubUser    :: Maybe String
  } deriving (Generic, Eq, Ord, Show, ToJSON)

-- | Data for the index page
data IndexInfo
  = IndexInfo
  { posts :: [Post]
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
  , date        :: String
  , tags        :: [Tag]
  , image       :: Maybe String
  } deriving (Generic, Eq, Ord, Show, FromJSON, ToJSON, Binary)

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
buildIndex :: [Post] -> Action ()
buildIndex posts' = do
  indexT <- compileTemplate' "site/templates/index.html"
  let indexInfo = IndexInfo {posts = posts'}
      html = T.unpack $ substitute indexT (withSiteMeta $ toJSON indexInfo)
  writeFile' (outputFolder </> "index.html") html

-- | Build the tagged index pages
buildTaggedIndex :: Set Tag -> [Post] -> Action ()
buildTaggedIndex tags =
  traverse_ (uncurry buildTaggedIndex') . Map.toList . groupPostsByTag tags
  where
    buildTaggedIndex' tag' posts' = do
      indexT <- compileTemplate' "site/templates/posts-with-tag.html"
      let info = IndexWithTagInfo {tag = tag', posts = posts'}
          html = T.unpack $ substitute indexT (withSiteMeta $ toJSON info)
      writeFile' (outputFolder </> "tag" </> tag' </> "index.html") html

groupPostsByTag :: Set Tag -> [Post] -> Map Tag [Post]
groupPostsByTag tags' posts' =
  Map.fromList [(t, ps) | t <- Set.toList tags', let ps = filter (matchesTag t) posts']
  where matchesTag t p = t `elem` tags p

-- | Build the about page
buildAbout :: Action ()
buildAbout = do
  aboutT <- compileTemplate' "site/templates/about.html"
  let html = T.unpack $ substitute aboutT $ toJSON siteMeta
  writeFile' (outputFolder </> "about" </> "index.html") html

-- | Find and build all posts
buildPosts :: Action [Post]
buildPosts = do
  pPaths <- getDirectoryFiles "." ["site/posts//*.md"]
  forP pPaths buildPost

-- | Load a post, process metadata, write it to output, then return the post object
-- Detects changes to either post content or template
buildPost :: FilePath -> Action Post
buildPost srcPath = cacheAction ("build" :: T.Text, srcPath) $ do
  liftIO . putStrLn $ "Rebuilding post: " <> srcPath
  postContent <- readFile' srcPath
  -- load post content and metadata as JSON blob
  postData <- markdownToHTML . T.pack $ postContent
  let postUrl = T.pack $ dropDirectory1 $ dropExtension srcPath
      withPostUrl = _Object . at "url" ?~ String ("/" <> postUrl)
  -- Add additional metadata we've been able to compute
  let fullPostData = withSiteMeta . withPostUrl $ postData
  template <- compileTemplate' "site/templates/post.html"
  writeFile' (outputFolder </> T.unpack postUrl </> "index.html") . T.unpack $ substitute template fullPostData
  convert fullPostData

-- | Copy all static files from the listed folders to their destination
copyStaticFiles :: Action ()
copyStaticFiles = do
  filepaths <- getDirectoryFiles "./site/" ["images//*", "css//*", "js//*"]
  void $ forP filepaths $ \filepath ->
    copyFileChanged ("site" </> filepath) (outputFolder </> filepath)

formatDate :: String -> String
formatDate humanDate = toIsoDate parsedTime
  where
    parsedTime =
      parseTimeOrError True defaultTimeLocale "%b %e, %Y" humanDate :: UTCTime

rfc3339 :: Maybe String
rfc3339 = Just "%H:%M:SZ"

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
    mkAtomPost p = p { date = formatDate $ date p }

extractUniqueTags :: [Post] -> Set Tag
extractUniqueTags posts' = Set.fromList [t | p <- posts', t <- tags p]

-- | Specific build rules for the Shake system
--   defines workflow to build the website
buildRules :: Action ()
buildRules = do
  allPosts <- buildPosts
  let allTags = extractUniqueTags allPosts
  buildIndex allPosts
  buildTaggedIndex allTags allPosts
  buildFeed allPosts
  buildAbout
  copyStaticFiles

main :: IO ()
main = do
  let shOpts = shakeOptions { shakeVerbosity = Chatty, shakeLintInside = ["\\"]}
  shakeArgsForward shOpts buildRules

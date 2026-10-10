---
title: How to set up your blog with Haskell, Slick and Netlify
author: Luc Tielen
postDate: Dec 21, 2020
tags:
  - haskell
  - meta
image: hello_blog.png
---

I wanted to setup a blog powered by Haskell and HTML/CSS and host it on
Netlify. Creating a static site is easy enough using _Slick_, but this has no
direct support for Netlify. In this first post I will show you how to fix that.

## Tech stack

I wanted my blog to be easy to setup, minimal and extensible. In the past
I've tried (and failed) setting up a blog because I never managed to finish
the initial setup due to a complicated setup. After some googling around,
I came across the
[Slick static site generator](https://hackage.haskell.org/package/slick).
Slick is written in Haskell and uses Pandoc + the Shake build system to
compile all files into a static site. Slick assumes some Haskell knowledge
but with the documentation you can have a first version of your blog up and
running in a few minutes if you use the
[default project template](https://github.com/ChrisPenner/slick-template).

After I got the blog up and running, it was time to give the website
it's own look and feel. I started with a standalone HTML and CSS file and
played around with the styling. Once I was happy with how the site looked,
I replaced the templates and CSS that Slick provides by default with
my own. The code for this blog can be found on
[Github](https://github.com/luc-tielen/blog.git). The "site/" directory
contains all templates / CSS / markdown used to generate this blog.

Generating the blog is as simple as running the following command:

```bash
$ stack run
```

This will put the generated files in the output folder you configured in
"app/Main.hs".

## Deploying the site

Now that I had the blog up and running, I still needed to deploy it somewhere.
For this, I chose Netlify since it is often used for hosting static sites.
Unfortunately though, Netlify has no direct support for Slick! To solve this,
I wrote a small bash script that builds the blog and commits all necessary files from the blog into a separate orphan branch named "deploy":

```bash
#!/bin/bash

# Run this script locally to trigger a build and deploy.
# It assumes the "deploy" branch already exists
# (create with "git checkout --orphan deploy")

set -e

echo "Generating blog..."
stack run
tar czf build.tar.gz build/

echo "Updating blog content..."
git checkout deploy
rm -rf build/
tar xzf build.tar.gz
rm build.tar.gz

echo "Deploying blog..."
git add build/
git commit -m 'Deploy new version'
git push

git checkout -

exit 0
```

On the Netlify side, I configured it to trigger a deploy of the blog if a
push was made to the "deploy" branch.

Last step was buying the domain name and linking it to my netlify app.
I would suggest to buy the domain name via Netlify as well, for the
easiest setup. For more information, see the
[official Netlify docs](https://docs.netlify.com/domains-https/custom-domains/#assign-a-domain-to-a-site).

## What's next?

Phew, that was the first blogpost! If you followed along, you should now
have your own blog setup! All that's left is to write some blogposts. :smile:

So what can you expect on this blog in the future? Most content will be related to functional programming, compilers and logic, with maybe
occassionally some other (non-)tech things. You can get updates
on this blog if you follow me on [Twitter](https://twitter.com/luctielen) or if
you subscribe to my [RSS feed](https://luctielen.com/atom.xml).

Stay tuned!


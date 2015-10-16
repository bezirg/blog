{-# LANGUAGE OverloadedStrings #-}
module Main where

import Hakyll
import Data.Monoid
import System.FilePath (takeFileName)

myConfiguration :: Configuration
myConfiguration = defaultConfiguration

main :: IO ()
main = hakyllWith myConfiguration $ do
    -- Compress CSS
    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "images/*" $ do
      route   idRoute
      compile copyFileCompiler

    tags <- buildTags "posts/*" (fromCapture "tags/*.html")

    -- Render each and every post
    match "posts/*.md" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
                >>= saveSnapshot "content"
                >>= return . fmap demoteHeaders
                >>= loadAndApplyTemplate "templates/post.html" (tagsCtx tags)
                >>= loadAndApplyTemplate "templates/default.html" defaultContext
                >>= relativizeUrls

    -- Render posts list
    create ["posts.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let ctx = constField "title" "Posts" <>
                        listField "posts" (tagsCtx tags) (return posts) <>
                        defaultContext
            makeItem ""
                >>= loadAndApplyTemplate "templates/posts.html" ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls

    -- Post tags
    tagsRules tags $ \tag pattern -> do
        let title = "Posts tagged " ++ tag

        -- Copied from posts, need to refactor
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll pattern
            let ctx = constField "title" title <>
                        listField "posts" (tagsCtx tags) (return posts) <>
                        defaultContext
            makeItem ""
                >>= loadAndApplyTemplate "templates/posts.html" ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls


    -- Index
    match "pages/index.html" $ do
        route $ customRoute (takeFileName . toFilePath)
        compile $ do
            posts <- fmap (take 5) . recentFirst =<< loadAll "posts/*"
            let indexContext =
                    listField "posts" (tagsCtx tags) (return posts) <>
                    field "tagcloud" (\_ -> renderTagCloud 100 200 tags) <>
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexContext
                >>= loadAndApplyTemplate "templates/default.html" indexContext
                >>= relativizeUrls

    match "pages/404.html" $ do
        route $ customRoute (takeFileName . toFilePath)
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls
    match "pages/about.html" $ do
            route $ customRoute (takeFileName . toFilePath)
            compile $ pandocCompiler
                        >>= loadAndApplyTemplate "templates/default.html" defaultContext
                        >>= relativizeUrls
    match "pages/archlinux.md" $ do
        route $ customRoute (takeFileName . toFilePath) `composeRoutes` setExtension "html"
        compile $ pandocCompiler
                >>= loadAndApplyTemplate "templates/default.html" defaultContext
                >>= relativizeUrls

    match "pages/software.md" $ do
        route $ customRoute (takeFileName . toFilePath) `composeRoutes` setExtension "html"
        compile $ pandocCompiler
                >>= loadAndApplyTemplate "templates/default.html" defaultContext
                >>= relativizeUrls

    -- Render RSS feed
    create ["rss.xml"] $ do
        route idRoute
        compile $ do
            loadAllSnapshots "posts/*" "content"
                >>= fmap (take 10) . recentFirst
                >>= renderRss feedConfiguration feedCtx


    -- Read templates
    match "templates/*" $ compile templateCompiler

feedConfiguration :: FeedConfiguration
feedConfiguration = FeedConfiguration
    { feedTitle       = "Bezirg's Blog"
    , feedDescription = "Bezirg's Blog"
    , feedAuthorName  = "Nikolaos Bezirgiannis"
    , feedAuthorEmail = "bezirg@gmail.com"
    , feedRoot        = "http://blog.bezirg.net"
    }



postCtx :: Context String
postCtx = mconcat
    [ modificationTimeField "mtime" "%U"
    , dateField "date" "%B %e, %Y"
    , defaultContext
    ]


--------------------------------------------------------------------------------
tagsCtx :: Tags -> Context String
tagsCtx tags = mconcat
    [ modificationTimeField "mtime" "%U"
    , dateField "date" "%B %e, %Y"
    , tagsField "prettytags" tags
    , defaultContext
    ]


--------------------------------------------------------------------------------
feedCtx :: Context String
feedCtx = mconcat
    [ bodyField "description"
    , defaultContext
    ]


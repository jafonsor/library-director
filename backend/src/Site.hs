{-# LANGUAGE OverloadedStrings #-}

module Site
  ( routes,
  )
where

import Control.Monad.IO.Class (MonadIO)
import Data.Aeson (encode, object, (.=))
import Data.ByteString (ByteString)
import Data.Text (Text)
import DevReload (devReloadScript, pingHandler)
import Lucid
import Snap.Core (Method (..), MonadSnap, ifTop, method, modifyResponse, setContentType, setResponseStatus, writeLBS)

------------------------------------------------------------------------------

-- | Application routes
--
-- These are polymorphic over any MonadSnap, so they work with both
-- plain Snap () and Handler App App ()
routes :: (MonadSnap m, MonadIO m) => [(ByteString, m ())]
routes =
  [ ("/", ifTop indexHandler),
    ("/api/health", method GET healthHandler),
    ("/api/books", method GET booksHandler),
    ("/api/books", method POST createBookHandler),
    ("/dev/ping", method GET pingHandler) -- dev live-reload
  ]

------------------------------------------------------------------------------

-- | Index page HTML using lucid2
indexPage :: Html ()
indexPage = do
  doctype_
  html_ $ do
    head_ $ do
      meta_ [charset_ "utf-8"]
      title_ "Library Director"
    body_ $ do
      h1_ "📚 Library Director"
      p_ "Welcome to the Library Director API server"
      h2_ "Available Endpoints:"
      ul_ $ do
        endpoint_ "GET" "/api/health" "Health check API endpoint"
        endpoint_ "GET" "/api/books" "List all books"
        endpoint_ "POST" "/api/books" "Add a new book"
      -- Dev live-reload (remove for production)
      devReloadScript

-- | Helper to render an endpoint list item
endpoint_ :: Text -> Text -> Text -> Html ()
endpoint_ method path desc = li_ $ do
  code_ $ toHtml $ method <> " " <> path
  " - "
  toHtml desc

------------------------------------------------------------------------------

-- | Index handler - serves the root path
indexHandler :: (MonadSnap m) => m ()
indexHandler = do
  modifyResponse $ setContentType "text/html; charset=utf-8"
  writeLBS $ renderBS indexPage

------------------------------------------------------------------------------

-- | Health check endpoint
healthHandler :: (MonadSnap m) => m ()
healthHandler = do
  modifyResponse $ setContentType "application/json"
  writeLBS $
    encode $
      object
        [ "status" .= ("ok" :: String),
          "service" .= ("library-director" :: String)
        ]

------------------------------------------------------------------------------

-- | Get all books (placeholder)
booksHandler :: (MonadSnap m) => m ()
booksHandler = do
  modifyResponse $ setContentType "application/json"
  writeLBS $
    encode $
      object
        [ "books" .= ([] :: [String]),
          "message" .= ("No books in the library yet" :: String)
        ]

------------------------------------------------------------------------------

-- | Create a new book (placeholder)
createBookHandler :: (MonadSnap m) => m ()
createBookHandler = do
  modifyResponse $ setContentType "application/json"
  modifyResponse $ setResponseStatus 201 "Created"
  writeLBS $
    encode $
      object
        [ "message" .= ("Book creation endpoint - implement me!" :: String)
        ]

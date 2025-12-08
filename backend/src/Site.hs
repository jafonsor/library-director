{-# LANGUAGE OverloadedStrings #-}

module Site
  ( routes
  ) where

import Data.Aeson (object, (.=), encode)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Snap.Core
  ( MonadSnap
  , Method(..)
  , method
  , writeBS
  , writeLBS
  , modifyResponse
  , setContentType
  , setResponseStatus
  , ifTop
  )

------------------------------------------------------------------------------
-- | Application routes
--
-- These are polymorphic over any MonadSnap, so they work with both
-- plain Snap () and Handler App App ()
routes :: MonadSnap m => [(ByteString, m ())]
routes =
  [ ("/"          , ifTop indexHandler)
  , ("/api/health", method GET healthHandler)
  , ("/api/books" , method GET booksHandler)
  , ("/api/books" , method POST createBookHandler)
  ]

------------------------------------------------------------------------------
-- | Index handler - serves the root path
indexHandler :: MonadSnap m => m ()
indexHandler = do
  modifyResponse $ setContentType "text/html; charset=utf-8"
  writeBS $ BS.unlines
    [ "<!DOCTYPE html>"
    , "<html>"
    , "<head><title>Library Director</title></head>"
    , "<body>"
    , "<h1>📚 Library Director</h1>"
    , "<p>Welcome to the Library Director API server.</p>"
    , "<h2>Available Endpoints:</h2>"
    , "<ul>"
    , "<li><code>GET /api/health</code> - Health check</li>"
    , "<li><code>GET /api/books</code> - List all books</li>"
    , "<li><code>POST /api/books</code> - Add a new book</li>"
    , "</ul>"
    , "</body>"
    , "</html>"
    ]

------------------------------------------------------------------------------
-- | Health check endpoint
healthHandler :: MonadSnap m => m ()
healthHandler = do
  modifyResponse $ setContentType "application/json"
  writeLBS $ encode $ object
    [ "status" .= ("ok" :: String)
    , "service" .= ("library-director" :: String)
    ]

------------------------------------------------------------------------------
-- | Get all books (placeholder)
booksHandler :: MonadSnap m => m ()
booksHandler = do
  modifyResponse $ setContentType "application/json"
  writeLBS $ encode $ object
    [ "books" .= ([] :: [String])
    , "message" .= ("No books in the library yet" :: String)
    ]

------------------------------------------------------------------------------
-- | Create a new book (placeholder)
createBookHandler :: MonadSnap m => m ()
createBookHandler = do
  modifyResponse $ setContentType "application/json"
  modifyResponse $ setResponseStatus 201 "Created"
  writeLBS $ encode $ object
    [ "message" .= ("Book creation endpoint - implement me!" :: String)
    ]

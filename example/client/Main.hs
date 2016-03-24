{-# language OverloadedStrings #-}
module Main where

import JavaScript.Web.MessageEvent
import JavaScript.Web.WebSocket
import GHCJS.Types

foreign import javascript unsafe  "console[\"log\"]($1)" consoleLog :: JSString -> IO ()

logMessage :: MessageEvent -> IO ()
logMessage messageEvent =
  case getData messageEvent of
    (StringData str) -> consoleLog str

main :: IO ()
main =
  do let request = WebSocketRequest
           { url = "ws://localhost:8000/echo"
           , protocols = []
           , onClose   = Nothing
           , onMessage = Just logMessage
           }
     ws <- connect request
     send "This is a test." ws
     pure ()

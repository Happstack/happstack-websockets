module Main where

import Control.Monad (msum, forever)
import Control.Monad.Trans (liftIO)
import Data.Text (Text)
import qualified Data.Text.IO as T
import Happstack.Server
import Happstack.Server.WebSockets (runWebSocketsHappstack)
import Network.WebSockets (ServerApp, acceptRequest, receiveData, sendTextData)

echoApp :: ServerApp
echoApp pendingConnection =
  do liftIO $ putStrLn "accepting connection..."
     conn <- acceptRequest pendingConnection
     liftIO $ putStrLn "accepted."
     forever $ do t <- receiveData conn :: IO Text
                  liftIO $ T.putStrLn t
                  sendTextData conn t

echoServerPart :: ServerPart Response
echoServerPart =
  do msum [ nullDir >> serveFile (asContentType "text/html") "../client/Main.jsexe/index.html"
          , dir "rts.js" $  serveFile (asContentType "text/javascript") "../client/Main.jsexe/rts.js"
          , dir "lib.js" $  serveFile (asContentType "text/javascript") "../client/Main.jsexe/lib.js"
          , dir "out.js" $  serveFile (asContentType "text/javascript") "../client/Main.jsexe/out.js"
          , dir "runmain.js" $  serveFile (asContentType "text/javascript") "../client/Main.jsexe/runmain.js"
--          , dir "echo.js" $ serveFile (asContentType "text/javascript") "echo.js"
          , dir "echo" $ do liftIO $ putStrLn "echo route"
                            runWebSocketsHappstack echoApp
                            ok $ toResponse ()
          ]

main :: IO ()
main =
  simpleHTTP nullConf echoServerPart


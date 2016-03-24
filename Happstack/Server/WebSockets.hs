module Happstack.Server.WebSockets where

import Control.Concurrent                      (forkIO, threadDelay)
import Control.Exception                       (SomeException, finally, handle)
import Control.Monad                           (forever)
import Control.Monad.Trans                     (MonadIO)
import qualified Data.ByteString.Char8         as BS
import Data.CaseInsensitive                    (mk)
import qualified Data.Map                      as Map
import Happstack.Server                        (ServerMonad, Headers, askRq)
import Happstack.Server.Internal.Monads        (escapeHTTP)
import Happstack.Server.Internal.TimeoutIO     (TimeoutIO(..))
import Happstack.Server.Internal.Types         (EscapeHTTP(..), Request(..), HeaderPair(..))
-- import Network.WebSockets                      (WebSockets(..))
import qualified Network.WebSockets            as WS
import qualified Network.WebSockets.Connection as WS
import qualified Network.WebSockets.Stream     as WS

runWebSocketsHappstackWith
  :: (ServerMonad m, MonadIO m) =>
     WS.ConnectionOptions
  -> WS.ServerApp
  -> m ()
runWebSocketsHappstackWith options app =
  do req <- askRq
     escapeHTTP $ \timeoutIO -> do
       putStrLn "calling makeStream"
       stream <- WS.makeStream (toGet timeoutIO) (\ms -> case ms of Nothing -> pure () ; (Just s) -> (toPutLazy timeoutIO s))
       putStrLn "have stream"
       let pc = WS.PendingConnection
                  { WS.pendingOptions  = options
                  , WS.pendingRequest  = mkRequestHead req
                  , WS.pendingOnAccept = forkPingThread
                  , WS.pendingStream   = stream
                  }
       app pc `finally` (WS.close stream)
         where

           mkHeaders :: Headers -> WS.Headers
           mkHeaders headers = foldr (\(HeaderPair k vs) l -> [ (mk k,v) | v <- vs ] ++ l) [] (Map.elems headers)

           mkRequestHead :: Request -> WS.RequestHead
           mkRequestHead req =
             WS.RequestHead { WS.requestPath    = BS.pack (rqUri req)
                            , WS.requestHeaders = mkHeaders (rqHeaders req)
                            , WS.requestSecure  = rqSecure req
                            }

forkPingThread :: WS.Connection -> IO ()
forkPingThread connection =
  do _ <- forkIO pingThread
     pure ()
       where
         pingThread :: IO ()
         pingThread =
           handle ignoreAll $ forever $
              do WS.sendPing connection (BS.pack "ping")
                 threadDelay (25 * 10^6) -- every 25 seconds, default timeout for TimeoutIO is 30 seconds
                                         -- FIXME: shouldbe configurable in case TimeoutIO is using a time less than 25 seconds

         ignoreAll :: SomeException -> IO ()
         ignoreAll _ = pure ()

runWebSocketsHappstack :: (ServerMonad m, MonadIO m) =>
                          WS.ServerApp
                       -> m ()
runWebSocketsHappstack =
  runWebSocketsHappstackWith WS.defaultConnectionOptions


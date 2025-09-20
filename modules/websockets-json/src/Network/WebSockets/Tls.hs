--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.
module Network.WebSockets.Tls where

import Prelude

import Control.Monad
    ( join
    )
import Data.ByteString
    ( ByteString
    )
import Data.List
    ( stripPrefix
    )
import Data.Maybe
    ( fromMaybe
    )
import           System.Timeout                (timeout)
import           Control.Exception             (bracket, throwIO)

import qualified Data.ByteString as BS
import qualified Network.Connection as Network
import qualified Network.Socket                as S
import qualified Network.WebSockets as WS
import qualified Wuss as WSS

-- needed until https://hackage.haskell.org/package/websockets can set keep-alive itself
runKeepAliveClient :: String -> Int -> String -> WS.ClientApp a -> IO a
runKeepAliveClient host port path app = do
  let
    opts = WS.defaultConnectionOptions
    hints    = S.defaultHints { S.addrSocketType = S.Stream }
    fullHost = if port == 80 then host else (host ++ ":" ++ show port)
  addr:_ <- S.getAddrInfo (Just hints) (Just host) (Just $ show port)
  sock   <- S.socket (S.addrFamily addr) S.Stream S.defaultProtocol
  S.setSocketOption sock S.NoDelay 1
  S.setSocketOption sock S.KeepAlive 1

  res <- bracket
    (timeout (WS.connectionTimeout opts * 1000 * 1000) $ S.connect sock (S.addrAddress addr))
    (const $ S.close sock) $ \maybeConnected -> case maybeConnected of
      Nothing -> throwIO $ WS.ConnectionTimeout
      Just () -> WS.runClientWithSocket sock fullHost path opts [] app

  return res

-- | A drop-in replacement for 'WS.runClient' but that also handles TLS connections.
runClient
    :: String
        -- ^ Protocol + host
    -> Int
        -- ^ Port
    -> WS.ClientApp a
        -- ^ Client application to run
    -> IO a
runClient url port =
    case stripPrefix "wss://" url of
        Just host ->
            let
                options = WS.defaultConnectionOptions
                config  = WSS.defaultConfig { WSS.connectionGet = connectionGet }
             in
               WSS.runSecureClientWithConfig host (fromIntegral port) "/" config options []
        _ ->
            let
                host = fromMaybe url (stripPrefix "ws://" url)
             in
                runKeepAliveClient host port "/"
  where
    connectionGet
        :: Network.Connection
        -> IO ByteString
    connectionGet conn =
        more id
      where
        more !dl = getChunkRepeatedly
            (\s -> more (dl . (s:)))
            (\s -> done (dl . (s:)))
            (done dl)

        done dl = return $! BS.concat $ dl []

        getChunkRepeatedly
            :: (ByteString -> IO r) -- moreK: need more input
            -> (ByteString -> IO r) -- doneK: end of line (line terminator found)
            -> IO r                 -- eofK:  end of file
            -> IO r
        getChunkRepeatedly moreK doneK _eofK =
            join $ Network.connectionGetChunk' conn $ \s ->
                if BS.null s then (moreK s, BS.empty)
                else (doneK s, BS.empty)

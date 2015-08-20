{-# LANGUAGE OverloadedStrings
           , OverloadedLists #-}

import Conduit
import Control.Applicative
import Control.Concurrent.Async (concurrently)
import Control.Concurrent hiding (yield)
import Control.Monad
import Data.Conduit
import qualified Data.Conduit.List as CL
import Data.Conduit.Network
import Data.Maybe
import Data.MessagePack as M
import Data.Text
import Network (withSocketsDo)
import Network.Socket
import Options.Applicative
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
import qualified Data.Conduit.Network.UDP as UDP
import qualified Data.Streaming.Network as Net
import System.IO (stdin)

import Network.NTCE.Types

data Settings = Settings { sPort    :: Int
                         , sPortUDP :: Int
                         }

tcpPort :: Parser Int
tcpPort = option auto $   long "listen"
                       <> short 'l'
                       <> metavar "TCP_PORT"
                       <> help "TCP port to listen"

udpPort :: Parser Int
udpPort = option auto $   long "udp-port"
                       <> short 'u'
                       <> metavar "UDP_PORT"
                       <> help "UDP port to listen"

settings :: Parser Settings
settings = Settings <$> tcpPort <*> udpPort

main = execParser opts >>= \opts -> do
  withSocketsDo $ do
    sock <- Net.bindPortUDP (sPortUDP opts) "*"
    forever $ do
      sourceSocket sock $$ do
        s <- fmap BSL.fromStrict <$> await
        let msg = maybe [] (fromMaybe [] . M.unpack) s :: [FlowRecord]
        liftIO $ print msg
--         let msg = liftM (M.unpack . BSL.fromStrict) s :: Maybe [FlowRecord]
--         maybe (return ()) (\s -> liftIO $ putStrLn (show (BS.length s))) s

  where
    opts = info (helper <*> settings)
                (fullDesc  <> progDesc "ntced sink proxy"
                           <> header "ntced-sink-tcp" )

--     message addr s = Net.Message (toBS s) (addrAddress addr)

--     toBS = BSL.toStrict . M.pack

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
import qualified Data.Conduit.Network.UDP as UDP
import qualified Data.Streaming.Network as Net
import System.IO (stdin)

import Network.NTCE.Types

data Settings = Settings { sHost :: String
                         , sPort :: Int
                         }

udpPort :: Parser Int
udpPort = option auto $   long "port"
                       <> short 'p'
                       <> metavar "PORT"
                       <> help "port"

settings :: Parser Settings
settings = Settings <$> strOption (  long "host"
                                  <> short 'H'
                                  <> metavar "HOST"
                                  <> help "host" )
                    <*> udpPort

main = execParser opts >>= \opts -> do
  msg <- M.unpack <$> BSL.hGetContents stdin :: IO (Maybe ([HostEntry],[FlowRecord]))

  let flows = maybe [] snd msg

  withSocketsDo $ do
    (sock, addr) <- Net.getSocketUDP (sHost opts) (sPort opts)
    CL.sourceList flows $= CL.map (message addr) $$ UDP.sinkToSocket sock

  where
    opts = info (helper <*> settings)
                (fullDesc  <> progDesc "sends flow data to the given host:port by UDP"
                           <> header "ntced-sink-udp" )

    message addr s = Net.Message (toBS s) (addrAddress addr)

    toBS = BSL.toStrict . M.pack . (:[])

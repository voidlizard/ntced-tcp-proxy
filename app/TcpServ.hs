{-# LANGUAGE OverloadedStrings
           , OverloadedLists #-}

import Conduit
import Control.Applicative
import Control.Concurrent.Async
import Control.Concurrent hiding (yield)
import Control.Exception (finally)
import Control.Monad
import Data.Conduit
import Data.Conduit.Network
import Data.Maybe
import Data.Text
import Network.Socket
import Network (withSocketsDo)
import Options.Applicative
import System.IO (stdin)

import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import qualified Data.STM.RollingQueue as R

import qualified Data.Map as M
import qualified Data.MessagePack as MP
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Network.UDP as UDP
import qualified Data.Streaming.Network as Net

import Network.NTCE.Types
import qualified Network.NTCE.FlowRecord.Pretty as FR

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

  clients <- newTVarIO 1 :: IO (TVar Int)
  queues  <- newTVarIO (M.empty) :: IO (TVar (M.Map Int (R.RollingQueue FlowRecord)))

  withSocketsDo $ do
    sock <- Net.bindPortUDP (sPortUDP opts) "*"
    forkIO $ forever $ do
      sourceSocket sock $$ do
        s <- fmap BSL.fromStrict <$> await
        let msg = maybe [] (fromMaybe [] . MP.unpack) s :: [FlowRecord]

        liftIO $ atomically $ do
          cls  <- readTVar queues
          cls' <- forM (M.toList cls) $ \(k,q) -> do
                    mapM_ (R.write q) msg
                    return (k,q)
          writeTVar queues (M.fromList cls')

        return ()

    runTCPServer (serverSettings (sPort opts) "*") $ \app -> do

      rq <- R.newIO 1000 :: IO (R.RollingQueue FlowRecord)
      clId <- atomically $ do n <- readTVar clients
                              modifyTVar' clients succ
                              return n

      let run = do

            atomically $ modifyTVar' queues (M.insert clId rq)

            forever $ do

              r <- atomically $ R.read rq
              ((yield (FR.renderBS (fst r)) >> yield "\n") $$ appSink app)


      void $ forkIO (finally run (atomically $ modifyTVar' queues (M.delete clId)))

      forever $ do
        threadDelay 5000000
        size <- M.size <$> atomically (readTVar queues)
        putStrLn $ "clients: " ++ show size

  where
    opts = info (helper <*> settings)
                (fullDesc  <> progDesc "ntced sink proxy"
                           <> header "ntced-sink-tcp" )

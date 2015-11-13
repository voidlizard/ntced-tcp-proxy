{-# LANGUAGE OverloadedStrings
           , OverloadedLists #-}
module Network.NTCE.Types (HostEntry(..), FlowRecord(..)) where

import Control.Applicative
import Data.MessagePack
import Data.Text

data HostEntry = HostEntry { hMac    :: Text
                           , hClass  :: Text
                           , hDevice :: Text
                           } deriving (Show)

data FlowRecord = FlowRecord { fProto   :: Text
                             , fMacA    :: Text
                             , fIpA     :: Text
                             , fPortA   :: Int
                             , fMacB    :: Text
                             , fIpB     :: Text
                             , fPortB   :: Int
                             , fExpired :: Bool
                             , fPkts    :: Int
                             , fBytes   :: Int
                             , fCat     :: [Text]
                             } deriving (Show)


instance MessagePack HostEntry where
  toObject e = ObjectArray [ toObject (hMac e)
                           , toObject (hClass e)
                           , toObject (hDevice e)
                           ]

  fromObject (ObjectArray [a,b,c]) = host
    where host = pure HostEntry <*> fromObject a
                                <*> fromObject b
                                <*> fromObject c

instance MessagePack FlowRecord where
  toObject e = ObjectArray [ toObject $ fProto e
                           , toObject $ fMacA  e
                           , toObject $ fIpA e
                           , toObject $ fPortA e
                           , toObject $ fMacB  e
                           , toObject $ fIpB e
                           , toObject $ fPortB e
                           , toObject $ fExpired e
                           , toObject $ fPkts e
                           , toObject $ fBytes e
                           , toObject $ fCat e
                           ]

  fromObject (ObjectArray [prot, m1, i1, p1, m2, i2, p2, e, p, b, cats]) = flow
    where flow = pure FlowRecord <*> fromObject prot
                                 <*> fromObject m1
                                 <*> fromObject i1
                                 <*> fromObject p1
                                 <*> fromObject m2
                                 <*> fromObject i2
                                 <*> fromObject p2
                                 <*> fromObject e
                                 <*> fromObject p
                                 <*> fromObject b
                                 <*> fromObject cats


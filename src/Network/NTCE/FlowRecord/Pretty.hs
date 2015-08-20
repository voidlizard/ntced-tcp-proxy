{-# LANGUAGE OverloadedStrings
           , OverloadedLists #-}
module Network.NTCE.FlowRecord.Pretty (renderText, renderBS) where

import Text.PrettyPrint.Leijen.Text
import Data.Text.Encoding
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Lazy (fromChunks, toChunks)
import Data.ByteString (ByteString)

import Network.NTCE.Types

mkDoc :: FlowRecord -> Doc
mkDoc r = entry

  where expired = if fExpired r
                    then text "E"
                    else text "N"

        txt = text . fromChunks . (:[])

        entry = cat $ punctuate (text "|") [ expired
                                     , txt (fProto r)
                                     , txt (fIpA r)
                                     , int (fPortA r)
                                     , txt (fIpB r)
                                     , int (fPortB r)
                                     , cat $ punctuate (text ",") (map txt (fCat r))
                                     ]

renderText :: FlowRecord -> Text
renderText r = T.concat $ toChunks $ displayT $ renderOneLine (mkDoc r)

renderBS :: FlowRecord -> ByteString
renderBS = encodeUtf8 . renderText


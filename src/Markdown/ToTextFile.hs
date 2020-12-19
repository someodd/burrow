-- | Markdown to a text file intended for gopherspace.
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE OverloadedStrings          #-}
module Markdown.ToTextFile where

import qualified Data.Map as Map
import Text.Mustache
import Commonmark hiding (addAttribute, escapeURI)
import           Commonmark.Types
import           Commonmark.Entity (lookupEntity)
import           Data.Text.IO as TIO
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import           Data.Text.Lazy.Builder (Builder, fromText, toLazyText,
                                         singleton)
import           Data.Text.Encoding   (encodeUtf8)
import qualified Data.ByteString.Char8 as B
import           Text.Printf          (printf)
import           Data.Char            (ord, isAlphaNum, isAscii, isSpace)
import           Data.Maybe           (fromMaybe)

import TextUtils.Headings
import Markdown.Common


-- | A text file intended to be viewed in gopherspace.
data GopherFile = GopherFile Text | GopherFileNull


instance HasAttributes GopherFile where
  addAttributes attrs x = x


instance Show GopherFile where
  show (GopherFile t) = T.unpack t


instance Semigroup (GopherFile) where
  x <> GopherFileNull                = x
  GopherFileNull <> x                = x
  GopherFile t1 <> GopherFile t2   = GopherFile (t1 <> t2)


instance Monoid (GopherFile) where
  mempty = GopherFileNull
  mappend = (<>)


instance Rangeable (GopherFile) where
  ranged _ x = x


instance Rangeable (GopherFile) => IsInline (GopherFile) where
  lineBreak = GopherFile "\n"
  softBreak = GopherFile "\n"
  str t = GopherFile t
  entity t = GopherFile t
  escapedChar c = GopherFile (T.pack $ c:[])
  emph ils = ils
  strong ils = ils
  link target title ils = ils
  image target title ils = ils
  code t = GopherFile t
  rawInline f t = GopherFile t


instance IsInline (GopherFile) => IsBlock (GopherFile) (GopherFile) where
  paragraph (GopherFile ils) = GopherFile $ parseParagraph ils
  plain ils = ils
  thematicBreak = GopherFile "------------------"
  blockQuote bs = bs
  codeBlock info t = GopherFile t
  heading level (GopherFile ils) = GopherFile $ parseHeading level ils
  rawBlock f t = GopherFile t
  referenceLinkDefinition _ _ = GopherFile ""
  list (BulletList _) lSpacing items = GopherFile ""
  list (OrderedList startnum enumtype _delimtype) lSpacing items = GopherFile ""

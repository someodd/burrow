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

import Control.Monad.Reader
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
import Types


-- | A text file intended to be viewed in gopherspace.
data GopherFile = GopherFile Text | GopherFileNull


instance HasAttributes (ParseEnv GopherFile) where
  addAttributes attrs x = x


-- | DO NOT USE
instance Show (ParseEnv GopherFile) where
  show _ = "I *said*: DO NOT USE!"


instance Semigroup (GopherFile) where
  x <> GopherFileNull                = x
  GopherFileNull <> x                = x
  GopherFile t1 <> GopherFile t2   = GopherFile (t1 <> t2)


instance Semigroup (ParseEnv GopherFile) where
  x <> y = do
    x' <- x
    y' <- y
    pure $ x' <> y'


instance Monoid (ParseEnv GopherFile) where
  mempty = pure GopherFileNull
  mappend = (<>)


instance Rangeable (ParseEnv GopherFile) where
  ranged _ x = x


instance Rangeable (ParseEnv GopherFile) => IsInline (ParseEnv GopherFile) where
  lineBreak = pure $ GopherFile "\n"
  softBreak = pure $ GopherFile "\n"
  str t = pure $ GopherFile t
  entity t = pure $ GopherFile t
  escapedChar c = pure $ GopherFile (T.pack $ c:[])
  emph ils = ils
  strong ils = ils
  link target title ils = ils
  image target title ils = ils
  code t = pure $ GopherFile t
  rawInline f t = pure $ GopherFile t


-- type FontMap = Map.Map String AsciiFont

-- should be in common?
-- | Parse a Markdown heading into a fancy, beautiful ASCII art heading.
parseHeading' :: Int -> Map.Map String AsciiFont -> Text -> Text
parseHeading' level fonts ils = T.pack $ headingCompose font $ show ils
 where
  -- temporary for prototype
  theFont =
    case Map.lookup "h1" fonts of
      Nothing -> error "oh no"
      Just f -> f

  font =
    case level of
      1 -> theFont
      _ -> theFont

-- for ils
gopherFileOrNull penv func = do
  gopherThing <- penv
  case gopherThing of
    GopherFile ils -> func ils
    GopherFileNull -> pure GopherFileNull

instance IsInline (ParseEnv GopherFile) => IsBlock (ParseEnv GopherFile) (ParseEnv GopherFile) where
  paragraph penv = do
    gopherFileOrNull penv (\ils -> pure $ GopherFile $ parseParagraph ils)
  plain penv = penv
  thematicBreak = pure $ GopherFile "------------------"
  blockQuote penv = penv
  codeBlock info t = pure (GopherFile t)
  heading level penv = do
    gopherFileOrNull penv daFunc
   where
    daFunc ils = do
      fonts <- ask
      pure $ GopherFile $ parseHeading' level fonts ils
  rawBlock f t = pure $ GopherFile t
  referenceLinkDefinition _ _ = pure $ GopherFile ""
  list (BulletList _) lSpacing items = pure $ GopherFile ""
  list (OrderedList startnum enumtype _delimtype) lSpacing items = pure $ GopherFile ""

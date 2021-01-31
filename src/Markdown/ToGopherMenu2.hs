-- | Markdown to a text file intended for gopherspace.
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE OverloadedStrings          #-}
module Markdown.ToGopherMenu2 where

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
import Types


-- | Parse a Markdown paragraph into... well a regular paragraph.
parseParagraph :: Text -> Text
parseParagraph ils = "\n" <> ils <> "\n"


-- | Make a Gopher link according to the Gopher spec (RFC 1496 I think).
parseLinkToGopherMenuLink :: Text -> Text -> Text -> Text
parseLinkToGopherMenuLink target title ils = "\n" <> ((determineLinkType target) target title ils) <> "\n"
 where
  determineLinkType target
    | "https" `T.isPrefixOf` target = gopherHttpLink
    | ".txt" `T.isSuffixOf` target = gopherFileLink
    | otherwise = gopherMenuLink

  gopherHttpLink target title label = "h" <> label <> "\t" <> "URL:" <> target
  gopherFileLink target title label = "1" <> label <> "\t" <> target
  gopherMenuLink target title label = "0" <> label <> "\t" <> target


-- | A text file intended to be viewed in gopherspace.
data GopherMenu = GopherMenu Text | GopherMenuNull


instance HasAttributes (ParseEnv GopherMenu) where
  addAttributes attrs x = x


-- | DO NOT USE
instance Show (ParseEnv GopherMenu) where
  show _ = "I *said*: DO NOT USE!"


instance Semigroup (GopherMenu) where
  x <> GopherMenuNull                = x
  GopherMenuNull <> x                = x
  GopherMenu t1 <> GopherMenu t2   = GopherMenu (t1 <> t2)


instance Semigroup (ParseEnv GopherMenu) where
  x <> y = do
    x' <- x
    y' <- y
    pure $ x' <> y'


instance Monoid (ParseEnv GopherMenu) where
  mempty = pure GopherMenuNull
  mappend = (<>)


instance Rangeable (ParseEnv GopherMenu) where
  ranged _ x = x


instance Rangeable (ParseEnv GopherMenu) => IsInline (ParseEnv GopherMenu) where
  lineBreak = pure $ GopherMenu "\n"
  softBreak = pure $ GopherMenu "\n"
  str t = pure $ GopherMenu t
  entity t = pure $ GopherMenu t
  escapedChar c = pure $ GopherMenu (T.pack $ c:[])
  emph ils = ils
  strong ils = ils
  link target title penv = do
    gopher <- penv
    case gopher of
      (GopherMenu ils) -> 
        let linkText = parseLinkToGopherMenuLink target title ils
        in pure $ GopherMenu ("\n" <> linkText <> "\n")
      GopherMenuNull -> pure GopherMenuNull
  image target title ils = ils
  code t = pure $ GopherMenu t
  rawInline f t = pure $ GopherMenu t


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
    GopherMenu ils -> func ils
    GopherMenuNull -> pure GopherMenuNull

instance IsInline (ParseEnv GopherMenu) => IsBlock (ParseEnv GopherMenu) (ParseEnv GopherMenu) where
  paragraph penv = do
    gopherFileOrNull penv (\ils -> pure $ GopherMenu $ parseParagraph ils)
  plain penv = penv
  thematicBreak = pure $ GopherMenu "------------------"
  blockQuote penv = penv
  codeBlock info t = pure (GopherMenu t)
  heading level penv = do
    gopherFileOrNull penv daFunc
   where
    daFunc ils = do
      fonts <- ask
      pure $ GopherMenu $ parseHeading' level fonts ils
  rawBlock f t = pure $ GopherMenu t
  referenceLinkDefinition _ _ = pure $ GopherMenu ""
  list (BulletList _) lSpacing items = pure $ GopherMenu ""
  list (OrderedList startnum enumtype _delimtype) lSpacing items = pure $ GopherMenu ""

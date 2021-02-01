-- | Markdown to a file intended for gopherspace.
--
-- Has a lot of flexibility due to Environment/ParseEnv.
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE OverloadedStrings          #-}
module Markdown where

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


-- | Override certain inline instance methods for the parser.
data InlineOverrides = InlineOverrides { overrideLink :: Maybe (Text -> Text -> ParseEnv GopherFile -> ParseEnv GopherFile) }
blankInlineOverrides = InlineOverrides { overrideLink = Nothing }


-- | Stuff to make available to the parser.
data Environment =
  Environment
    { envFonts :: HeadingLevelFontMap
    , envInlineOverrides :: InlineOverrides
    }

  
-- | Make the ascii art font files available to a commonmark parser.
type ParseEnv a = Reader Environment a

-- | Parse a Markdown paragraph into... well a regular paragraph.
parseParagraph :: Text -> Text
parseParagraph ils = "\n" <> ils <> "\n"


-- | Make a Gopher link according to the Gopher spec (RFC 1496 I think). This is for
-- gopher menus, to be supplied as overrideLink.
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
link' target title penv = do
  gopher <- penv
  case gopher of
    (GopherFile ils) -> 
      let linkText = parseLinkToGopherMenuLink target title ils
      in pure $ GopherFile ("\n" <> linkText <> "\n")
    GopherFileNull -> pure GopherFileNull


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
  link target title ils = do
    env <- ask
    case overrideLink $ envInlineOverrides env of
      Just func -> func target title ils
      Nothing -> ils
  image target title ils = ils
  code t = pure $ GopherFile t
  rawInline f t = pure $ GopherFile t


-- type FontMap = Map.Map String AsciiFont

-- should be in common?
-- | Parse a Markdown heading into a fancy, beautiful ASCII art heading.
parseHeading' :: Int -> HeadingLevelFontMap -> Text -> Text
parseHeading' level fonts ils = T.pack $ headingCompose font $ show ils
 where
  font =
    case Map.lookup level fonts of
      Just x -> x
      Nothing -> error $ "No font for level: " ++ show level

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
      env <- ask
      let fonts = envFonts env
      pure $ GopherFile $ parseHeading' level fonts ils
  rawBlock f t = pure $ GopherFile t
  referenceLinkDefinition _ _ = pure $ GopherFile ""
  list (BulletList _) lSpacing items = pure $ GopherFile ""
  list (OrderedList startnum enumtype _delimtype) lSpacing items = pure $ GopherFile ""

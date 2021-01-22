-- | Markdown to a Gopher protocol menu.
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE OverloadedStrings          #-}
module Markdown.ToGopherMenu where

import Data.List (intercalate, intersperse)

import Data.List.Split
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


-- | Make a Gopher link according to the Gopher spec (RFC 1496 I think).
parseLinkToGopherMenuLink :: Text -> Text -> [GopherLine] -> Text
parseLinkToGopherMenuLink target title ils = "\n" <> ((determineLinkType target) target title ils') <> "\n"
 where
  determineLinkType target
    | "https" `T.isPrefixOf` target = gopherHttpLink
    | ".txt" `T.isSuffixOf` target = gopherFileLink
    | otherwise = gopherMenuLink

  -- "flatten" the ils which starts off as [GopherLine]
  ils' = T.pack $ intercalate "" $ fmap show ils

  gopherHttpLink target title label = "h" <> label <> "\t" <> "URL:" <> target
  gopherFileLink target title label = "1" <> label <> "\t" <> target
  gopherMenuLink target title label = "0" <> label <> "\t" <> target

-- GopherBlock should only be either GopherInfoLine GopherLinkLine

--data GopherMenu = GopherMenu Text | GopherMenuLink Text | GopherNull
--data GopherMenu = GopherMenu Text | GopherMenuParagraphHasLink Text | GopherMenuLink Text | GopherNull


instance Show GopherLine where
  show (GopherNewLine) = ""
  show (GopherCompleteInfoLine t) = T.unpack . T.filter (/= '\t') $ t
  show (GopherIncompleteInfoLine t) = T.unpack t
  show (GopherLinkLine t) = T.unpack t-- or something lol


instance Semigroup (GopherLine) where
  x <> GopherNullLine                = x
  GopherNullLine <> x                = x


instance Monoid (GopherLine) where
  mempty = GopherNullLine
  mappend = (<>)


instance HasAttributes GopherMenu where
  addAttributes attrs x = x


instance Show GopherMenu where
  --show (GopherBlock t) = intercalate "" (fmap (\x -> show x) t)
  show (GopherBlock t) = unlines $ fmap (show) $ groupByLines t


instance Semigroup (GopherMenu) where
  x <> GopherNull                = x
  GopherNull <> x                = x

  GopherBlock l1 <> GopherBlock l2 = GopherBlock (l1 ++ l2)


instance Monoid (GopherMenu) where
  mempty = GopherNull
  mappend = (<>)


instance Rangeable (GopherMenu) where
  ranged _ x = x


data GopherLine = GopherCompleteInfoLine Text | GopherIncompleteInfoLine Text | GopherLinkLine Text | GopherNewLine | GopherNullLine
data GopherMenu = GopherBlock [GopherLine] | GopherNull

-- | Groups lines by joining everything preceding \n into a "line"
--
-- You're supposed to intercalate "" the inner bits...
groupByLines :: [GopherLine] -> [GopherLine]
--groupByLines gopherLines = fmap (GopherCompleteInfoLine . T.pack . (intercalate "") . (fmap show)) $ splitWhen splitWhenNewLineOrLink gopherLines
groupByLines gopherLines = fmap (GopherCompleteInfoLine . T.pack . (intercalate "") . (fmap show)) $ mySplitWhen gopherLines
 where
  mySplitWhen = split (keepDelimsR $ whenElt splitWhenNewLineOrLink)

  splitWhenNewLineOrLink gopherLine =
    case gopherLine of
      GopherNewLine -> True
      GopherLinkLine _ -> True
      _ -> False

  {-
  
  -- | Joins text onto a single line separated by new lines
  -- | Joins the chunks of GopherText
  foldr (\new acc -> acc ++ [new]) [] gopherLines

  joinTheText' gopherLinesChunk = foldr joinFunc [] gopherLines
   where
    joinFunc someChunk 

  joinTheText gopherLinesChunk =
    case head gopherLinesChunk of
      (GopherText _) -> "i" <> (intercalate "" (fmap 
      _ -> gopherLinesChunk
  {-
  joinIfGopherText gopherLine =
    case gopherLine of
      (GopherText t) -> 
  -}
  -}


instance Rangeable (GopherMenu) => IsInline (GopherMenu) where
  lineBreak = GopherBlock [GopherNewLine]
  softBreak = GopherBlock [GopherNewLine]
  str t = GopherBlock [GopherIncompleteInfoLine t]
  entity t = GopherBlock [GopherIncompleteInfoLine t]
  escapedChar c = GopherBlock [GopherIncompleteInfoLine $ T.pack $ c:[]]
  emph ils = ils
  strong ils = ils
  link target title (GopherBlock ils) =
    let linkText = parseLinkToGopherMenuLink target title ils
    in GopherBlock [GopherLinkLine linkText]
  --link target title (GopherMenu ils) = error . show $ ils
  -- below never happens
  -- makeGopherLink target title ils = ...
  image target title ils = ils
  code t = GopherBlock [GopherIncompleteInfoLine t]
  rawInline f t = GopherBlock [GopherIncompleteInfoLine t]


parseBlock ils = error . show $ ils

instance IsInline (GopherMenu) => IsBlock (GopherMenu) (GopherMenu) where
  -- paragraph should not prefix with 'i' if it's a paragraph link...
  -- this is why even the link gets prefixed with 'i' when it shouldn't... how do you avoid this?
  paragraph (GopherBlock ils) = GopherBlock ([GopherNewLine] ++ ils ++ [GopherNewLine])
  plain (GopherBlock ils) = GopherBlock [GopherIncompleteInfoLine "~~~~"]
  thematicBreak = GopherBlock [GopherIncompleteInfoLine "------------------"]
  blockQuote bs = bs
  codeBlock info t = GopherBlock [GopherIncompleteInfoLine t]

  -- here below ils is a list of GopherLines
  heading level (GopherBlock ils) = GopherBlock $ [GopherNewLine] ++ formattedHeader ++ [GopherNewLine]
  --heading level (GopherBlock ils) = GopherBlock $ [GopherCompleteInfoLine "wowza"]
   where
    headerText = 
      let toLines (GopherIncompleteInfoLine t) = t
      in T.unlines $ map toLines ils
    header = T.lines $ parseHeading level headerText
    --(map (\x -> GopherCompleteInfoLine (x <> "\n")) ())
    formattedHeader = intersperse GopherNewLine $ map GopherIncompleteInfoLine header
    --toLines (GopherLinkLine t) = t

  rawBlock f t = GopherBlock [GopherIncompleteInfoLine t]
  referenceLinkDefinition _ _ = GopherBlock [GopherIncompleteInfoLine ""]
  list (BulletList _) lSpacing items = GopherBlock [GopherIncompleteInfoLine ""]
  list (OrderedList startnum enumtype _delimtype) lSpacing items = GopherBlock [GopherIncompleteInfoLine ""]

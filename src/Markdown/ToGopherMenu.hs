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
import Control.Monad.Reader
import qualified Data.ByteString.Char8 as B
import           Text.Printf          (printf)
import           Data.Char            (ord, isAlphaNum, isAscii, isSpace)
import           Data.Maybe           (fromMaybe)

import TextUtils.Headings
import Markdown.Common
import Types-- FIXME: should go in markdown common?


-- | Make a Gopher link according to the Gopher spec (RFC 1496 I think).
parseLinkToGopherMenuLink :: Text -> Text -> [GopherLine] -> Text
parseLinkToGopherMenuLink target title ils = "\n" <> ((determineLinkType target) target title ils') <> "\n"
 where
  determineLinkType target
    | "https" `T.isPrefixOf` target = gopherHttpLink
    | ".txt" `T.isSuffixOf` target = gopherFileLink
    | otherwise = gopherMenuLink

  -- "flatten" the ils which starts off as [GopherLine]
  ils' = T.intercalate "" $ fmap gopherLineToText ils

  gopherHttpLink target title label = "h" <> label <> "\t" <> "URL:" <> target
  gopherFileLink target title label = "1" <> label <> "\t" <> target
  gopherMenuLink target title label = "0" <> label <> "\t" <> target

-- GopherBlock should only be either GopherInfoLine GopherLinkLine

--data GopherMenu = GopherMenu Text | GopherMenuLink Text | GopherNull
--data GopherMenu = GopherMenu Text | GopherMenuParagraphHasLink Text | GopherMenuLink Text | GopherNull


gopherLineToText :: GopherLine -> Text
gopherLineToText (GopherNewLine) = ""
gopherLineToText (GopherCompleteInfoLine t) = T.filter (/= '\t') $ t
gopherLineToText (GopherIncompleteInfoLine t) = t
gopherLineToText (GopherLinkLine t) = t-- or something lol


-- | DO NOT USE
instance Show (ParseEnv GopherLine) where
  show _ = "I *said*: DO NOT USE!"


instance Semigroup (GopherLine) where
  x <> GopherNullLine                = x
  GopherNullLine <> x                = x


instance Semigroup (ParseEnv GopherLine) where
  x <> y = do
    x' <- x
    y' <- y
    pure $ x' <> y'


instance Monoid (ParseEnv GopherLine) where
  mempty = pure GopherNullLine
  mappend = (<>)


instance HasAttributes (ParseEnv GopherMenu) where
  addAttributes attrs x = x


gopherMenuToText :: GopherMenu -> Text
gopherMenuToText (GopherBlock t) = T.unlines $ fmap (gopherLineToText) $ groupByLines t
-- | DO NOT USE
instance Show (ParseEnv GopherMenu) where
  --show (GopherBlock t) = unlines $ fmap (show) $ groupByLines t
  show _ = "I *said*: DO NOT USE!"


instance Semigroup (GopherMenu) where
  x <> GopherNull                = x
  GopherNull <> x                = x

  GopherBlock l1 <> GopherBlock l2 = GopherBlock (l1 ++ l2)


instance Semigroup (ParseEnv GopherMenu) where
  x <> y = do
    x' <- x
    y' <- y
    pure $ x' <> y'


instance Monoid (ParseEnv GopherMenu) where
  mempty = pure GopherNull
  mappend = (<>)


instance Rangeable (ParseEnv GopherMenu) where
  ranged _ x = x


data GopherLine = GopherCompleteInfoLine Text | GopherIncompleteInfoLine Text | GopherLinkLine Text | GopherNewLine | GopherNullLine
data GopherMenu = GopherBlock [GopherLine] | GopherNull

-- | Groups lines by joining everything preceding \n into a "line"
--
-- You're supposed to intercalate "" the inner bits...
groupByLines :: [GopherLine] -> [GopherLine]
--groupByLines gopherLines = fmap (GopherCompleteInfoLine . T.pack . (intercalate "") . (fmap show)) $ splitWhen splitWhenNewLineOrLink gopherLines
groupByLines gopherLines = fmap (GopherCompleteInfoLine . (T.intercalate "") . (fmap gopherLineToText)) $ mySplitWhen gopherLines
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


instance Rangeable (ParseEnv GopherMenu) => IsInline (ParseEnv GopherMenu) where
  lineBreak = pure $ GopherBlock [GopherNewLine]
  softBreak = pure $ GopherBlock [GopherNewLine]
  str t = pure $ GopherBlock [GopherIncompleteInfoLine t]
  entity t = pure $ GopherBlock [GopherIncompleteInfoLine t]
  escapedChar c = pure $ GopherBlock [GopherIncompleteInfoLine $ T.pack $ c:[]]
  emph ils = ils
  strong ils = ils
  link target title penv = do
    gopher <- penv
    case gopher of
      (GopherBlock ils) -> 
        let linkText = parseLinkToGopherMenuLink target title ils
        in pure $ GopherBlock [GopherLinkLine linkText]
      GopherNull -> pure GopherNull
  --link target title (GopherMenu ils) = error . show $ ils
  -- below never happens
  -- makeGopherLink target title ils = ...
  image target title ils = ils
  code t = pure $ GopherBlock [GopherIncompleteInfoLine t]
  rawInline f t = pure $ GopherBlock [GopherIncompleteInfoLine t]


parseBlock ils = error . show $ ils


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


instance IsInline (ParseEnv GopherMenu) => IsBlock (ParseEnv GopherMenu) (ParseEnv GopherMenu) where
  -- paragraph should not prefix with 'i' if it's a paragraph link...
  -- this is why even the link gets prefixed with 'i' when it shouldn't... how do you avoid this?
  paragraph penv = do
    gopher <- penv
    case gopher of
      (GopherBlock ils) -> pure $ GopherBlock ([GopherNewLine] ++ ils ++ [GopherNewLine])
      GopherNull -> pure $ GopherNull
    
  plain _ = pure $ GopherBlock [GopherIncompleteInfoLine "~~~~"]
  thematicBreak = pure $ GopherBlock [GopherIncompleteInfoLine "------------------"]
  blockQuote penv = penv
  codeBlock info t = pure $ GopherBlock [GopherIncompleteInfoLine t]

  -- here below ils is a list of GopherLines
  heading level penv = do
    gopher <- penv
    case gopher of
      (GopherBlock ils) -> do
        fonts <- ask
        pure $ GopherBlock $ [GopherNewLine] ++ formattedHeader fonts ils ++ [GopherNewLine]
      GopherNull -> pure GopherNull
  --heading level (GopherBlock ils) = GopherBlock $ [GopherCompleteInfoLine "wowza"]
   where
    headerText ils =
      let toLines (GopherIncompleteInfoLine t) = t
      in T.pack $ intercalate "" $ map T.unpack $ map toLines ils
    header fonts ils = T.lines $ parseHeading' level fonts (headerText ils)
    --(map (\x -> GopherCompleteInfoLine (x <> "\n")) ())
    formattedHeader fonts ils = intersperse GopherNewLine $ map GopherIncompleteInfoLine (header fonts ils)
    --toLines (GopherLinkLine t) = t

  rawBlock f t = pure $ GopherBlock [GopherIncompleteInfoLine t]
  referenceLinkDefinition _ _ = pure $ GopherBlock [GopherIncompleteInfoLine ""]
  list (BulletList _) lSpacing items = pure $ GopherBlock [GopherIncompleteInfoLine ""]
  list (OrderedList startnum enumtype _delimtype) lSpacing items = pure $ GopherBlock [GopherIncompleteInfoLine ""]

-- | Markdown to a Gopher protocol menu.
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
module Markdown
  ( Environment(..)
  , gopherMenuToText
  , ParseEnv
  , GopherPage(..)
  ) where

import Data.Maybe (isJust)
import Data.Foldable (fold)
import Data.Char (toLower)
import Data.List (groupBy, intercalate, intersperse)
import Text.Numeral.Roman (toRoman)
import Commonmark hiding (addAttribute, escapeURI)
import           Data.Text (Text)
import qualified Data.Text as T
import Control.Monad.Reader
import qualified Data.Map as Map

import TextUtils.Headings
import TextUtils (italicize, embolden)


-- | Stuff to make available to the parser.
data Environment =
  Environment
    { envFonts :: HeadingLevelFontMap
    -- ^ Used to render headings.
    , envMenuLinks :: Maybe (Text, Text)
    -- ^ If nothing will assume a plain text file, won't do anything particular
    -- for links. If defined will use the host and port.
    , envPreserveLineBreaks :: Bool
    --, envInlineOverrides :: InlineOverrides
    }

  
-- | Make the ascii art font files available to a commonmark parser.
type ParseEnv a = Reader Environment a


-- | Parse a Markdown heading into a fancy, beautiful ASCII art heading.
parseHeading' :: Int -> HeadingLevelFontMap -> Text -> Text
parseHeading' level fonts ils = T.pack $ headingCompose font $ show ils
 where
  font =
    case Map.lookup level fonts of
      Just x -> x
      Nothing -> error $ "No font for level: " ++ show level


-- | Make a Gopher link according to the Gopher spec (RFC 1496 I think) for a
-- gophermap.
createGopherPageLink :: (Text, Text) -> Text -> Text -> Text -> Text
createGopherPageLink (host, port) target title label =
  determineLinkType
 where
  intercalateLink :: Text -> Text -> Maybe (Text, Text) -> Text
  intercalateLink itemType newTarget maybeHostAndPort =
    let title' = if T.null title then "" else " (" <> title <> ")"
        addHostPort = if isJust maybeHostAndPort then (++ ["\t", host, "\t", port]) else id
    in T.intercalate "" $ addHostPort [itemType, label <> title' :: Text, "\t", newTarget]

  -- FIXME: rename or refactor!
  -- FIXME: should use leading indicators too like /0/ or /1/
  determineLinkType :: Text
  determineLinkType
    -- Link to an HTTP web page.
    | "https" `T.isPrefixOf` target = intercalateLink "h" ("URL:" <> target) Nothing
    -- Link to a plaintext file in gopherspace.
    | ".txt" `T.isSuffixOf` target = intercalateLink "1" target (Just (host, port))
    -- Anything else is assumed to be a link to another gophermap/menu.
    | otherwise = intercalateLink "0" target (Just (host, port))


-- | DO NOT USE
instance Show (ParseEnv GopherLine) where
  show _ = "I *said*: DO NOT USE!"


instance Semigroup (GopherLine) where
  x <> NullLine = x
  NullLine <> x = x
  InfoLineToken x <> InfoLineToken y = InfoLineToken $ x <> y
  -- FIXME
  _ <> _ = error "what"


instance Monoid GopherLine where
  mempty = NullLine


instance Semigroup (ParseEnv GopherLine) where
  x <> y = do
    x' <- x
    y' <- y
    pure $ x' <> y'


instance Monoid (ParseEnv GopherLine) where
  mempty = pure NullLine
  mappend = (<>)


instance HasAttributes (ParseEnv GopherPage) where
  addAttributes _ x = x


-- FIXME: could even take the environment instead of that bool to simplify usage/less moving parts/less to go wrong.
-- | Convert the representation created by the `commonmark` parser into `Text`.
gopherMenuToText :: Environment -> GopherPage -> Text
gopherMenuToText _ NullBlock = ""
gopherMenuToText environment (Block t) =
  blankLineReplacements $ T.intercalate "" $ map gopherLineToText $ reduceNewLines . linkSpacing $ filter (/= NullLine) $ map joinTokens $ groupIncompleteLines t
 where
  menuMagic = isJust $ envMenuLinks environment

  blankLineReplacements = if menuMagic then T.replace "\n\n" "\ni \n" else id

  predicate (InfoLineToken _) (InfoLineToken _) = True
  predicate _ _ = False
  groupIncompleteLines :: [GopherLine] -> [[GopherLine]]
  groupIncompleteLines gopherLines =
    groupBy predicate gopherLines

  joinTokens :: [GopherLine] -> GopherLine
  joinTokens [InfoLineToken l] = CompleteInfoLine l
  joinTokens incompleteLines@(InfoLineToken _:_) =
    let (InfoLineToken token) = (fold incompleteLines :: GopherLine) in CompleteInfoLine token
  joinTokens [x] = x
  joinTokens _ = error "should be impossible! FIXME!" -- FIXME

  -- | Remove newlines by adding a \n to the previous line if exists.
  reduceNewLines :: [GopherLine] -> [GopherLine]
  reduceNewLines gopherLines =
    foldl foo [] gopherLines
   where
    foo acc@(_:_) GopherNewLine =
      case last acc of
        GopherNewLine -> init acc ++ [CompleteInfoLine $ if menuMagic then "\ni \n" else "\n\n"]
        (CompleteInfoLine i) -> init acc ++ [CompleteInfoLine $ i <> "\n"]
        (LinkLine link') -> init acc ++ [LinkLine $ link' <> "\n"]
        a -> acc ++ [a]
    foo acc n = acc ++ [n]

  linkSpacing :: [GopherLine] -> [GopherLine]
  linkSpacing gopherLines =
    foldl foo [] gopherLines
   where
    foo acc@(_:_) (CompleteInfoLine x) =
      case last acc of
        (LinkLine z) -> init acc ++ [LinkLine $ z <> "\n", CompleteInfoLine x]
        _ -> acc ++ [CompleteInfoLine x]
    foo acc@(_:_) (LinkLine x) =
      case last acc of
        (CompleteInfoLine _) -> acc ++ [LinkLine $ "\n" <> x]
        _ -> acc ++ [LinkLine x]
    foo acc n = acc ++ [n]

  gopherLineToText :: GopherLine -> Text
  gopherLineToText gopherLine =
    case gopherLine of
      NullLine -> ""
      GopherNewLine -> ""
      -- NOTE: there's a tab filter here because tabs will break info lines in the future spacecookie
      (CompleteInfoLine l) -> ((if menuMagic then "i" else "") <>) . T.filter (/= '\t') $ l
      (InfoLineToken l) -> l
      (LinkLine l) -> l


-- | DO NOT USE
instance Show (ParseEnv GopherPage) where
  --show (Block t) = unlines $ fmap (show) $ groupByLines t
  show _ = "I *said*: DO NOT USE!"


instance Semigroup GopherPage where
  x <> NullBlock = x
  NullBlock <> x = x
  Block l1 <> Block l2 = Block $ l1 ++ l2


instance Monoid GopherPage where
  mempty = NullBlock


instance Semigroup (ParseEnv GopherPage) where
  x <> y = do
    x' <- x
    y' <- y
    pure $ x' <> y'


instance Monoid (ParseEnv GopherPage) where
  mempty = pure NullBlock
  mappend = (<>)


instance Rangeable (ParseEnv GopherPage) where
  ranged _ x = x


-- TODO/FIXME: the entire token/complete scheme is counter-intuitive and should
-- at least be explained, if not replaced.
data GopherLine
  = CompleteInfoLine Text
  | InfoLineToken Text
  -- ^ An InfoLineToken is a part of an info line, which must be combined with
  -- other tokens to create a CompleteInfoLine.
  | LinkLine Text
  | GopherNewLine
  | NullLine
  deriving (Show, Eq)
-- FIXME: better name!
data GopherPage = Block [GopherLine] | NullBlock


lineToText :: GopherLine -> Text
lineToText (InfoLineToken t) = t
lineToText _ = error "this should be impossible!"  -- FIXME


-- intersperse GopherNewLine $ map InfoLineToken
transformLines :: (Text -> Text) -> GopherPage -> GopherPage
transformLines someFunc (Block gopherLines) =
  -- first change it to a bunch of text lines
  let textLines = map lineToText (gopherLines :: [GopherLine])
      transformedText = map someFunc textLines
  in Block $ map InfoLineToken transformedText
transformLines _ NullBlock = NullBlock


combineLines :: [GopherLine] -> Text
combineLines gopherLines =
  T.intercalate "" $ map lineToText gopherLines


instance Rangeable (ParseEnv GopherPage) => IsInline (ParseEnv GopherPage) where
  -- should have frontmatter setting about line breaks TODO
  lineBreak = pure NullBlock
  softBreak = do
    environment <- ask
    if envPreserveLineBreaks environment
      then pure $ Block [GopherNewLine]
      else pure NullBlock
  str t = pure $ Block [InfoLineToken t]
  entity t = pure $ Block [InfoLineToken t]
  escapedChar c = pure $ Block [InfoLineToken $ T.pack $ c:[]]

  emph penv = fmap (transformLines italicize) penv
  strong penv = fmap (transformLines embolden) penv

  -- FIXME: link
  link target title penv = do
    environment <- ask
    let menuLinks = envMenuLinks environment
    maybe penv makeLink menuLinks
   where
     makeLink (host, port) = do
      gopher <- penv
      case gopher of
        -- FIXME: label from ils messed up.
        (Block gopherLines) -> 
          let linkText = createGopherPageLink (host, port) target title (combineLines gopherLines) -- FIXME
          in pure $ Block [LinkLine linkText]
        NullBlock -> pure NullBlock

  image _ _ ils = ils
  -- could use a weird "font" for code? TODO
  code t = pure $ Block [InfoLineToken t]
  --rawInline f t = pure $ Block [InfoLineToken t]
  rawInline _ t = pure $ Block [InfoLineToken t]


-- TODO/FIXME: use/implement list item/bullet/enumerator spacing, which is
-- passed from commonmark library.  ListSpacing (which can be TightList or
-- LooseList).
listMagic :: [ParseEnv GopherPage] -> [Text] -> ParseEnv GopherPage
listMagic listItems infiniteBullets = do
  let listItemsInsideOut = (sequence listItems :: ParseEnv [GopherPage])
  (gopherMenus :: [GopherPage]) <- listItemsInsideOut
  let zipped = (zip infiniteBullets $ (map listItemFromTokens gopherMenus :: [GopherLine]) :: [(Text, GopherLine)])
  pure $ Block $ intersperse GopherNewLine $ map (\(prefix, InfoLineToken item) -> InfoLineToken $ prefix <> item) zipped
 where
  listItemFromTokens :: GopherPage -> GopherLine
  listItemFromTokens (Block gopherLines) = InfoLineToken $ T.intercalate "" . map lineToText $ gopherLines
  listItemFromTokens NullBlock = InfoLineToken ""


instance IsInline (ParseEnv GopherPage) => IsBlock (ParseEnv GopherPage) (ParseEnv GopherPage) where
  paragraph penv = do
    gopher <- penv
    case gopher of
      (Block ils) -> pure $ Block (ils ++ [GopherNewLine, GopherNewLine])
      NullBlock -> pure NullBlock
  plain penv = penv
  thematicBreak = pure $ Block [GopherNewLine, InfoLineToken "------------------", GopherNewLine]
  blockQuote penv = penv
  -- TODO: use "info" (the first argument) for codeBlock.
  codeBlock _ t = pure $ Block [InfoLineToken t]

  heading level penv = do
    gopher <- penv
    case gopher of
      (Block ils) -> do
        environment <- ask
        let fonts = envFonts environment
        pure $ Block $ [GopherNewLine, GopherNewLine] ++ formattedHeader fonts ils ++ [GopherNewLine, GopherNewLine]
      NullBlock -> pure NullBlock
   where
    headerText ils =
      T.pack $ intercalate "" $ map (T.unpack . lineToText) ils
    header fonts ils = T.lines $ parseHeading' level fonts (headerText ils)
    formattedHeader fonts ils = intersperse GopherNewLine . map InfoLineToken $ header fonts ils

  rawBlock _ t = pure $ Block [InfoLineToken t]
  -- FIXME: implement instead of just giving a blank result.
  referenceLinkDefinition _ _ = pure $ Block [InfoLineToken ""]

  list (BulletList bulletChar) _ (listItems :: [ParseEnv GopherPage]) =
    listMagic listItems (repeat $ T.cons bulletChar " ")

  list (OrderedList number enumeratorType delimiterType) _ listItems = do
    let (enumerator :: [Text]) = do
          case enumeratorType of
            Decimal -> map (T.pack . show) (drop (number - 1) [1..] :: [Int])
            UpperAlpha -> map (T.pack . show) $ drop (number - 1) ['A'..'Z']
            LowerAlpha -> map (T.pack . show) $ drop (number - 1) ['a'..'z']
            UpperRoman -> map (T.pack . toRoman) (drop (number - 1) [1..] :: [Int])
            LowerRoman -> map (T.pack . map toLower . toRoman) (drop (number - 1) [1..] :: [Int]) :: [Text]
        (prefixEnumerator :: [Text]) =
          case delimiterType of
            Period -> map (<> ". ") enumerator
            OneParen -> map (<> ") ") enumerator
            TwoParens -> map (\x -> "(" <> x <> ") ") enumerator
    listMagic listItems prefixEnumerator

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

import Network.URI
  (URI
  , uriScheme
  , parseURI
  , uriAuthority
  , uriPath
  , uriRegName
  , uriPort
  )
import Data.Maybe (isJust)
import Data.Foldable (fold)
import Data.Char (toLower)
import Data.List (groupBy, intercalate, intersperse, isPrefixOf, isSuffixOf)
import Commonmark hiding (addAttribute, escapeURI)
import           Data.Text (Text)
import qualified Data.Text as T
import Control.Monad.Reader
import qualified Data.Map as Map
import System.FilePath (takeExtension)

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
    , envBucktooth :: Bool
    -- ^ use the bucktooth/spacecookie format of .gophermap menus.
    }


toRoman :: Int -> String
toRoman num = 
  let values = [(1000,"M"), (900,"CM"), (500,"D"), (400,"CD"), (100,"C"), (90,"XC"), (50,"L"), (40,"XL"), (10,"X"), (9,"IX"), (5,"V"), (4,"IV"), (1,"I")]
      convert _ [] = ""
      convert n ((value, symbol):rest)
        | n >= value = symbol ++ convert (n - value) ((value, symbol):rest)
        | otherwise = convert n rest
  in convert num values
  
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


-- | Represents a gpohermap/menu link of (item type, label, resource/magic
-- string/path, host, port).
type GopherLink = (Char, String, String, String, Int)


-- | Create a `GopherLink` using a relative or absolute URI.
--
-- If the scheme is `http://` or `https://` then create an 'h' type URL link for
-- clients to open in a web browser.
--
-- If the URI is relative assume the host and port provided by default.
--
-- If the scheme is `gopher://` link to a (probably) remote gopher server,
-- using the host and port (or default port 70) defined in the URI.
--
-- >>> linkFromURI ("example.org", "7070") "/foo/bar/file.txt" "Some File"
-- ('0', "Some File", "/foo/bar/file.txt", "example.org", 7070)
--
-- Does not parse relative paths?
linkFromURI :: (Text, Text) -> Text -> Text -> GopherLink
linkFromURI (hostDefault, portDefault) uriText displayString =
  maybe
    (itemTypeChar . T.unpack $ uriText, T.unpack displayString, T.unpack uriText, hostDefaultString, portDefaultInt)
    fromURI
    uri
 where
  uri :: Maybe URI
  uri = parseURI . T.unpack $ uriText

  hostDefaultString :: String
  hostDefaultString = T.unpack hostDefault

  portDefaultInt :: Int
  portDefaultInt = read . T.unpack $ portDefault

  isScheme :: [String] -> Bool
  isScheme schemes = 
    case (\validScheme -> any (\x -> x ++ ":" == validScheme) schemes) . uriScheme <$> uri of
      Just True -> True
      _ -> False

  isHTTP :: Bool
  isHTTP = isScheme ["http", "https"]

  fromURI :: URI -> GopherLink
  fromURI uri' =
    let path = uriPath uri'
        charType = if isHTTP then 'h' else itemTypeChar $ uriPath uri'
        (hostOverride, portOverride) = hostPort uri'
    in (charType, T.unpack displayString, path, hostOverride, portOverride)

  hostPort :: URI -> (String, Int)
  hostPort uri' =
    case uriAuthority uri' of
      Just uriAuth ->
        -- could be an HTTP link here... FIXME logic isn't done...
        let
          host' = (if isHTTP then "URL:" else "") ++ uriRegName uriAuth
          port' =
            case uriPort uriAuth of -- will be something like ":42" or ""
              -- if gopher then default port is 70 if not a relative URI!
              "" -> 70
              x -> let withoutColon = tail x in read withoutColon :: Int
        in (host', port')
      -- FIXME: figure out why this might happen. passing the default host and
      -- port might not be wise here.
      Nothing ->
        -- the uriauthority can fail because the uris can be relative?
        -- FIXME: this is wrong if http? but then again, it can't have http
        -- links without authority specified...
        (hostDefaultString, portDefaultInt)


-- FIXME: could use a map, do a look up, use that item type char, if Nothing
-- then test if it has an extension (it'll be a binary file) otherwise it's a
-- directory!
itemTypeChar :: String -> Char
itemTypeChar path
  -- canonical types
  | "/0/" `isPrefixOf` path = '0'
  | "/1/" `isPrefixOf` path = '1'
  | "/2/" `isPrefixOf` path = '2'
  | "/3/" `isPrefixOf` path = '3'
  | "/4/" `isPrefixOf` path = '4'
  | "/5/" `isPrefixOf` path = '5'
  | "/6/" `isPrefixOf` path = '6'
  | "/7/" `isPrefixOf` path = '7'
  | "/8/" `isPrefixOf` path = '8'
  | "/9/" `isPrefixOf` path = '9'
  | "/+/" `isPrefixOf` path = '+'
  | "/g/" `isPrefixOf` path = 'g'
  | "/I/" `isPrefixOf` path = 'I'
  | "/T/" `isPrefixOf` path = 'T'
  -- gopher+ types
  | "/:/" `isPrefixOf` path = ':'
  | "/;/" `isPrefixOf` path = ';'
  | "/</" `isPrefixOf` path = '<'
  -- non-canonical types
  | "/d/" `isPrefixOf` path = 'd'
  | "/s/" `isPrefixOf` path = 's'
  -- determine by suffix instead
  | ".txt" `isSuffixOf` path = '0'
  | ".jpg" `isSuffixOf` path = 'I'
  | ".jpeg" `isSuffixOf` path = 'I'
  | ".png" `isSuffixOf` path = 'I'
  | ".gif" `isSuffixOf` path = 'g'
  -- If we couldn't match/figure it out, we'll assume it's a binary file if
  -- there's an extension
  | takeExtension path /= "" = '9'
  -- otherwise we assume (if no dot) that it's a gopher directory
  | otherwise = '1'


-- FIXME: using a hole for title. Should use title.
-- | Make a Gopher link according to the Gopher spec (RFC 1496 I think) for a
-- gophermap.
createGopherPageLink :: (Text, Text) -> Text -> Text -> Text -> Text
createGopherPageLink (host, port) target _ label =
  let
    ((itemTypeChar', displayString, magicString, host', port') :: GopherLink) =
      linkFromURI (host, port) target label
    lineParts =
      [ itemTypeChar' : displayString
      , magicString
      , host'
      , show port'
      ]
  in
    T.intercalate "\t" $ map T.pack lineParts


-- | DO NOT USE
instance Show (ParseEnv GopherLine) where
  show _ = "I *said*: DO NOT USE!"


instance Semigroup GopherLine where
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


-- | Turn the representation made from parsing the commonmark/markdown into
-- `Text` which can be written out to a file, the final product.
gopherMenuToText :: Environment -> GopherPage -> Text
gopherMenuToText _ NullBlock = ""
gopherMenuToText environment (Block t) =
  blankLineReplacements $ T.intercalate "" $ map gopherLineToText $ reduceNewLines . linkSpacing $ filter (/= NullLine) $ map joinTokens $ groupIncompleteLines t
 where
  infoLineSuffixMenuMagic = "\tfake\t(NULL)\t0"
  infoSuffixConditional = if menuMagic && not bucktooth then infoLineSuffixMenuMagic else ""
  menuMagic = isJust $ envMenuLinks environment
  bucktooth = envBucktooth environment

  blankLineReplacements = if menuMagic && not bucktooth then T.replace "\n\n" ("\ni " <> infoSuffixConditional <> "\n") else id

  predicate (InfoLineToken _) (InfoLineToken _) = True
  predicate _ _ = False
  groupIncompleteLines :: [GopherLine] -> [[GopherLine]]
  groupIncompleteLines gopherLines =
    groupBy predicate gopherLines

  joinTokens :: [GopherLine] -> GopherLine
  joinTokens [InfoLineToken l] = CompleteInfoLine $ l <> infoSuffixConditional
  joinTokens incompleteLines@(InfoLineToken _:_) =
    let (InfoLineToken token) = (fold incompleteLines :: GopherLine)
    in CompleteInfoLine $ token <> infoSuffixConditional
  joinTokens [x] = x
  joinTokens _ = error "should be impossible! FIXME!" -- FIXME

  -- | Remove newlines by adding a \n to the previous line if exists.
  reduceNewLines :: [GopherLine] -> [GopherLine]
  reduceNewLines gopherLines =
    foldl foo [] gopherLines
   where
    -- This match describes encountering a GopherNewLine and we then need to determine what came before it?
    foo acc@(_:_) GopherNewLine =
      case last acc of
        -- The line which preceeded the current GopherNewLine was another GopherNewLine! Remove the last GopherNewLine and replace it with...
        GopherNewLine -> init acc ++ [CompleteInfoLine $ if menuMagic then "\ni " <> infoLineSuffixMenuMagic <> "\n" else "\n\n"]
        (CompleteInfoLine i) -> init acc ++ [CompleteInfoLine $ i <> "\n"]
        (LinkLine link') -> init acc ++ [LinkLine $ link' <> "\n"]
        -- why this? doesn't this disappear the GopherNewLine we encountered?
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
      -- NOTE: maybe there should be a tab filter here because tabs will break info lines in the future spacecookie
      (CompleteInfoLine l) -> ((if menuMagic && not bucktooth then "i" else "") <>) $ l
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


data GopherLine
  = CompleteInfoLine Text
  -- ^ An info line which was completed as a whole line by joining a bunch of
  -- `InfoLineToken`s together.
  | InfoLineToken Text
  -- ^ An InfoLineToken is a part of an info line, which must be combined with
  -- other tokens to create a CompleteInfoLine. This is simply due to the way
  -- our parsing code interacts with `Commonmark` library, creating
  -- `InfoLineTokens` and `Block`s for everything that isn't a link.
  | LinkLine Text
  -- ^ A markdown link parsed into a gopher link according to the gopher
  -- protocol RFC, as well as the extended and noncanonical link types (like
  -- HTML URLs).
  | GopherNewLine
  | NullLine
  deriving (Show, Eq)
data GopherPage = Block [GopherLine] | NullBlock


lineToText :: GopherLine -> Text
lineToText (InfoLineToken t) = t
lineToText _ = error "this should be impossible!"  -- FIXME


-- | Use a function to change the text belonging to the lines making up a
-- `Block`.
transformLines :: (Text -> Text) -> GopherPage -> GopherPage
transformLines someFunc (Block gopherLines) =
  let textLines = map lineToText (gopherLines :: [GopherLine])
      transformedText = map someFunc textLines
  in Block $ map InfoLineToken transformedText
transformLines _ NullBlock = NullBlock


-- | Turn a bunch of `GopherLine`s into a single `Text`. This is especially
-- handy for merging `InfoLineToken`s.
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


-- $setup
-- >>> :set -XOverloadedStrings

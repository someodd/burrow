-- | Markdown to a file intended for gopherspace.
--
-- You should be familiar with the Commonmark parsing library being used.
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
import Commonmark hiding (addAttribute, escapeURI)
import           Data.Text (Text)
import qualified Data.Text as T

import TextUtils.Headings
--import Common


-- | Override certain inline instance methods for the parser.
data InlineOverrides = InlineOverrides { overrideLink :: Maybe (Text -> Text -> ParseEnv GopherFile -> ParseEnv GopherFile) }


blankInlineOverrides :: InlineOverrides
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



-- FIXME: it would be worth having target, title, and label for override interface consistency!
-- | Parse a link out to a format suitable for a regular plaintext
-- file in gopherspace.
createGopherMenuLink :: Text -> Text -> ParseEnv GopherFile -> ParseEnv GopherFile
createGopherMenuLink target title penv = do
  gopher <- penv
  case gopher of
    (GopherFile ils) -> 
      -- given the names of these functions I'm confused why the menu link's parser is being used!
      let linkText = parseLinkToGopherMenuLink ils
      in pure $ GopherFile ("\n" <> linkText <> "\n")
    GopherFileNull -> pure GopherFileNull
 where
  -- | Make a Gopher link according to the Gopher spec (RFC 1496 I think). This is for
  -- gopher menus, to be supplied as overrideLink.
  --
  -- >>> parseLinkToGopherLink "https://example.org/foo/bar/file.txt" "Some Label"
  -- this should fail
  parseLinkToGopherMenuLink :: Text -> Text
  parseLinkToGopherMenuLink label = "\n" <> determineLinkType <> "\n"
   where
    intercalateLink :: Text -> Text -> Text
    intercalateLink itemType newTarget =
      let title' = if T.null title then "" else " (" <> title <> ")"
      in T.intercalate "" [itemType, label <> title' :: Text, "\t", newTarget]

    determineLinkType :: Text
    determineLinkType
      -- Link to an HTTP web page.
      | "https" `T.isPrefixOf` target = intercalateLink "h" ("URL:" <> target)
      -- Link to a plaintext file in gopherspace.
      | ".txt" `T.isSuffixOf` target = intercalateLink "1" target
      -- Anything else is assumed to be a link to another gophermap/menu.
      | otherwise = intercalateLink "0" target


-- | A text file intended to be viewed in gopherspace.
data GopherFile = GopherFile Text | GopherFileNull


instance HasAttributes (ParseEnv GopherFile) where
  addAttributes _ x = x


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
  code t = pure $ GopherFile t
  rawInline _ t = pure $ GopherFile t

  -- | Has a special behavior of possibly using and override hook for
  -- how to parse the link. The default behavior is to simply give
  -- simply use the label. This behavior was chose because of the
  -- difference in how Gophermaps need to parse links to make them
  -- selectable/followable within the menu.
  link target title ils = do
    env <- ask
    case overrideLink $ envInlineOverrides env of
      Just func -> func target title ils
      Nothing -> ils

  -- TODO: something fun with image links... image to ascii?
  -- target title ils
  image _ _ ils = ils


-- | Parse a Markdown heading into a fancy, beautiful ASCII art heading.
parseHeading' :: Int -> HeadingLevelFontMap -> Text -> Text
parseHeading' level fonts ils = T.pack $ headingCompose font $ show ils
 where
  font =
    case Map.lookup level fonts of
      Just x -> x
      Nothing -> error $ "No font for level: " ++ show level


-- | Apply a function to the GopherFile if it isn't GopherFileNull.
gopherFileOrNull :: ParseEnv GopherFile -> (Text -> ParseEnv GopherFile) -> ParseEnv GopherFile
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
  codeBlock _ t = pure (GopherFile t)
  rawBlock _ t = pure $ GopherFile t
  referenceLinkDefinition _ _ = pure $ GopherFile ""

  -- TODO/FIXME: use config-based definition of bullet char and actually
  -- build the unordered list.
  list (BulletList _) _ _ = pure $ GopherFile ""

  -- TODO/FIXME: use config-based definition of delimiter type/enumtype
  -- (OrderedList startnum enumtype _delimtype) lSpacing items
  list (OrderedList _ _ _) _ _ = pure $ GopherFile ""

  -- | Create fancy ASCII-art headers using the ASCII art font system.
  heading level penv = do
    gopherFileOrNull penv daFunc
   where
    daFunc ils = do
      env <- ask
      let fonts = envFonts env
      pure $ GopherFile $ parseHeading' level fonts ils

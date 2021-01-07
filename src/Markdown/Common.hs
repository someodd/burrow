-- | Functions useful for rendering both text files and menus from markdown.
{-# LANGUAGE OverloadedStrings          #-}
module Markdown.Common where

import           Data.Text (Text)
import qualified Data.Text as T

import TextUtils.Headings


makeHeading' :: Text -> Text
makeHeading' t = (T.pack $ headingCompose h1 $ T.unpack t)


-- | Parse a Markdown paragraph into... well a regular paragraph.
parseParagraph :: Text -> Text
parseParagraph ils = "\n" <> ils <> "\n"


-- | Parse a Markdown heading into a fancy, beautiful ASCII art heading.
parseHeading :: Int -> Text -> Text
parseHeading level ils = T.pack $ headingCompose font $ show ils
 where
  font =
    case level of
      1 -> h1
      _ -> h1



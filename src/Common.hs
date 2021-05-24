-- This all belongs in gopherhole.ini!
{-# LANGUAGE OverloadedStrings          #-}
module Common where

import qualified Data.Text as T


-- | A file extension which maps to one of Burrow's Markdown parsers.
type ParseSuffix = String

-- | The extension to use for menus in gopherspace. This is the file extension
-- the user will use to indicate that the file is a gophermap/menu in the directory
-- they want to parse/build.
gophermapSuffix :: ParseSuffix
gophermapSuffix = ".menu.md.mustache"

-- | Same as above, but a file extension for regular text files in gopherspace.
textFileSuffix :: ParseSuffix
textFileSuffix = ".text.md.mustache"

-- | This is the special name for generating spacecookie indexes if enabled (gophermaps).
spacecookieGophermapName :: String
spacecookieGophermapName = "index.menu.md.mustache"


-- | The partial name used in the extension but also used when inserting tempaltes into
-- the partial namespace for Mustache when using partial templates.
--
-- An important note: excludes the leading dot as you might expect from file extensions.
partialExtension :: T.Text
partialExtension = "partial"

module Types where

import Control.Monad.Reader
import Data.Map as Map

import TextUtils.Headings

-- | Make the ascii art font files available to a commonmark parser.
type ParseEnv a = Reader (Map.Map String AsciiFont) a

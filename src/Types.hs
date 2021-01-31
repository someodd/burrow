module Types (ParseEnv) where

import Control.Monad.Reader (Reader)
import qualified Data.Map as Map

import TextUtils.Headings (AsciiFont)

-- | Make the ascii art font files available to a commonmark parser.
type ParseEnv a = Reader (Map.Map String AsciiFont) a

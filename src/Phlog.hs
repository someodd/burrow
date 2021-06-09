{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE FlexibleInstances          #-}
module Phlog
  (
  -- * Produce different kinds of indexes for the phlog.
  -- $phlogIndexes
    renderMainPhlogIndex
  , renderTagIndexes
  -- * Re-exports to make handling phlogs easier.
  , FrontMatter(..)
  , getFrontMatter
  , FileFrontMatter
  ) where

import Data.Hourglass as HG
import Data.Default (def)
import qualified Text.XML as XML
import Text.XML.Writer
import Data.Foldable (toList)
import Data.Hashable (Hashable)
import Data.Time.Calendar (toGregorian)
import Data.Time.Clock (getCurrentTime, utctDay)
import qualified Data.Dates.Parsing as DP
import System.FilePath (takeDirectory)
import System.Directory (createDirectoryIfMissing)
import Data.Foldable (traverse_)
import Data.Maybe (fromMaybe, fromJust)
import qualified Data.Map.Strict as Map
import qualified Data.HashMap.Strict as HashMap
--import Control.Arrow ((&&&))
import Control.Monad.Reader (runReader, Reader, ask)
import Data.List (sortOn, intercalate, isSuffixOf)
import qualified Data.Text as T

import Config (getConfig, getConfigValue)
import Common (gophermapSuffix, textFileSuffix)
import FrontMatter (FileFrontMatter, FrontMatter(..), getFrontMatter)

-- NOTE/TODO/FIXME: would it be worth forcing all [ (FilePath, FrontMatter) ] to Maybe
-- FrontMatter for consistency or should we introduce a new type for that? If we were
-- consistent more things could be shared!


-- | This is like `FrontMatter`, except blog posts require certain metadata,
-- largely to be indexed properly, to be sortable, and to be capable of being
-- put into atom/feed format.
data PostMeta = PostMeta
  { metaPublished :: T.Text--should be datetime
  , metaUpdated :: T.Text--when created defaults to metaPublished if not exist
  , metaPath :: T.Text
  -- ^ File path (relative) to the post.
  , metaTitle :: T.Text
  , metaAuthor :: Maybe T.Text
  , metaTags :: Maybe [T.Text]-- FIXME: remove Maybe because [] indicates no tags, anyway!
  , metaFrontMatter :: FrontMatter
  -- ^ All of the FrontMatter.
  } deriving (Show)

-- FIXME: need beautiful errors informing a user of what is needed.
-- only trigger this if it is post type?
pairToPostMeta :: FileFrontMatter -> Maybe PostMeta
pairToPostMeta (filePath, Just frontMatter) = Just $ PostMeta
  { metaPublished = fromJust $ fmPublished frontMatter
  , metaUpdated = fromMaybe (fromJust $ fmPublished frontMatter) (fmUpdated frontMatter)
  , metaPath = T.pack filePath
  , metaTitle = fromJust $ fmTitle frontMatter
  , metaAuthor = fmAuthor frontMatter
  -- ^ Can be Nothing because of default author entry in .ini
  , metaTags = fmTags frontMatter
  , metaFrontMatter = frontMatter
  }
pairToPostMeta (_, Nothing) = Nothing

-- must filter out non post types FIXME
-- | Filters out non-posts based on the type: post.
preparePostsOnlyFromPairs :: [FileFrontMatter] -> [PostMeta]
preparePostsOnlyFromPairs filePathFrontMatterPairs = do
  [postMeta | pair@(_, Just frontMatter) <- filePathFrontMatterPairs, fromMaybe False (fmType frontMatter >>= \t -> Just $ t == "post"), let (Just postMeta) = pairToPostMeta pair]


data PhlogConfig = PhlogConfig
  { phlogPath :: FilePath
  -- ^ Path to the phlog directory.
  , phlogTagPath :: FilePath
  -- ^ Path to the phlog's tags directory.
  , phlogDefaultAuthor :: String
  -- ^ The name of the author that is used by default.
  , phlogHost :: String
  -- ^ The domain name/host/ip address of the gopherhole.
  , phlogPort :: String
  -- ^ The port of the gopherhole.
  }

getPhlogConfig :: IO PhlogConfig
getPhlogConfig = do
  configParser <- getConfig
  -- phlog section
  phlogPath' <- getConfigValue configParser "phlog" "phlogPath"
  tagPath' <- getConfigValue configParser "phlog" "tagPath"
  defaultAuthor' <- getConfigValue configParser "phlog" "defaultAuthor"
  -- general section
  host' <- getConfigValue configParser "general" "host"
  port' <- getConfigValue configParser "general" "port"
  pure $ PhlogConfig
    { phlogPath = phlogPath'
    , phlogTagPath = tagPath'
    , phlogDefaultAuthor = defaultAuthor'
    , phlogHost = host'
    , phlogPort = port'
    }


data AtomFeedRecipe = AtomFeedRecipe
  { atomTitle :: T.Text
  -- ^ The title of the feed.
  , atomID :: T.Text
  -- ^ The ID of the feed (URI).
  , atomEntries :: [PostMeta]
  -- ^ All the individual entries for the feed.
  , atomPhlogConfig :: PhlogConfig
  -- ^ All of the information from the config required to create the feed.
  , atomPath :: FilePath
  -- ^ Path (not URI) to this feed.
  }

data AtomFeedEntryRecipe = AtomFeedEntryRecipe
  { entryPostMetas :: [PostMeta]
  , entryPhlogConfig :: PhlogConfig
  }

baseURL :: PhlogConfig -> String
baseURL phlogConfig =
  "gopher://" ++ phlogHost phlogConfig ++ ":" ++ phlogPort phlogConfig ++ "/"

instance ToXML AtomFeedEntryRecipe where
  toXML (AtomFeedEntryRecipe { entryPostMetas=postMetas, entryPhlogConfig=phlogConfig }) = do
    -- FIXME: needs to iterate and needs to not assume Just
    foldr ((>>) . assembleEntry) empty postMetas
   where
    -- These could be combined easily due to maybe monad...
    assembleEntry postMeta = do
     let postURI = (T.pack $ baseURL phlogConfig ++ (T.unpack $ metaPath postMeta))
     element "entry" $ do
       element "title" $ content (metaTitle postMeta)
       elementA "link" [("href", postURI)] empty
       element "id" $ content postURI
       element "author" $ element "name" $ content (T.pack $ phlogDefaultAuthor phlogConfig)
       fromMaybe empty (metaTags postMeta >>= \t -> Just $ elementA "category" [("term", T.intercalate " " t)] empty)
       element "published" $ content (rfc3339 $ metaPublished postMeta)
       element "updated" $ content (rfc3339 $ metaUpdated postMeta)

-- | The Atom spec wants rfc3339 (ISO8601 as far as I can tell) datetime strings.
rfc3339 :: T.Text -> T.Text
rfc3339 dateString =
  -- FIXME: could just put current year into the feed recipe?
  let currentYear = 2021
      -- TODO: config to set timezone
      dateTime = (HG.timePrint HG.ISO8601_DateAndTime $ (dateStringToDateTime currentYear dateString))
  in T.pack $ dateTime

createAtomFeed :: AtomFeedRecipe -> XML.Document
createAtomFeed atomFeedRecipe = do
  let phlogConfig = atomPhlogConfig atomFeedRecipe
      base = baseURL phlogConfig
  atomDocument $ do
    element "title" $ content (atomTitle atomFeedRecipe)
    elementA "link" [("href", T.pack base)] $ empty
    elementA "link" [("href", T.pack $ base ++ (atomPath atomFeedRecipe)), ("rel", "self")] $ empty
    elementA "generator" [("uri", "https://github.com/hyperrealgopher/burrow")] $ content "hyperrealgopher's burrow" 
    element "id" $ content (T.pack $ base ++ (atomPath atomFeedRecipe))
    element "updated" $ content (T.pack lastUpdated)--FIXME
    toXML $ AtomFeedEntryRecipe (atomEntries atomFeedRecipe) phlogConfig
    comment "that's it!"
 where
  -- FIXME: repeats/overlap rfc3339
  lastUpdated :: String
  lastUpdated = HG.timePrint HG.ISO8601_DateAndTime $ maximum $ map (dateStringToDateTime 2021 . metaUpdated) $ atomEntries atomFeedRecipe

  atomDocument children = XML.Document
    { XML.documentPrologue = XML.Prologue def def def
    , XML.documentRoot = XML.Element "feed" (Map.fromList [("xmlns","http://www.w3.org/2005/Atom")]) (render children)
    , XML.documentEpilogue = def
    }

-- TODO: look for patterns throughout all the instances and automate it here
-- | Represents different kinds of phlog indexes and the tools required to do
-- things with them.
--
-- Type `a` is what will create the model (type `b`) used in creating the
-- gophermap as well as the Atom feed.
class PhlogIndex a b | b -> a where
  -- | Sort the index model.
  sortIndexModel :: b -> b

  -- | Create the data model of the phlog index in question. Do not use this
  -- directly, simply define it.
  createIndexModel' :: a -> b

  -- | Do not override this. This is what you should use to actually
  -- create the index model.
  createIndexModel :: a -> b
  createIndexModel = sortIndexModel . createIndexModel'

  -- | Render the phlog index model file in order to be viewed as a gophermap
  -- in gopherspace.  This includes creating the string/file contents from the
  -- supplied model, which is then written to file.
  renderIndexGophermap :: b -> IO ()

  -- TODO:
  -- This could do more work or have two abstract/higher order functoins since
  -- all my renderatoms are basically the same and jsut spit out a document and
  -- then write the exact same way...
  -- could do a createAtom' b -> document then renderAtom Document -> IO but
  -- that complicates things because path needed
  renderAtom :: b -> IO ()

  -- | Render both the gophermap and the atom feed.
  renderAll :: b -> IO ()
  renderAll b = renderAtom b >> renderIndexGophermap b


-- The Int is the current year
newtype MainPhlogIndex = MainPhlogIndex (Reader Integer [PostMeta])

-- | The main phlog index which contains all of the posts.
instance PhlogIndex [PostMeta] MainPhlogIndex where
  sortIndexModel (MainPhlogIndex readerIntPostMetas) = do
      MainPhlogIndex $ do
        currentYear <- ask
        -- FIXME: this is so like the sorting function i have external to this instance except the fm is a maybe
        let sorter = sortOn (\pm -> dateStringToDateTime currentYear $ metaUpdated pm)
        fmap sorter readerIntPostMetas

  createIndexModel' postMetasPairs = do
    MainPhlogIndex $ do
      currentYear <- ask
      pure $ makePhlogIndex postMetasPairs currentYear
   where
    -- TODO: actually parse into a nice thing
    -- | An index (gophermap) of all the phlog posts! Just sorts all the
    -- filepath/frontmatter pairs.
    makePhlogIndex :: [PostMeta] -> Integer -> [PostMeta]
    makePhlogIndex postMetas defaultYear =
      sortOn (\y -> dateStringToDateTime defaultYear $ metaPublished y) postMetas

  renderIndexGophermap (MainPhlogIndex mainPhlogIndex) = do
    -- TODO: list main tag index
    -- TODO: 
    --  createDirectoryIfMissing True (takeDirectory mainTagIndexPath)
    -- FIXME: directories need to be defined in config
    -- | Create the main phlog index which includes all the posts sorted by date.
    currentYear <- getCurrentYear
    configParser <- getConfig
    buildPath <- getConfigValue configParser "general" "buildPath"
    phlogPath' <- getConfigValue configParser "phlog" "phlogPath"
    tagPath <- getConfigValue configParser "phlog" "tagPath"
    let outputPath = buildPath ++ "/" ++ phlogPath' ++ "/.gophermap"
        phlogIndex = runReader mainPhlogIndex currentYear
    writeFile outputPath $ makePhlogIndexPage phlogIndex tagPath
   where
    makePhlogIndexPage :: [PostMeta] -> FilePath -> String
    makePhlogIndexPage postMetas tagIndexPath =
      let viewByTagsEntry = show $ MenuLink "1" "view by tags" tagIndexPath Nothing Nothing
          allThePosts = "all phlog posts\n" ++ (intercalate "\n" $ map (makeLocalLink) postMetas)
      in viewByTagsEntry ++ "\n" ++ allThePosts

  renderAtom (MainPhlogIndex mainPhlogIndex) = do
    currentYear <- getCurrentYear
    phlogConfig <- getPhlogConfig
    let phlogIndex = runReader mainPhlogIndex currentYear
        -- FIXME
        atomFeed = (createAtomFeed $ AtomFeedRecipe {atomTitle="All Posts", atomID="foo", atomEntries=phlogIndex, atomPhlogConfig=phlogConfig, atomPath="phlog/main.xml"})
    createDirectoryIfMissing True "built/phlog/"
    XML.writeFile def "built/phlog/main.xml" atomFeed


renderMainPhlogIndex :: [FileFrontMatter] -> IO ()
renderMainPhlogIndex pairs = do
  let mainPhlogIndex = createIndexModel (preparePostsOnlyFromPairs pairs) :: MainPhlogIndex
  renderAll mainPhlogIndex


-- | This is for the default date while interpreting FrontMatter dates.
getCurrentYear :: IO Integer
getCurrentYear = (\(y,_,_) -> y) <$> (getCurrentTime >>= return . toGregorian . utctDay) 


type Tag = T.Text


-- | Sort [PostMeta] based on date.
sortOnDate :: Integer -> [PostMeta] -> [PostMeta]
sortOnDate defaultYear =
  sortOn (\y -> dateStringToDateTime defaultYear $ metaUpdated y)


-- FIXME TODO
-- in the future also should look more like a hashmap of tag to [(filepath, frontmatter)]
-- in fact passing the sorted tag index could be something handy to have as reader in both instances
-- for efficiency
newtype MainTagIndex = MainTagIndex (HashMap.HashMap Tag [PostMeta])

-- FIXME/TODO: there's a TON of overlap between this and the other tag indexes so write some helper functions
-- FIXME/TODO: just use Group like SpecificTagIndex does but for all
-- | The phlog index which lists all the tags and the latest posts for each tag.
instance PhlogIndex [PostMeta] MainTagIndex where
  -- | Sort the hashmap values by date.
  sortIndexModel (MainTagIndex mainTagIndexMap) =
    let defaultYear = 2021
    in MainTagIndex $ HashMap.map (sortOnDate defaultYear) mainTagIndexMap

  -- NOTE: due to PhlogIndex SpecificTagIndex being similar, this results in much overlap/recalculation...
  createIndexModel' postMetas =
    MainTagIndex $ HashMap.map (take postsPerTag) $ HashMap.fromListWith (++) result
   where
    -- TODO/FIXME: make into config value... will have to use reader?
    postsPerTag :: Int
    postsPerTag = 3

    result :: [ (Tag, [PostMeta]) ]
    result =
      [(tag, [postMeta]) | postMeta <- postMetas, let (Just tags') = metaTags postMeta, tag <- tags']-- FIXME: what if no tags? that should error right and be just fine?

  -- TODO: rewrite better
  renderIndexGophermap (MainTagIndex mainTagIndex) = do
    -- get paths from config
    configParser <- getConfig
    buildPath <- getConfigValue configParser "general" "buildPath"
    tagPath <- getConfigValue configParser "phlog" "tagPath"
    let mainTagIndexPath = buildPath ++ "/" ++ tagPath ++ "/.gophermap"

    -- write out the main tag index
    createDirectoryIfMissing True (takeDirectory mainTagIndexPath)
    writeFile mainTagIndexPath (makeMainTagIndex tagPath)
   where
    allTags :: [T.Text]
    allTags = HashMap.keys mainTagIndex

    -- TODO: rewrite better
    -- | Write the index of tag indexes! This makes it so the user can see all the tags and
    -- see five example posts (should make this sorted by date...) should include instead of
    -- filepath include a bunch of data necessary for building the indexes.
    makeMainTagIndex :: FilePath -> String
    makeMainTagIndex tagIndexDirectory =
      -- fromjust is bad
      -- FIXME: this kleisli function is a hack because no fm was included in tag index at the moment
      let viewAllPostsEntry tag = show $ MenuLink "1" ("View all posts tagged " ++ (T.unpack tag)) (tagIndexDirectory ++ (T.unpack tag)) Nothing Nothing
          threeSummary tag = intercalate "\n"
            [ (T.unpack tag)
            -- FIXME: use of fromJust here is bad.
            , intercalate "\n" $ map makeLocalLink $ fromJust $ HashMap.lookup tag mainTagIndex
            , viewAllPostsEntry tag
            ]
      in intercalate "\n\n" $ map threeSummary allTags

  -- FIXME/TODO: this is not doing what it should be! very sloppily put together...
  renderAtom (MainTagIndex mainTagIndexMap) = do
    -- FIXME: bad!
    phlogConfig <- getPhlogConfig
    let phlogIndex = foldr ((++) . snd) [] $ HashMap.toList $ mainTagIndexMap
        atomFeed = (createAtomFeed $ AtomFeedRecipe "blah" "foo" phlogIndex phlogConfig "phlog/tagSummary.xml") -- FIXME: path
    createDirectoryIfMissing True "built/phlog/"
    -- FIXME
    XML.writeFile def "built/phlog/tagSummary.xml" atomFeed


-- FIXME: this no longer makes sense the way it compiles results.
-- this one could be more general like PostMetasCriterion for cats and authors etc
--newtype PostMetasTag = PostMetasTag (Tag, PostMetas)
newtype SpecificTagIndex = SpecificTagIndex (Tag, [PostMeta])

instance PhlogIndex (PostMetasGroupPair Tag) SpecificTagIndex where
  sortIndexModel (SpecificTagIndex (tag, filePathFrontMatterPairs)) =
    let year = 2021
    in SpecificTagIndex $ (tag, sortOnDate year filePathFrontMatterPairs)

  -- FIXME: is all I need to do is sort postMetasPairs?
  createIndexModel' (PostMetasGroupPair (_, tag, postMetasPairs)) =
    -- FIXME
    SpecificTagIndex (tag, postMetasPairs)

  renderIndexGophermap (SpecificTagIndex (tag, specificTagIndexes)) = do
    configParser <- getConfig
    tagIndexPath <- getConfigValue configParser "phlog" "tagPath"
    buildPath <- getConfigValue configParser "general" "buildPath"
    let outputPath = buildPath ++ "/" ++ tagIndexPath ++ "/" ++ (T.unpack tag) ++ "/.gophermap"
        outputDirectoryPath = takeDirectory outputPath
    createDirectoryIfMissing True outputDirectoryPath
    writeFile outputPath (tagIndexContents tag)
   where
    -- FIXME: no need for second argument Tag
    tagIndexContents :: Tag -> String
    tagIndexContents _ =
      -- fromjust bad! FIXME
      (T.unpack tag) ++ "\n\n" ++ (intercalate "\n" $ map makeLocalLink specificTagIndexes)

  renderAtom (SpecificTagIndex (tag, phlogIndex)) = do
    phlogConfig <- getPhlogConfig
    let filePath = "built/phlog/tags/" ++ (T.unpack tag) ++ ".xml"
        metas = phlogIndex
        -- FIXME
        atomFeed = (createAtomFeed $ AtomFeedRecipe (T.pack $ (T.unpack tag) ++ " tag phlog feed") "foo" metas phlogConfig ("phlog/tags/" ++ (T.unpack tag) ++ ".xml"))
    createDirectoryIfMissing True (takeDirectory filePath)
    XML.writeFile def filePath atomFeed


newtype PostMetasGroupPair a = PostMetasGroupPair (String, a, [PostMeta]) -- this is what get accepted by the thingy builder
newtype PostMetasGroup a = PostMetasGroup (String, HashMap.HashMap a [PostMeta]) deriving (Show)


getPostMetasGroupPair :: (Eq a, Hashable a) => PostMetasGroup a -> a -> PostMetasGroupPair a
getPostMetasGroupPair (PostMetasGroup (label, hashMap)) key =
  -- FIXME: fromjust
  PostMetasGroupPair (label, key, fromJust $ HashMap.lookup key hashMap)

-- | Group posts together by some property of the FrontMatter. Weeds out posts
-- without FrontMatter.
--
-- If you're not getting a value that is a list you can use Identity as the return
-- value. 
--
-- >>> import Data.Functor.Identity (Identity(..))
-- >>> (frontMatterHashMapGroup somePostMetas ("title", Identity . title) :: (String, HashMap.HashMap (Maybe T.Text) [PostMeta))
-- >>> (frontMatterHashMapGroup somePostMetas ("tag", tags) :: (String, HashMap.HashMap Tag [PostMeta]))
frontMatterHashMapGroup
  :: (Eq a, Hashable a, Foldable f)
  => [PostMeta]
  -> (String, PostMeta -> f a)
  -> PostMetasGroup a
frontMatterHashMapGroup postMetaList (groupName, groupFunction) =
  PostMetasGroup $ (groupName, HashMap.fromListWith (++) result)
 where
  result =
    [ (group, [postMeta]) | postMeta <- postMetaList, group <- (toList $ groupFunction postMeta)]


-- | Render the tag indexes from a collection of file paths and their
-- associated `FrontMatter` (if any), which contains the tags for that file.
--
-- The tags are written out to the supplied `FilePath`.
renderTagIndexes :: [FileFrontMatter] -> IO ()
renderTagIndexes filePathFrontMatter = do
  let postMetaList = preparePostsOnlyFromPairs filePathFrontMatter
      -- FIXME/TODO: I filter out no tags instead of putting into [] group?
      ppmg@(PostMetasGroup (_, hashMap)) = (frontMatterHashMapGroup postMetaList ("tag", \pm -> fromMaybe [] (metaTags pm)) :: PostMetasGroup Tag)
      tagsFound = HashMap.keys hashMap
  traverse_ (\x -> renderAll (createIndexModel $ getPostMetasGroupPair ppmg x :: SpecificTagIndex)) tagsFound

  let mainTagIndex = createIndexModel (preparePostsOnlyFromPairs filePathFrontMatter) :: MainTagIndex
  renderAll mainTagIndex


-- FIXME: add fancy error for this and also make a part of frontmatter and automatically transform into this type?
-- FIXME: could error out (usage of `head`)
-- | Fuzzy match a date time string (for a `FrontMatter` date/time definition).
--
-- `extractDateTimesY` does all the heavy lifting.
--
-- >>> dateStringToDateTime (2021 :: Integer) (T.pack "july 29")
-- DateTime {dtDate = Date {dateYear = 2021, dateMonth = July, dateDay = 29}, dtTime = TimeOfDay {todHour = 0h, todMin = 0m, todSec = 0s, todNSec = 0ns}}
-- >>> dateStringToDateTime (2021 :: Integer) (T.pack "2021-06-20T04:30")
-- DateTime {dtDate = Date {dateYear = 2021, dateMonth = June, dateDay = 20}, dtTime = TimeOfDay {todHour = 0h, todMin = 0m, todSec = 0s, todNSec = 0ns}}
dateStringToDateTime :: Integer -> T.Text -> DP.DateTime
dateStringToDateTime defaultYear dateText = head $ DP.extractDateTimesY (fromIntegral defaultYear :: Int) (T.unpack dateText)


-- | A useful tool in making phlog indexes: create a nice link for the menu
-- using a supplied pair from PostMetas.
makeLocalLink :: PostMeta -> String
makeLocalLink postMeta
  | gophermapSuffix `isSuffixOf` path = restOfLink "1"
  | textFileSuffix `isSuffixOf` path = restOfLink "0"
  -- Otherwise we are very lazily just going to assume it's a text file. This
  -- will be improved in the future.
  | otherwise = restOfLink "1"
 where
  path :: FilePath
  path = T.unpack $ metaPath postMeta

  restOfLink :: String -> String
  restOfLink itemType = show $ MenuLink itemType (T.unpack $ metaTitle postMeta) path Nothing Nothing


-- TODO: Maybe belongs in common.
-- | Representation of a gophermap menu entry, specifically links and not info items.
data MenuLink = MenuLink
  { linkType :: String
  , displayString :: String
  , selector :: String
  , server :: Maybe String
  , port :: Maybe Int }

-- | Make a spacecookie spec .gophermap link.
--
-- gopherfiletypeNAME\t [SELECTOR [\tSERVER [\tPORT]]]
--
-- https://sternenseemann.github.io/spacecookie/spacecookie.gophermap.5.html
instance Show MenuLink where
  show ml =
    let firstPart =
          intercalate "\t"
            [ (linkType ml) ++ (displayString ml)
            , "/" ++ (selector ml)
            ]
        secondPart =
          fromMaybe "" (server ml >>= \x -> port ml >>= \y -> Just $ "\t" ++ x ++ "\t" ++ (show y))
    in firstPart ++ secondPart

{- $phlogIndexes

A phlog index is like blog indexes. There are different ways to index the posts to a phog.
There's the main index which simply is a list of *all* of the blog posts in order of date.
There's also individual tag indexes, which lists posts belonging to a specific tag. There's
also an index of all the tags, showing a few of the msot recent posts belonging to those
tags. The pattern here is that there's different ways of listing the meta/information
belonging to phlog posts, in a way that's presentable and easily perused by the user.

These specific kinds of indexes are the type that gets written to file to be browsed
in gopher. These same indexes also have associated RSS feeds.
-}

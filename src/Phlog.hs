{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE FlexibleInstances          #-}
{- | Logic for blogging.

I will likely refactor to use recipes or the like.

-}
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
import System.FilePath (splitExtension, takeDirectory, (</>), (<.>))
import System.Directory (createDirectoryIfMissing)
import Data.Foldable (traverse_)
import Data.Maybe (fromMaybe, fromJust)
import qualified Data.Map.Strict as Map
import qualified Data.HashMap.Strict as HashMap
--import Control.Arrow ((&&&))
import Control.Monad.Reader (runReader, Reader, ask)
import Data.List (sortOn, intercalate, isSuffixOf)
import qualified Data.Text as T

import qualified Config
import FrontMatter (FileFrontMatter, FrontMatter(..), getFrontMatter)


-- | The Atom spec wants rfc3339 (ISO8601 as far as I can tell) datetime strings.
toRFC3339 :: DP.DateTime -> T.Text
toRFC3339 = T.pack . HG.timePrint HG.ISO8601_DateAndTime


-- | This is like `FrontMatter`, except blog posts require certain metadata,
-- largely to be indexed properly, to be sortable, and to be capable of being
-- put into atom/feed format.
--
-- To get all the info on what each record means look at `FrontMatter`.
data PostMeta = PostMeta
  { metaPublished :: DP.DateTime
  , metaUpdated :: DP.DateTime-- FIXME: use Data.Default so it can default to metaPublished.
  -- ^ Defaults to metaPublished.
  , metaPath :: T.Text
  -- ^ File path (relative) to the post.
  , metaTitle :: T.Text
  , metaAuthor :: Maybe T.Text
  , metaTags :: Maybe [T.Text]
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
  -- metaAuthor can be Nothing because of default author entry in .ini
  , metaTags = fmTags frontMatter
  , metaFrontMatter = frontMatter
  }
pairToPostMeta (_, Nothing) = Nothing

-- must filter out non post types FIXME
-- | Filters out non-posts based on the type: post.
preparePostsOnlyFromPairs :: [FileFrontMatter] -> [PostMeta]
preparePostsOnlyFromPairs filePathFrontMatterPairs = do
  [postMeta | pair@(_, Just frontMatter) <- filePathFrontMatterPairs, fromMaybe False (fmType frontMatter >>= \t -> Just $ t == "post"), let (Just postMeta) = pairToPostMeta pair]


{- | Phlog meta/config.

This is redundant and may get removed in the near future.

-}
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


getPhlogConfig :: Config.Config -> PhlogConfig
getPhlogConfig config =
  let
    -- phlog section
    phlogPath' = Config.phlogPath (Config.phlog config) 
    tagPath' = Config.tagPath (Config.phlog config) 
    defaultAuthor' = Config.defaultAuthor (Config.phlog config) 
    -- general section; FIXME: spacecookie section now?
    host' = Config.host (Config.general config) 
    port' = Config.port (Config.general config)
  in
    PhlogConfig
      { phlogPath = T.unpack phlogPath'
      , phlogTagPath = T.unpack tagPath'
      , phlogDefaultAuthor = T.unpack defaultAuthor'
      , phlogHost = T.unpack host'
      , phlogPort = show port'
      }


data AtomFeedRecipe = AtomFeedRecipe
  { atomTitle :: T.Text
  -- ^ The title of the feed.
  , atomEntries :: [PostMeta]
  -- ^ All the individual entries for the feed.
  , atomPhlogConfig :: PhlogConfig
  -- ^ All of the information from the config required to create the feed.
  , atomPath :: FilePath
  -- ^ Path (not URI) to this feed. Is also used for the feed ID and its self href.
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
       element "published" $ content (toRFC3339 $ metaPublished postMeta)
       element "updated" $ content (toRFC3339 $ metaUpdated postMeta)


-- FIXME: need to handle no entries.
createAtomFeed :: AtomFeedRecipe -> XML.Document
createAtomFeed atomFeedRecipe = do
  let phlogConfig = atomPhlogConfig atomFeedRecipe
      base = baseURL phlogConfig
  atomDocument $ do
    element "title" $ content (atomTitle atomFeedRecipe)
    elementA "link" [("href", T.pack base)] $ empty
    elementA "link" [("href", T.pack $ base ++ (atomPath atomFeedRecipe)), ("rel", "self")] $ empty
    elementA "generator" [("uri", "https://github.com/someodd/burrow")] $ content "someodd's burrow" 
    element "id" $ content (T.pack $ base ++ (atomPath atomFeedRecipe))
    element "updated" $ content lastUpdated--FIXME
    toXML $ AtomFeedEntryRecipe (atomEntries atomFeedRecipe) phlogConfig
    comment "that's it!"
 where
  -- FIXME: I noticed that this indicates an error where atomEntries is empty... that should be impossible!
  -- I think it's because of tagSummary! If tagSummary is blank.
  lastUpdated :: T.Text
  lastUpdated = case map metaUpdated (atomEntries atomFeedRecipe) of
                  [] -> "n/a"-- FIXME: this is a bad way to handle this error?
                  list -> toRFC3339 $ maximum list

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
--
-- Note in the future I may change passing around the config for just ensuring
-- you define a Reader Config ... or something like that.
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
  renderIndexGophermap :: Config.Config -> b -> IO ()

  -- TODO:
  -- This could do more work or have two abstract/higher order functoins since
  -- all my renderatoms are basically the same and jsut spit out a document and
  -- then write the exact same way...
  -- could do a createAtom' b -> document then renderAtom Document -> IO but
  -- that complicates things because path needed
  renderAtom :: Config.Config -> b -> IO ()

  -- | Render both the gophermap and the atom feed.
  renderAll :: Config.Config -> b -> IO ()
  renderAll config b = renderAtom config b >> renderIndexGophermap config b


-- FIXME: no need for reader anymore (year)
-- The Int is the current year
newtype MainPhlogIndex = MainPhlogIndex (Reader Integer [PostMeta])

-- | The main phlog index which contains all of the posts.
instance PhlogIndex [PostMeta] MainPhlogIndex where
  sortIndexModel (MainPhlogIndex readerIntPostMetas) = do
      MainPhlogIndex $ do
        -- FIXME
        _ <- ask
        fmap (sortOn metaUpdated) readerIntPostMetas

  createIndexModel' postMetasPairs = do
    MainPhlogIndex $ do
      _ <- ask
      pure $ postMetasPairs

  renderIndexGophermap config (MainPhlogIndex mainPhlogIndex) = do
    currentYear <- getCurrentYear
    let
      buildPath = T.unpack $ Config.buildPath (Config.general config)
      phlogPath' = T.unpack $ Config.phlogPath (Config.phlog config)
      tagPath' = T.unpack $ Config.tagPath (Config.phlog config)
      outputPath = buildPath </> phlogPath' </> ".gophermap"
      phlogIndex = runReader mainPhlogIndex currentYear
    writeFile outputPath $ makePhlogIndexPage phlogIndex tagPath' phlogPath'
   where
    makePhlogIndexPage :: [PostMeta] -> FilePath -> FilePath -> String
    makePhlogIndexPage postMetas tagIndexPath phlogPathRelative =
      let viewByTagsEntry = show $ MenuLink "1" "view by tags" tagIndexPath Nothing Nothing
          atomEntry = show $ MenuLink "0" "atom feed" (phlogPathRelative </> "main.xml") Nothing Nothing
          allThePosts = "all phlog posts\n" ++ (intercalate "\n" $ map (makeLocalLink) postMetas)
      in atomEntry ++ "\n" ++ viewByTagsEntry ++ "\n" ++ allThePosts

  renderAtom config (MainPhlogIndex mainPhlogIndex) = do
    currentYear <- getCurrentYear
    let phlogConfig = getPhlogConfig config
        buildPath = T.unpack $ Config.buildPath (Config.general config)
    let phlogIndex = runReader mainPhlogIndex currentYear
        phlogDirectory = phlogPath phlogConfig
        -- FIXME
        atomFeed = (createAtomFeed $ AtomFeedRecipe {atomTitle="All Posts", atomEntries=phlogIndex, atomPhlogConfig=phlogConfig, atomPath=phlogDirectory </> "main.xml"})
    createDirectoryIfMissing True (buildPath </> phlogDirectory)
    XML.writeFile def (buildPath </> phlogDirectory </> "main.xml") atomFeed


renderMainPhlogIndex :: Config.Config -> [FileFrontMatter] -> IO ()
renderMainPhlogIndex config pairs = do
  let mainPhlogIndex = createIndexModel (preparePostsOnlyFromPairs pairs) :: MainPhlogIndex
  renderAll config mainPhlogIndex


-- | This is for the default date while interpreting FrontMatter dates.
getCurrentYear :: IO Integer
getCurrentYear = (\(y,_,_) -> y) <$> (getCurrentTime >>= return . toGregorian . utctDay) 


type Tag = T.Text


-- FIXME: do not need defaultYear anymore
-- | Sort [PostMeta] based on date.
sortOnDate :: Integer -> [PostMeta] -> [PostMeta]
sortOnDate _ = sortOn metaUpdated


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
      [(tag, [postMeta]) | postMeta <- postMetas, tag <- fromMaybe [] (metaTags postMeta)]-- FIXME: what if no tags? that should error right and be just fine?

  -- TODO: rewrite better
  renderIndexGophermap config (MainTagIndex mainTagIndex) = do
    -- get paths from config
    -- FIXME: what if build path from CLI instead of INI? Also why not use getPhlogConfig?
    -- It also doesn't make sense that we have a --spacecookie flag but then we just use
    -- .gophermap by default everywhere.
    let
      buildPath = T.unpack $ Config.buildPath (Config.general config)
      tagPath = T.unpack $ Config.tagPath (Config.phlog config)
      mainTagIndexPath = buildPath </> tagPath </> ".gophermap"

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

  -- FIXME: hardcoded paths!
  -- FIXME/TODO: this is not doing what it should be! very sloppily put together...
  renderAtom config (MainTagIndex mainTagIndexMap) = do
    let
      phlogConfig = getPhlogConfig config
      buildPath = T.unpack $ Config.buildPath (Config.general config)
      phlogIndex = foldr ((++) . snd) [] $ HashMap.toList $ mainTagIndexMap
      phlogDirectory = phlogPath phlogConfig
      atomRelativePath = phlogDirectory </> "tagSummary.xml"
      atomFeed = (createAtomFeed $ AtomFeedRecipe "Tag Summaries" phlogIndex phlogConfig atomRelativePath) -- FIXME: path
    createDirectoryIfMissing True $ buildPath </> phlogDirectory
    XML.writeFile def (buildPath </> atomRelativePath) atomFeed


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

  renderIndexGophermap configParser (SpecificTagIndex (tag, specificTagIndexes)) = do
    let
      tagIndexPath = T.unpack $ Config.tagPath (Config.phlog configParser)
      buildPath = T.unpack $ Config.buildPath (Config.general configParser)
      phlogPath' = T.unpack $ Config.phlogPath (Config.phlog configParser)
      outputPath = buildPath </> tagIndexPath </> (T.unpack tag) </> ".gophermap"
      outputDirectoryPath = takeDirectory outputPath
    createDirectoryIfMissing True outputDirectoryPath
    writeFile outputPath (tagIndexContents phlogPath')
   where
    -- FIXME: no need for second argument Tag
    -- atomEntry = show $ MenuLink "1" "atom feed for this tag" (phlogPathRelative </> "tags" </> (T.unpack tag) </> ".xml") Nothing Nothing
    tagIndexContents :: String -> String
    tagIndexContents phlogPathRelative =
      -- fromjust bad! FIXME
      let
        atomEntry = show $ MenuLink "0" "atom feed for this tag" (phlogPathRelative </> "tags" </> (T.unpack tag) ++ ".xml") Nothing Nothing
      in
        (T.unpack tag) ++ "\n" ++ atomEntry ++ "\n\n" ++ (intercalate "\n" $ map makeLocalLink specificTagIndexes)

  renderAtom config (SpecificTagIndex (tag, phlogIndex)) = do
    let
      phlogConfig = getPhlogConfig config
      outputDirectory = T.unpack $ Config.buildPath (Config.general config)
      phlogDirectory = phlogPath phlogConfig
      feedRelativePath = phlogDirectory </> "tags" </> (T.unpack tag <.> "xml")
      outputPath = outputDirectory </> feedRelativePath
      metas = phlogIndex
      -- FIXME
      atomFeed = (createAtomFeed $ AtomFeedRecipe (T.pack $ (T.unpack tag) ++ " tag phlog feed") metas phlogConfig feedRelativePath)
    createDirectoryIfMissing True (takeDirectory outputPath)
    XML.writeFile def outputPath atomFeed


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
-- >>> import Data.HashMap.Strict
-- >>> import Data.Text
-- >>> frontMatterHashMapGroup somePostMetas ("title", Identity . title) :: (String, HashMap (Maybe Text) [PostMeta])
-- ...
-- >>> frontMatterHashMapGroup somePostMetas ("tag", tags) :: (String, HashMap.HashMap Tag [PostMeta])
-- ...
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


{- | Render the tag indexes from a collection of file paths and their
associated `FrontMatter` (if any), which contains the tags for that file.

The tags are written out to the supplied `FilePath`.
-}
renderTagIndexes :: Config.Config -> [FileFrontMatter] -> IO ()
renderTagIndexes config filePathFrontMatter = do
  let postMetaList = preparePostsOnlyFromPairs filePathFrontMatter
      -- FIXME/TODO: I filter out no tags instead of putting into [] group?
      ppmg@(PostMetasGroup (_, hashMap)) = (frontMatterHashMapGroup postMetaList ("tag", \pm -> fromMaybe [] (metaTags pm)) :: PostMetasGroup Tag)
      tagsFound = HashMap.keys hashMap
  -- Only render tag indexes if there are any tags to render.
  if length tagsFound > 0
     then do
      traverse_ (\x -> renderAll config (createIndexModel $ getPostMetasGroupPair ppmg x :: SpecificTagIndex)) tagsFound
      let mainTagIndex = createIndexModel (preparePostsOnlyFromPairs filePathFrontMatter) :: MainTagIndex
      renderAll config mainTagIndex
     else pure ()


-- FIXME: pass the menu suffix here (file extension)
-- UH OH NOT USING SPECIFIC SUFFIXES FOR MENUS VS REGULAR FILES RESULTS IN THIS HEADACHE!
-- | A useful tool in making phlog indexes: create a nice link for the menu
-- using a supplied pair from PostMetas.
makeLocalLink :: PostMeta -> String
makeLocalLink postMeta
  -- FIXME: hardcoded "menu"
  | ".menu" `isSuffixOf` (fst . splitExtension $ path) = restOfLink "1"
  -- Otherwise we are very lazily just going to assume it's a text file. This
  -- will be improved in the future.
  | otherwise = restOfLink "0"
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

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

import           Data.Monoid                    ( mappend )
import           Hakyll
import           Control.Monad
import           Data.List                      ( intercalate )
import           Data.Time.Clock                ( UTCTime(..) )
import           Data.Time.Format               ( formatTime )
import qualified Data.Time.Format              as TF
import           Data.Time.Locale.Compat        ( TimeLocale
                                                , defaultTimeLocale
                                                )
import           Hakyll.Core.Util.String        ( splitAll )
import           System.FilePath                ( takeBaseName
                                                , splitDirectories
                                                )
import           Control.Monad                  ( liftM )
import           Control.Monad.Fail             ( MonadFail )
import           Data.List                      ( intersperse
                                                , sortBy
                                                )
import           Data.Ord                       ( comparing )
import           Data.Time.Locale.Compat        ( defaultTimeLocale )
import qualified Data.Text                     as T
import           Slug                           ( toSlug )
import           Data.Maybe                     ( fromMaybe )
-- Alternatively, when the metadata has a field called @path@ in a
-- @folder/yyyy-mm-dd-title.extension@ format (the convention for pages)
-- and no @published@ metadata field set, this function can render
-- the date. This pattern matches the file name or directory names
-- that begins with @yyyy-mm-dd@ . For example:
-- @folder//yyyy-mm-dd-title//dist//main.extension@ .
-- In case of multiple matches, the rightmost one is used.
dateFieldArchive
    :: String     -- ^ Key in which the rendered date should be placed
    -> String     -- ^ Format to use on the date
    -> Context a  -- ^ Resulting context
dateFieldArchive = dateFieldArchiveWith defaultTimeLocale


--------------------------------------------------------------------------------
-- | This is an extended version of 'dateFieldArchive' that allows you to
-- specify a time locale that is used for outputting the date. For more
-- details, see 'dateFieldArchive'.
dateFieldArchiveWith
    :: TimeLocale  -- ^ Output time locale
    -> String      -- ^ Destination key
    -> String      -- ^ Format to use on the date
    -> Context a   -- ^ Resulting context
dateFieldArchiveWith locale key format = field key $ \i -> do
    time <- getArchiveItemUTC locale $ itemIdentifier i
    return $ formatTime locale format time


--------------------------------------------------------------------------------
-- | Parser to try to extract and parse the time from the @published@
-- field or from the filename. See 'dateFieldArchive' for more information.
-- Exported for user convenience.
getArchiveItemUTC
    :: (MonadMetadata m, MonadFail m)
    => TimeLocale        -- ^ Output time locale
    -> Identifier        -- ^ Input page
    -> m UTCTime         -- ^ Parsed UTCTime
getArchiveItemUTC locale id' = do
    metadata <- getMetadata id'
    let tryField k fmt = lookupString k metadata >>= parseTime' fmt
        paths = splitDirectories $ toFilePath id'
    maybe empty' return
        $  msum
        $  [ tryField "published" fmt | fmt <- formats ]
        ++ [ tryField "date" fmt | fmt <- formats ]
        ++ [parseTime' "%Y%m%d%H%M" (takeBaseName (last paths))]
  where
    empty' =
        fail
            $  "Hakyll.Web.Template.Context.getArchiveItemUTC: "
            ++ "could not parse time for "
            ++ show (takeBaseName (last (splitDirectories $ toFilePath id')))
    parseTime' a = parseTimeM True locale a where x = print a
    formats =
        [ "%a, %d %b %Y %H:%M:%S %Z"
        , "%Y%m%d%H%M"
        , "%Y-%m-%dT%H:%M:%S%Z"
        , "%Y-%m-%d %H:%M:%S%Z"
        , "%Y-%m-%d"
        , "%B %e, %Y %l:%M %p"
        , "%B %e, %Y"
        , "%b %d, %Y"
        ]
parseTimeM :: Bool -> TimeLocale -> String -> String -> Maybe UTCTime
parseTimeM = TF.parseTimeM


--------------------------------------------------------------------------------
-- | Sort pages chronologically. Uses the same method as 'dateField' for
-- extracting the date.
chronologicalArchive :: (MonadMetadata m, MonadFail m) => [Item a] -> m [Item a]
chronologicalArchive =
    sortByM $ getArchiveItemUTC defaultTimeLocale . itemIdentifier
  where
    sortByM :: (Monad m, Ord k) => (a -> m k) -> [a] -> m [a]
    sortByM f xs = liftM (map fst . sortBy (comparing snd))
        $ mapM (\x -> liftM (x, ) (f x)) xs


--------------------------------------------------------------------------------
-- | The reverse of 'chronological'
recentFirstArchive :: (MonadMetadata m, MonadFail m) => [Item a] -> m [Item a]
recentFirstArchive = liftM reverse . chronologicalArchive

--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match "images/*" $ do
        route idRoute
        compile copyFileCompiler

    match "fonts/**/*" $ do
        route idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route idRoute
        compile compressCssCompiler

    match (fromList ["about.markdown", "contact.markdown"]) $ do
        route $ setExtension "html"
        compile
            $   pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "posts/*" $ do
        let ctx = constField "type" "article" <> postCtx
        route $ metadataRoute titleRoute
        compile
            $   pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    ctx
            >>= loadAndApplyTemplate "templates/default.html" ctx
            >>= relativizeUrls

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirstArchive =<< loadAll "posts/*"
            let archiveCtx =
                    listField "posts" postCtx (return posts)
                        `mappend` constField "title" "Archives"
                        `mappend` defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls


    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirstArchive =<< loadAll "posts/*"
            let indexCtx =
                    listField "posts" postCtx (return (take 10 posts))
                        `mappend` defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx = dateFieldArchive "date" "%B %e, %Y" `mappend` defaultContext


getTitleFromMeta :: Metadata -> String
getTitleFromMeta = fromMaybe "no title" . lookupString "title"


fileNameFromTitle :: Metadata -> FilePath
fileNameFromTitle =
    T.unpack . (`T.append` ".html") . toSlug . T.pack . getTitleFromMeta


titleRoute :: Metadata -> Routes
titleRoute = constRoute . fileNameFromTitle

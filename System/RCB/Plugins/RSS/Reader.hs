-------------------------------------------------------------------------------
-- | Module      :  System.RCB.Plugins.RSS.Reader
--   Copyright   :  (c) Matthias Brettschneider 2019
--   License     :  as-is
--
--   Maintainer  :  frosch03@gmail.com
--   Stability   :  unstable
--   Portability :  unportable
--
-------------------------------------------------------------------------------

module System.RCB.Plugins.RSS.Reader
where

import System.RCB.Plugins.RSS.RssConfig.FeedDescriptor
import System.RCB.Plugins.RSS.RssConfig.FeedTransformer
import System.RCB.Plugins.RSS.ITransformable

import Data.List                (isPrefixOf)
import Data.Maybe               (catMaybes, maybeToList)
import Network.HTTP             (getResponseBody, simpleHTTP, getRequest)
import Network.Http.Client      (get, concatHandler, URL(..))
import Text.HTML.TagSoup        (Tag(..), Attribute, parseTags, sections)
import Text.HTML.TagSoup.Entity (lookupEntity, htmlEntities)
import Data.ByteString.UTF8 as BSU (toString, fromString)

type Date = String -- ^ Publish date

-- | Defines an rss feed item
data RssItem = RssItem
    { pubDate     :: Date
    , title       :: String
    , link        :: String
    , description :: String
    } deriving Show

-- | Transform the RssItem into a string that follows the rocket chat
-- format
rss2string :: FeedTransformer -> RssItem -> String
rss2string ftr itm =
    "[" ++ (tfunc ftr t) ++ "](" ++ (tfunc ftr l) ++ "): " ++ (dfunc ftr d)
    where
      t = title       itm
      l = link        itm
      d = description itm

-- | Read one feed into a list of rssitems
readFeed :: Int -> String -> IO [RssItem]
readFeed n url = do
    let urlBS = BSU.fromString url
    c <- (get urlBS concatHandler) >>= (\x -> return $ BSU.toString x)
    let items = getChildren "item" [] $ parseTags c
    return . limitItems n . catMaybes $ map readItem items

    where
        -- 0 means unlimited
        limitItems 0 = id
        limitItems n = take n

        -- read one item tag
        readItem i = let title       = fix $ getText "title"       [] i
                         link        = fix $ getText "link"        [] i
                         pubDate     = fix $ getText "pubDate"     [] i
                         description = fix $ getText "description" [] i
                     -- required parts to be usable
                     in if all (/= "") [title, link, description]
                            then Just $ RssItem pubDate title link description
                            else Nothing

        fix = fixWhiteSpace . onlyPrintables . replaceEntities


-- | Replace all html entities with their referenced values
replaceEntities :: String -> String
replaceEntities = replaceAll (namedList ++ numericList) maybeLookup

    where
        -- add the extra characters so the replaceAll works
        namedList   = map ((("&" ++) . (++";")) . fst) htmlEntities
        numericList = map ((("&#"++) . (++";")) . show) [1..255]

        maybeLookup :: String -> String
        maybeLookup = head . maybeToList . lookupEntity . dropAmp . reverse . dropSemi . reverse

            where
                -- strip characters so the lookup works
                dropAmp ('&':xs) = xs
                dropAmp xs       = xs

                dropSemi (';':xs) = xs
                dropSemi xs       = xs

-- Returns list of child tags given a parent node
getChildren :: String -> [Attribute String] -> [Tag String]-> [[Tag String]]
getChildren t a = map (drop 1 . takeWhile (/= TagClose t)) . sections (== TagOpen t a)

-- Returns all innertext of a given tag
getText :: String -> [Attribute String] -> [Tag String] -> String
getText t a = concatMap getText' . takeWhile (/= TagClose t) . drop 1 . dropWhile (/= TagOpen t a)

    where

        getText' (TagText s) = s
        getText' _           = []

-- Some useful formatting utils {{{
--
-- | an infix operator that concatenates two strings with a space in
--   between. this is useful with for example:
--
-- > fields = ["Name:", "Age:"]
-- > values = ["Pat", "25"]
-- > data   = zipWith (+-+) fields values
--
(+-+) :: String -> String -> String
s1 +-+ s2 = s1 ++ " " ++ s2
infixr 5 +-+

-- | removes tab, newline, and return (replacing them with space), then
--   compresses duplicate spaces, then trims spaces from the front and
--   back
fixWhiteSpace :: String -> String
fixWhiteSpace = trim . compressSpace

-- | strip all but printable characters
onlyPrintables :: String -> String
onlyPrintables [] = []
onlyPrintables (x:xs) =
    if x `elem` [' '..'~']
        then x : onlyPrintables xs
        else     onlyPrintables xs

-- | trim any spaces from the front and back of the input string
trim :: String -> String
trim = f . f
    where
        f = reverse . dropSpace

dropSpace :: String -> String
dropSpace = dropWhile (== ' ')

-- | compresses 1 or more spaces into a single space
compressSpace :: String -> String
compressSpace []       = []
compressSpace (' ':xs) = ' ' : compressSpace (dropSpace xs)
compressSpace (x:xs)   = x   : compressSpace xs

-- | remove any text appearing between c1 and c2 (inclusive)
stripBetween :: Char -> Char -> String -> String
stripBetween _  _   []     = []
stripBetween c1 c2  (x:xs) = 
    if x == c1
        then stripBetween c1 c2 . drop 1 $ dropWhile (/= c2) xs
        else x : stripBetween c1 c2 xs

-- -- | given a width and (possibly infinite) list, it will work out a list
-- --   of shorter lists that, when viewed one by one successively, appear
-- --   as the original list rolling by right to left (like a stock ticker)
-- tickerText :: Int -> [a] -> [[a]]
-- tickerText _ []     = []
-- tickerText w (x:xs) = (x : take (w - 1) xs) : tickerText w xs

-- | works with [a], but designed for strings. it's best explained by an
--   example:
--
-- > scrubFile = do
-- >   dirty <- readFile "myfile"
-- >   putStrLn $ replaceAll ["my@email.com","mypassword"] (\_ -> "") dirty
--
replaceAll :: (Eq a) 
           => [[a]]        -- ^ the list of all /from/s that need replacing
           -> ([a] -> [a]) -- ^ a function, how to get the /to/ given any /from/
           -> [a]          -- ^ the input string
           -> [a]          -- ^ result
replaceAll _      _ [] = []
replaceAll []     _ s  = s
replaceAll (x:xs) f s  = replace x (f x) $ replaceAll xs f s

-- | taken from http://bluebones.net/2007/01/replace-in-haskell/
replace :: (Eq a) => [a] -> [a] -> [a] -> [a]
replace _    _  [] = []
replace _    [] s  = s
replace []   _  s  = s
replace from to s@(x:xs) =
    if from `isPrefixOf` s
        then to ++ replace from to (drop (length from) s)
        else x : replace from to xs

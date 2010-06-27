
module Main where

import Control.Monad
import Data.Either
import Data.List hiding (find)
import Data.Maybe
import Network.MPD
import Text.XML.HXT.Arrow

toString :: [Song] -> IO (Maybe String)
toString xs = fmap listToMaybe $ runX (toXML xs >>> writeDocumentToString [(a_indent, v_1)])

toXML :: (ArrowXml a) => [Song] -> a XmlTree XmlTree
toXML = root [] . (: []) . mkelem "library" [] . map artistToXML . organize . map addKey

type Key = (String, (String, (Int, String)))

type KSong = (Key, Song)

addKey :: Song -> KSong
addKey song = ((sgArtist song, (sgAlbum song, (fst $ sgTrack song, sgTitle song))), song)

k0 :: KSong -> String
k0 = fst . fst

k1 :: KSong -> String
k1 = fst . snd . fst

k2 :: KSong -> (Int, String)
k2 = snd . snd . fst

eq :: (Eq a) => (b -> a) -> b -> b -> Bool
eq k a b = (k a) == (k b)

cmp :: (Ord a) => (b -> a) -> b -> b -> Ordering
cmp k a b = compare (k a) (k b)

groupSort :: (Ord a) => (b -> a) -> [b] -> [[b]]
groupSort k = groupBy (eq k) . sortBy (cmp k)

organize :: [KSong] -> [[[KSong]]]
organize = map (map $ sortBy $ cmp k2) . map (groupSort k1) . groupSort k0

artistToXML :: (ArrowXml a) => [[KSong]] -> a XmlTree XmlTree
artistToXML xs@((song : _) : _) = mkelem "artist" [sattr "name" $ k0 song] $ map albumToXML xs
artistToXML _ = txt ""

albumToXML :: (ArrowXml a) => [KSong] -> a XmlTree XmlTree
albumToXML xs@(song : _) = mkelem "album" [sattr "name" $ k1 song] $ map songToXML xs

songToXML :: (ArrowXml a) => KSong -> a XmlTree XmlTree
songToXML (_, song) = mkelem "song" (map (\(a, f) -> sattr a $ f song) attrs) []

attrs :: [(String, Song -> String)]
attrs = [
  ("title", sgTitle),
  ("file", sgFilePath),
  ("genre", sgGenre),
  ("length", show . sgLength),
  ("date", show . sgDate),
  ("track", show . fst . sgTrack),
  ("disc", maybe "" (show . fst) . sgDisc)]

main :: IO ()
main = (withMPD $ listAllInfo "/" >>= return . rights)
       >>=
       either print (toString >=> putStr . maybe "Nothing" id)


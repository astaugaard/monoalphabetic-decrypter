module Main where

import Data.Char
import Debug.Trace
import Data.List
import Data.Maybe (fromMaybe)
import Data.Map (Map)
import qualified Data.Map as M
import System.Random (StdGen, initStdGen)
import qualified System.Random as R

split :: Char -> String -> [String]
split c [] = []
split c s =
  let uptoChar = takeWhile (/= c) s
      after = dropWhile (/= c) s
   in uptoChar : split c (drop 1 after)

convertToMap :: String -> Map String Float
convertToMap s =
  let lines = split '\n' s
      wordsInLines = map (split ' ') lines

      convertLine [w, v] = (map toLower w, read v)
      convertLine _ = error "invalid map"

      totalSum = sum $ map snd mapList
      mapList = map convertLine wordsInLines

      adjusted = map (\(s,v) -> (s, log (fromInteger v/fromInteger totalSum))) mapList

   in M.fromList adjusted


frequencies :: String -> Map Char Integer
frequencies [] = M.empty
frequencies (c : str) =
  let rest = frequencies str
   in case M.lookup c rest of
        Just a -> M.insert c (a + 1) rest
        Nothing -> M.insert c 1 rest

letterFrequency = "etaoinsrhldcumfpgwybvkxjqz"

improve :: Integer -> Map String Float -> Map Char Char -> String -> StdGen -> Map Char Char
improve c freq m s r
  | c > 0 =
    let (a, g') = improveOnce freq m s r
     in improve (c - 1) freq a s g'
  | otherwise = m

improveOnce :: Map String Float -> Map Char Char -> String -> StdGen -> (Map Char Char, StdGen)
improveOnce freq map s g =
  let (c1, g') = R.uniformR ('a', 'z') g
      (c2, g'') = R.uniformR ('a', 'z') g'
      vc1 = map M.! c1
      vc2 = map M.! c2
      swaped = M.insert c1 vc2 $ M.insert c2 vc1 map
   in if isBetter freq swaped map s then (swaped, g'') else (map, g'')

isBetter :: Map String Float -> Map Char Char -> Map Char Char -> String -> Bool
isBetter freq m1 m2 s = isBetter' freq m1 s > isBetter' freq m2 s

isBetter' :: Map String Float -> Map Char Char -> String -> Float
isBetter' f m s =
  let substed = subst m s
      quadgrams = chunksSized 4 substed
   in let a = sum $
                map
                  ( \q -> case M.lookup q f of
                      Nothing -> -24
                      Just a ->  a
                  )
                  quadgrams
      in a


subst :: Map Char Char -> String -> String
subst map = foldr (\c -> (:) (map M.! c)) ""

subst2 :: Map Char Char -> String -> String
subst2 map =
  foldr
    ( \c ->
        (:)
          ( fromMaybe c (M.lookup (toLower c) map)
          )
    )
    ""

chunksSized :: Int -> [a] -> [[a]]
chunksSized c as = filter (\l -> length l == c) $ map (take c) $ tails as

padWithZeros :: Map Char Integer -> Map Char Integer
padWithZeros f = pad' f ['a' .. 'z']
  where
    pad' :: Map Char Integer -> [Char] -> Map Char Integer
    pad' m (c : cs) = flip pad' cs $ M.insertWith (\_ o -> o) c 0 m
    pad' m [] = m

main :: IO ()
main = do
  englishGrams <- readFile "english-quadgrams.txt"
  let enMap = convertToMap englishGrams
  putStrLn "file to decrypt"
  file <- getLine

  fileCont <- readFile file
  let stripedFile = filter isAlpha $ map toLower fileCont
  let freq = sortBy (\(_, a) (_, b) -> b `compare` a) $ M.toList $ padWithZeros $ frequencies stripedFile


  -- I can't spell
  let inialMap = M.fromList $ zip (map fst freq) letterFrequency

  print inialMap

  generator <- initStdGen
  let finalMap = improve 2000 enMap inialMap stripedFile generator

  putStrLn $ subst2 finalMap fileCont

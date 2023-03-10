{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import GHC.Base
import Text.Show (show)
import System.IO (putStrLn)
import Rhythmicity.MarkerSeqs hiding (id) 
import Rhythmicity.BasicF 
import Data.List 
import Data.Tuple (fst)
import Phladiprelio.General.PrepareText 
import Phladiprelio.General.Syllables 
import Phladiprelio.General.Base
import System.Environment (getArgs)

generalF 
 :: GWritingSystemPRPLX -- ^ Data used to obtain the phonetic language representation of the text.
 -> [(Char,Char)] -- ^ The pairs of the 'Char' that corresponds to the similar phonetic languages consonant phenomenon (e. g. allophones). Must be sorted in the ascending order to be used correctly. 
 -> CharPhoneticClassification
 -> SegmentRulesG
 -> String -- ^ Corresponds to the 100 delimiter in the @ukrainian-phonetics-basic-array@ package.
 -> String -- ^ Corresponds to the 101 delimiter in the @ukrainian-phonetics-basic-array@ package.
 -> ([[[PRS]]] -> [[Double]])
 -> [String] 
 -> IO [()] 
generalF wrs ks arr gs us vs h = mapM (\(x,y) -> putStrLn (show x `mappend` (' ':y)))  . sortOn id . map ((\xss -> (f xss, xss)) . unwords) . permutations
               where f = sum . countHashesG (H [1,1..] 3) 4 [4,3] . mconcat . h . createSyllablesPL wrs ks arr gs us vs

main = putStrLn "Hello, World!" 

processingF
 :: GWritingSystemPRPLX -- ^ Data used to obtain the phonetic language representation of the text.
 -> [(Char,Char)] -- ^ The pairs of the 'Char' that corresponds to the similar phonetic languages consonant phenomenon (e. g. allophones). Must be sorted in the ascending order to be used correctly. 
 -> CharPhoneticClassification
 -> SegmentRulesG
 -> String -- ^ Corresponds to the 100 delimiter in the @ukrainian-phonetics-basic-array@ package.
 -> String -- ^ Corresponds to the 101 delimiter in the @ukrainian-phonetics-basic-array@ package.
 -> ([[[PRS]]] -> [[Double]])
 -> [[String]] 
 -> [[String]] 
 -> String 
 -> IO ()
processingF wrs ks arr gs us vs h ysss zsss xs = do
  args <- getArgs
  let str1 = take 7 . words . mconcat . prepareText ysss zsss xs . unwords $ args
  generalF wrs ks arr gs us vs h str1 >> return ()


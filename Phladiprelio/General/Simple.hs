{-# LANGUAGE NoImplicitPrelude, BangPatterns #-}

module Phladiprelio.General.Simple where

import GHC.Base
import GHC.Enum (fromEnum,toEnum)
import GHC.Real (Integral,fromIntegral,(/),quot,rem,quotRem,round,gcd,(^))
import Text.Show (Show(..))
import Phladiprelio.General.PrepareText 
import Phladiprelio.General.Syllables 
import Phladiprelio.General.Base
import System.Environment (getArgs)
import GHC.Num (Num,(+),(-),(*),Integer)
import Text.Read (readMaybe)
import System.IO (putStrLn, FilePath,stdout,universalNewlineMode,hSetNewlineMode,getLine,appendFile,readFile,writeFile)
import Rhythmicity.MarkerSeqs hiding (id) 
import Rhythmicity.BasicF 
import Data.List hiding (foldr)
import Data.Maybe (fromMaybe, mapMaybe, catMaybes,isNothing,fromJust) 
import Data.Tuple (fst,snd)
import Data.Char (isDigit,isAlpha,isSpace)
import CLI.Arguments
import CLI.Arguments.Get
import CLI.Arguments.Parsing
import GHC.Int (Int8)
import Data.Ord (comparing)
import Phladiprelio.PermutationsRepresent
import Phladiprelio.ConstraintsEncoded
import Phladiprelio.PermutationsArr
import Phladiprelio.StrictVG
import Numeric (showFFloat)
import Phladiprelio.Halfsplit
import System.Directory (doesFileExist,readable,writable,getPermissions,Permissions(..),doesFileExist,getCurrentDirectory)
import Data.ReversedScientific
import Control.Concurrent.Async (mapConcurrently)
import Data.MinMax1 (minMax11By) 
import Phladiprelio.Tests
import Phladiprelio.General.Datatype3
import Phladiprelio.General.Distance
import Phladiprelio.UniquenessPeriodsG

generalF 
 :: Int -- ^ A power of 10. 10 in this power is then multiplied the value of distance if the next ['Double'] argument is not empty. The default one is 4. The proper values are in the range [2..6].
 -> Int -- ^ A 'length' of the next argument here.
 -> [Double] -- ^ A list of non-negative values normed by 1.0 (the greatest of which is 1.0) that the line options are compared with. If null, then the program works as for version 0.12.1.0 without this newly-introduced argument since the version 0.13.0.0. The length of it must be a least common multiplier of the (number of syllables plus number of \'_digits\' groups) to work correctly. Is not used when the next 'FilePath' and 'String' arguments are not null.
 -> Bool -- ^ If 'True' then adds \"<br>\" to line endings for double column output
 -> FilePath -- ^ A path to the file to save double columns output to. If empty then just prints to 'stdout'.
 -> String -- ^ If not null than instead of rhythmicity evaluation using hashes and and feets, there is computed a diversity property for the specified 'String' here using the 'selectSounds' function. For more information, see: 'https://oleksandr-zhabenko.github.io/uk/rhythmicity/PhLADiPreLiO.Eng.21.html#types'
 -> (String -> String) -- ^ A function that specifies what 'Char's in the list the first argument makes to be the function sensitive to. Analogue of the @g@ function in the definition: https://hackage.haskell.org/package/phonetic-languages-simplified-examples-array-0.21.0.0/docs/src/Phonetic.Languages.Simplified.Array.Ukrainian.FuncRep2RelatedG2.html#selectSounds. Use just small 'Char' if they are letters, do not use \'.\' and spaces.
 -> (String, String)  -- ^ If the next element is not equal to -1, then the prepending and appending lines to be displayed. Used basically for working with the multiline textual input data.
 -> Int -- ^ The number of the line in the file to be read the lines from. If equal to -1 then neither reading from the file is done nor the first argument influences the processment results.
 -> GWritingSystemPRPLX -- ^ Data used to obtain the phonetic language representation of the text.
 -> [(Char,Char)] -- ^ The pairs of the 'Char' that corresponds to the similar phonetic languages consonant phenomenon (e. g. allophones). Must be sorted in the ascending order to be used correctly. 
 -> CharPhoneticClassification
 -> SegmentRulesG
 -> String -- ^ Corresponds to the 100 delimiter in the @ukrainian-phonetics-basic-array@ package.
 -> String -- ^ Corresponds to the 101 delimiter in the @ukrainian-phonetics-basic-array@ package.
 -> ([[[PRS]]] -> [[Double]])
 -> Int
 -> HashCorrections 
 -> (Int8,[Int8])
 -> Bool
 -> Int -- ^ The hashing function step. The default value is 20. Is expected to be greater than 2, and better greater than 12. 
 -> Bool 
 -> Int8
 -> (FilePath, Int)
 -> Bool -- ^ In the testing mode, whether to execute computations in concurrent mode (for speed up) or in single thread. If specified needs the executable to be compiled with -rtsopts and -threaded options and run with the command line +RTS -N -RTS options.
 -> String -- ^ An initial string to be analyzed.
 -> [String] 
 -> IO [String] 
generalF power10 ldc compards html dcfile selStr selFun (prestr,poststr) lineNmb wrs ks arr gs us vs h numTest hc (grps,mxms) descending hashStep emptyline splitting (fs, code) concurrently initstr universalSet 
 | null universalSet = let strOutput = ["You have specified the data and constraints on it that lead to no further possible options.", "Please, specify another data and constraints."] in mapM putStrLn strOutput >> return strOutput
 | length universalSet == 1 = mapM putStrLn universalSet >> return universalSet
 | otherwise = do
   let syllN = countSyll wrs arr us vs initstr
--       universalSet = map unwords . permutations $ rss
       f ldc compards grps mxms 
          | null selStr = (if null compards then (sum . countHashes2G hashStep hc grps mxms) else (round . (*10^power10) . distanceSqrG2 ldc compards)) . read3 (not . null . filter (not . isSpace)) 1.0 (mconcat . h .  createSyllablesPL wrs ks arr gs us vs)
          | otherwise = fromIntegral . diverse2GGL (selectSounds selFun selStr) (us `mappend` vs) . concatMap string1 . stringToXG wrs . filter (\c -> not (isDigit c) && c /= '_' && c /= '=')
   hSetNewlineMode stdout universalNewlineMode
   if numTest >= 0 && numTest <= 179 && numTest /= 1 && null compards then testsOutput concurrently syllN f ldc numTest universalSet 
   else let sRepresent = zipWith (\k (x, ys) -> S k x ys) [1..] . 
             (let h1 = if descending then (\(u,w) -> ((-1)*u,w)) else id in sortOn h1) . map (\xss -> (f ldc compards grps mxms xss, xss)) $ universalSet
            strOutput = (:[]) . halfsplit1G (\(S _ y _) -> y) (if html then "<br>" else "") (jjj splitting) $ sRepresent
                          in do
                             _ <- (if null dcfile then mapM putStrLn strOutput else do {mapM putStrLn strOutput >> doesFileExist dcfile >>= \exist -> if exist then do {getPermissions dcfile >>= \perms -> if writable perms then mapM (writeFile dcfile) strOutput else error $ "Phladiprelio.General.IO.generalF: File " `mappend` dcfile `mappend` " is not writable!"} else do {getCurrentDirectory >>= \currdir -> do {getPermissions currdir >>= \perms -> if writable perms then mapM (writeFile dcfile) strOutput else error $ "Phladiprelio.General.IO.generalF: Directory of the file " `mappend` dcfile `mappend` " is not writable!"}}})
                             let l1 = length sRepresent
                             if code == -1 
                                 then if lineNmb == -1 then return strOutput
                                      else do 
                                          print23 prestr poststr 1 [initstr]
                                          return strOutput
                                 else do 
                                       print23 prestr poststr 1 [initstr]
                                       parseLineNumber l1 >>= \num -> do
                                         permiss <- getPermissions fs
                                         let writ = writable permiss
                                             readab = readable permiss
                                         if writ && readab then outputWithFile h wrs ks arr gs us vs selStr compards sRepresent code grps fs num
                                         else error "The specified file cannot be used for appending the text! Please, specify another file!"
                                         return []
     where jjj kk = let (q1,r1) = quotRem kk (if kk < 0 then -10 else 10) in jjj' q1 r1 emptyline
           jjj' q1 r1 emptyline
             | r1 == (-1) || r1 == (-3) = -10*q1 + (if emptyline then -5 else r1)
             | r1 == 1 || r1 == 3 = 10*q1 + (if emptyline then 5 else r1)
             | r1 < 0 = -10*q1 + (if emptyline then -4 else r1)
             | otherwise = 10*q1 + (if emptyline then 4 else r1)

data PhladiprelioGen = S Int Integer String deriving Eq

instance Show PhladiprelioGen where
  show (S i j xs) = showBignum 7 j `mappend` " " `mappend` xs `mappend` "  " `mappend` showWithSpaces 4 i

countSyll 
  :: GWritingSystemPRPLX -- ^ Data used to obtain the phonetic language representation of the text.
  -> CharPhoneticClassification 
  ->  String -- ^ Corresponds to the 100 delimiter in the @ukrainian-phonetics-basic-array@ package.
  -> String -- ^ Corresponds to the 101 delimiter in the @ukrainian-phonetics-basic-array@ package.
  -> String 
  -> Int
countSyll wrs arr us vs xs = numUnderscoresSyll + (fromEnum . foldr (\x y -> if createsSyllable x then y + 1 else y) 0 . concatMap (str2PRSs arr) . words1 . mapMaybe g . concatMap string1 . stringToXG wrs $ xs)
   where numUnderscoresSyll = length . filter (\xs -> let (ys,ts) = splitAt 1 xs in ys == "_" && all isDigit ts && not (null ts)) . groupBy (\x y -> x=='_' && isDigit y) $ xs
         g :: Char -> Maybe Char
         g x
          | x `elem` us = Nothing
          | x `notElem` vs = Just x
          | otherwise = Just ' '
         words1 xs = if null ts then [] else w : words1 s'' -- Practically this is an optimized version for this case 'words' function from Prelude.
           where ts = dropWhile (== ' ') xs
                 (w, s'') = break (== ' ') ts
         {-# NOINLINE words1 #-}

stat1 :: Int -> (Int8,[Int8]) -> Int
stat1 n (k, ks) = fst (n `quotRemInt` fromEnum k) * length ks

outputSel :: PhladiprelioGen -> Int -> String
outputSel (S x1 y1 ts) code
  | code < 0 = []
  | code == 1 || code == 11 || code == 16 = intercalate " " [show x1, ts] `mappend` "\n"
  | code == 2 || code == 12 || code == 17 = intercalate " " [show y1, ts] `mappend` "\n"
  | code == 3 || code == 13 || code == 18 = intercalate " " [show x1, ts, show y1] `mappend` "\n"
  | code == 4 || code == 14 || code == 19 = intercalate " " [show x1, show y1] `mappend` "\n"
  | otherwise = ts `mappend` "\n"

parseLineNumber :: Int -> IO Int
parseLineNumber l1 = do 
  putStrLn "Please, specify the number of the option to be written to the file specified: "
  number <- getLine
  let num = readMaybe (filter isDigit number)::Maybe Int
  if isNothing num || num > Just l1 || num == Just 0 
      then parseLineNumber l1 
      else return . fromJust $ num

{-| Uses 'getArgs' inside to get the needed data from the command line arguments. Use with this in
 mind. 
-}
argsProcessing
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
 -> IO (Int, Int, [Double], Bool, FilePath, String, String, String, Int, Bool, Int8, FilePath, Int, Bool, String, [String]) -- ^ These ones are intended to be used inside 'generalF'.
argsProcessing wrs ks arr gs us vs h ysss zsss xs = do
  args0 <- getArgs
  let (argsC, args) = takeCs1R ('+','-') cSpecs args0
      (argsB, args11) = takeBsR bSpecs args
      prepare = any (== "-p") args11
      emptyline = any (== "+l") args11 
      splitting = fromMaybe 50 (readMaybe (concat . getB "+w" $ argsB)::Maybe Int8) 
      concurrently = any (== "-C") args11
      dcspecs = getB "+dc" argsB
      (html,dcfile) 
        | null dcspecs = (False, "")
        | otherwise = (head dcspecs == "1",last dcspecs)
      selStr = concat . getB "+ul" $ argsB
      filedata = getB "+f" argsB
      power10' = fromMaybe 4 (readMaybe (concat . getB "+q" $ argsB)::Maybe Int)
      power10 
         | power10' < 2 && power10' > 6 = 4
         | otherwise = power10'
      (multiline2, multiline2LineNum)
        | oneB "+m3" argsB =
            let r1ss = getB "+m3" argsB in
                  if length r1ss == 3
                      then let (kss,qss) = splitAt 2 r1ss in
                                   (kss, max 1 (fromMaybe 1 (readMaybe (concat qss)::Maybe Int)))
                      else (r1ss, 1)
        | oneB "+m2" argsB = (getB "+m" argsB,  max 1 (fromMaybe 1 (readMaybe (concat . getB "+m2" $ argsB)::Maybe Int)))
        | otherwise = (getB "+m" argsB, -1)
      (fileread,lineNmb)
        | null multiline2 = ("",-1)
        | length multiline2 == 2 = (head multiline2, fromMaybe 1 (readMaybe (last multiline2)::Maybe Int))
        | otherwise = (head multiline2, 1)
  (arg3s,prestr,poststr,linecomp3) <- do
       if lineNmb /= -1 then do
           txtFromFile <- readFile fileread
           let lns = lines txtFromFile
               ll1 = length lns
               ln0 = max 1 (min lineNmb (length lns))
               lm3
                 | multiline2LineNum < 1 = -1
                 | otherwise = max 1 . min multiline2LineNum $ ll1
               linecomp3
                 | lm3 == -1 = []
                 | otherwise = lns !! (lm3 - 1)
               ln_1 
                  | ln0 == 1 = 0
                  | otherwise = ln0 - 1
               ln1
                  | ln0 == length lns = 0
                  | otherwise = ln0 + 1
               lineF = lns !! (ln0 - 1)
               line_1F 
                  | ln_1 == 0 = []
                  | otherwise = lns !! (ln_1 - 1)
               line1F
                  | ln1 == 0 = []
                  | otherwise = lns !! (ln1 - 1)
           return $ (words lineF, line_1F,line1F,linecomp3)
       else return (args11, [], [],[])
  let line2comparewith
        | oneC "+l2" argsC || null linecomp3 = unwords . getC "+l2" $ argsC
        | otherwise = linecomp3
      basecomp = read3 (not . null . filter (not . isSpace)) 1.0 (mconcat . h . createSyllablesPL wrs ks arr gs us vs) line2comparewith
      (filesave,codesave)
        | null filedata = ("",-1)
        | length filedata == 2 = (head filedata, fromMaybe 0 (readMaybe (last filedata)::Maybe Int))
        | otherwise = (head filedata,0)
      ll = let maxWordsNum = (if any (== "+x") arg3s then 9 else 7) in take maxWordsNum . (if prepare then id else words . mconcat . prepareTextN maxWordsNum ysss zsss xs . unwords) $ arg3s
      l = length ll
      argCs = catMaybes (fmap (readMaybeECG l) . getC "+a" $ argsC)
      argCBs = unwords . getC "+b" $ argsC -- If you use the parenthese with +b ... -b then consider also using the quotation marks for the whole algebraic constraint. At the moment though it is still not working properly for parentheses functionality. The issue should be fixed in the further releases.
      !perms 
        | not (null argCBs) = filterGeneralConv l argCBs . genPermutationsL $ l
        | null argCs = genPermutationsL l
        | otherwise = decodeLConstraints argCs . genPermutationsL $ l 
      basiclineoption = unwords arg3s
      example = read3 (not . null . filter (not . isSpace)) 1.0 (mconcat . h .  createSyllablesPL wrs ks arr gs us vs) (unwords arg3s)
      le = length example
      lb = length basecomp
      gcd1 = gcd le lb
      ldc = le * lb `quot` gcd1
      mulp = ldc `quot` lb
      max2 = maximum basecomp
      compards = concatMap (replicate mulp . (/ max2)) basecomp
      variants1 = uniquenessVariants2GNBL ' ' id id id perms ll
  return (power10, ldc, compards, html, dcfile, selStr, prestr, poststr, lineNmb, emptyline, splitting, filesave, codesave, concurrently, basiclineoption, variants1)

processingF
 :: (String -> String) -- ^ A function that specifies what 'Char's in the list the first argument makes to be the function sensitive to. Analogue of the @g@ function in the definition: https://hackage.haskell.org/package/phonetic-languages-simplified-examples-array-0.21.0.0/docs/src/Phonetic.Languages.Simplified.Array.Ukrainian.FuncRep2RelatedG2.html#parsey0Choice. Use just small 'Char' if they are letters, do not use \'.\' and spaces.
 -> GWritingSystemPRPLX -- ^ Data used to obtain the phonetic language representation of the text.
 -> [(Char,Char)] -- ^ The pairs of the 'Char' that corresponds to the similar phonetic languages consonant phenomenon (e. g. allophones). Must be sorted in the ascending order to be used correctly. 
 -> CharPhoneticClassification
 -> SegmentRulesG
 -> String -- ^ Corresponds to the 100 delimiter in the @ukrainian-phonetics-basic-array@ package.
 -> String -- ^ Corresponds to the 101 delimiter in the @ukrainian-phonetics-basic-array@ package.
 -> ([[[PRS]]] -> [[Double]])
 -> Int
 -> HashCorrections 
 -> (Int8,[Int8]) 
 -> [[String]] 
 -> [[String]] 
 -> Bool
 -> Int -- ^ The hashing function step. The default value is 20. Is expected to be greater than 2, and better greater than 12. 
 -> String 
 -> IO ()
processingF selFun wrs ks arr gs us vs h numTest hc (grps,mxms) ysss zsss descending hashStep xs = argsProcessing wrs ks arr gs us vs h ysss zsss xs >>= \(power10, ldc, compards, html, dcfile, selStr, prestr, poststr, lineNmb, emptyline, splitting, filesave, codesave, concurrently, basiclineoption, variants1) -> generalF power10 ldc compards html dcfile selStr selFun (prestr,poststr) lineNmb wrs ks arr gs us vs h numTest hc (grps,mxms) descending hashStep emptyline splitting (filesave, codesave) concurrently basiclineoption variants1 >> return ()
{-# INLINE processingF #-}

-- | Specifies the group of the command line arguments for 'processingF', which specifies the
-- PhLADiPreLiO constraints. For more information, see:
-- https://oleksandr-zhabenko.github.io/uk/rhythmicity/PhLADiPreLiO.Eng.21.html#constraints 
cSpecs :: CLSpecifications
cSpecs = zip ["+a","+b","+l2"] . cycle $ [-1]

bSpecs :: CLSpecifications
bSpecs = [("+f",2),("+m",2),("+m2",2),("+m3",3),("+ul",1),("+w",1),("+dc",2),("+q",1)]

{-| 'selectSounds' converts the argument after \"+ul\" command line argument into a list of sound representations that is used for evaluation of \'uniqueness periods\' properties of the line. Is a modified Phonetic.Languages.Simplified.Array.General.FuncRep2RelatedG2.parsey0Choice from the @phonetic-languages-simplified-generalized-examples-array-0.19.0.1@ package.
 -}
selectSounds 
  :: (String -> String) -- ^ A function that specifies what 'Char's in the list the first argument makes to be the function sensitive to. Analogue of the @g@ function in the definition: https://hackage.haskell.org/package/phonetic-languages-simplified-examples-array-0.21.0.0/docs/src/Phonetic.Languages.Simplified.Array.Ukrainian.FuncRep2RelatedG2.html#selectSounds. Use just small 'Char' if they are letters, do not use \'.\' and spaces.
  -> String 
  -> String
selectSounds g xs = f . sort . concatMap g . words . map (\c -> if c  == '.' then ' ' else c) $ us
    where (ts,us) = break (== '.') . filter (\c -> c /= 'H' && c /= 'G') $ xs
          f (x:ts@(y:xs)) 
           | x == y = f ts
           | otherwise = x:f ts
          f xs = xs

-- | Internal part of the 'generalF' for processment in case of using tests mode.
testsOutput
  :: (Show a1, Integral a1) =>
     Bool
     -> Int
     -> (p -> [a2] -> Int8 -> [Int8] -> String -> a1)
     -> p
     -> Int
     -> [String]
     -> IO [String]
testsOutput concurrently syllN f ldc numTest universalSet = do
      putStrLn "Feet   Val  Stat   Proxim" 
      (if concurrently then mapConcurrently else mapM) 
           (\(q,qs) -> let m = stat1 syllN (q,qs)
                           (min1,max1) = fromJust . minMax11By (comparing (f ldc [] q qs)) $ universalSet 
                           mx = f ldc [] q qs max1
                           strTest = (show (fromEnum q) `mappend` "   |   " `mappend`  show mx `mappend` "     " `mappend` show m `mappend` "  -> " `mappend` showFFloat (Just 3) (100 * fromIntegral mx / fromIntegral m) "%" `mappend` (if rem numTest 10 >= 4 
                                                               then -- let min1 = minimumBy (comparing (f ldc [] q qs)) universalSet in 
                                                                     ("\n" `mappend` min1 `mappend` "\n" `mappend` max1 `mappend` "\n")  
                                                               else "")) in putStrLn strTest >> return strTest) . zip (sel2 numTest) $ (sel numTest)

-- | Internal part of the 'generalF' for processment with a file.
outputWithFile
  :: (Eq a1, Num a1) =>
     ([[[PRS]]] -> [[Double]])
     -> GWritingSystemPRPLX -- ^ Data used to obtain the phonetic language representation of the text.
     -> [(Char, Char)] -- ^ The pairs of the 'Char' that corresponds to the similar phonetic languages consonant phenomenon (e. g. allophones). Must be sorted in the ascending order to be used correctly. 
     -> CharPhoneticClassification
     -> SegmentRulesG
     -> String -- ^ Corresponds to the 100 delimiter in the @ukrainian-phonetics-basic-array@ package.
     -> String -- ^ Corresponds to the 101 delimiter in the @ukrainian-phonetics-basic-array@ package.
     -> String -- ^ If not null than instead of rhythmicity evaluation using hashes and and feets, there is computed a diversity property for the specified 'String' here using the 'selectSounds' function. For more information, see: 'https://oleksandr-zhabenko.github.io/uk/rhythmicity/PhLADiPreLiO.Eng.21.html#types'
     -> [Double] -- ^ A list of non-negative values normed by 1.0 (the greatest of which is 1.0) that the line options are compared with. If null, then the program works as for version 0.12.1.0 without this newly-introduced argument since the version 0.13.0.0. The length of it must be a least common multiplier of the (number of syllables plus number of \'_digits\' groups) to work correctly. Is not used when the next 'FilePath' and 'String' arguments are not null.
     -> [PhladiprelioGen]
     -> Int
     -> a1
     -> FilePath -- ^ A file to be probably added output parts to.
     -> Int
     -> IO ()
outputWithFile h wrs ks arr gs us vs selStr compards sRepresent code grps fs num
  | mBool && code >= 10 && code <= 19 && grps == 2 = putStrLn (mconcat [textP, "\n", breaks, "\n", show rs]) >> appendF ((if code >= 15 then mconcat [show rs, "\n", breaks, "\n"] else "") `mappend` outputS)
  | otherwise = appendF outputS
           where mBool = null selStr && null compards
                 appendF = appendFile fs
                 lineOption = head . filter (\(S k _ ts) -> k == num) $ sRepresent
                 textP = (\(S _ _ ts) -> ts) lineOption
                 sylls = createSyllablesPL wrs ks arr gs us vs textP
                 outputS = outputSel lineOption code
                 qqs = readEq4 (mconcat . h . createSyllablesPL wrs ks arr gs us vs) (map (map charS) . mconcat . createSyllablesPL wrs ks arr gs us vs) . basicSplit $ textP
                 (breaks,rs) = showZerosFor2PeriodMusic qqs


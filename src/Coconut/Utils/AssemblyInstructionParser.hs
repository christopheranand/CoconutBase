module Coconut.Utils.AssemblyInstructionParser where

import Data.Set (Set)
import Data.Maybe
import Data.Char 
import qualified Data.Set as Set

getAssemblyInstructions :: FilePath -> IO (Set String)
getAssemblyInstructions path = do 
    contents <- readFile path 
    return $ extractInstructions contents 

extractInstructions :: String -> Set String 
extractInstructions fileString = let 
    parseLine  = lines fileString 
    parseWords = map words parseLine
    filteredInstructions = map fromJust $ filter (maybe False (all isLower)) (map listToMaybe parseWords) 
    in Set.fromList filteredInstructions


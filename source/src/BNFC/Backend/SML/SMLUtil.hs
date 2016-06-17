module BNFC.Backend.SML.SMLUtil where

import BNFC.CF
import BNFC.Utils
import Data.Char (toLower)

-- Translate Haskell types to SML types
-- Note: OCaml (data-)types start with lowercase letter
fixType :: Cat -> String
fixType (ListCat c) = fixType c +++ "list"
fixType (TokenCat "Integer") = "int"
fixType (TokenCat "Double") = "real"
fixType cat = let c:cs = show cat in
                let ls = toLower c : cs in
                  if ls `elem` reservedSML then ls ++ "T" else ls

-- Reserved words of SML, as given in _The Definition of Standard ML_
-- by Milner, Tofte, Harper, and MacQueen._
reservedSML :: [String]
reservedSML = [
    "abstype","and","andalso","as","case","datatype","do","else","end",
    "exception","fn","fun","handle","if","in","infix","infixr","let","local",
    "nonfix","of","op","open","orelse","raise","rec","then","type","val",
    "with","withtype","while"]

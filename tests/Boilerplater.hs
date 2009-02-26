module Boilerplater where

import Data.Char
import Data.List
import Data.List.Split
import Data.Maybe

import Language.Haskell.TH

import Debug.Trace


testProperties :: Q [Dec] -> Q Exp
testProperties mdecs = do
    decs <- mdecs
    -- NB: the use of mkName here ensures we do late binding to the testProperty function. This means that
    -- it can refer to either the function from QuickCheck or QuickCheck2 according to what the user has.
    property_exprs <- sequence [[| $(varE (mkName "testProperty")) $(stringE prop_name) $(varE nm) |]
                               | Just nm <- map decName_maybe decs
                               , Just raw_prop_name <- [stripPrefix_maybe "prop_" (nameBase nm)]
                               , let prop_name = humanize raw_prop_name ]
    return $ LetE decs (ListE property_exprs)

-- | Extracts a 'Name' from the declaration if it binds precisely one name
decName_maybe :: Dec -> Maybe Name
decName_maybe (FunD nm _clauses)    = Just nm
decName_maybe (ValD pat _body _dec) = patName_maybe pat
decName_maybe _                     = Nothing

-- | Extracts a 'Name' from the pattern if it binds precisely one name
patName_maybe :: Pat -> Maybe Name
patName_maybe (VarP nm)      = Just nm
patName_maybe (TildeP pat)   = patName_maybe pat
patName_maybe (SigP pat _ty) = patName_maybe pat
patName_maybe _              = Nothing

stripPrefix_maybe :: String -> String -> Maybe String
stripPrefix_maybe prefix what
  | what_start == prefix = Just what_end
  | otherwise            = trace ("Nothing: " ++ what) Nothing
  where (what_start, what_end) = splitAt (length prefix) what

-- | Makes a valid Haskell identifier more comprehensible to human eyes. For example:
--
-- > humanize "new_HashTable_is_empty" == "New HashTable is empty"
humanize :: String -> String
humanize identifier = case splitOneOf "_" identifier of
    [] -> "(no name)"
    (first_word:next_words) -> intercalate " " $ capitalizeFirstLetter first_word : next_words

capitalizeFirstLetter :: String -> String
capitalizeFirstLetter []     = []
capitalizeFirstLetter (c:cs) = toUpper c : cs
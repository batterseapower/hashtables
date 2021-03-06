module Properties (tests) where

import Boilerplater

import Data.HashTable.MArray


import Control.Monad.ST

import Data.Array.ST ( STArray )

import Test.Framework
import Test.Framework.Providers.QuickCheck2

import Test.QuickCheck
import Test.QuickCheck.Monadic


import Prelude hiding ( lookup )


tests :: [Test]
tests = [
        testGroup "Data.HashTable.MArray" data_hashtable_marray_tests
    ]


data_hashtable_marray_tests :: [Test]
data_hashtable_marray_tests = $(testProperties [d|
        --prop_fromList_toList_fromList_is_id (xs :: [(Int, Int)]) = fromList (toList (fromList xs)) == xs
        
        prop_new_HashTable_is_empty = runST $ do
            ht <- new :: ST s (HashTable (STArray s) (ST s) Int Int)
            fmap null (toList ht)
        
        prop_insert_changes_value_at_key ht key val = monadic runST $ do
            value_afterwards <- run $ do
                insert (ht :: HashTable (STArray s) (ST s) Int Int) key val
                lookup ht key
            assert (value_afterwards == Just val)
    |])
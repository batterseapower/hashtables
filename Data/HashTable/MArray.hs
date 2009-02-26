-- | A variety of HashTables that use Data.Array arrays as their backing store.
-- The implementation is substantially based on Data.HashTable from the base library.
module Data.HashTable.MArray (
        HashTable, ArrayElement,
        new, insert, delete, lookup, update,
        fromList, toList,
        longestChain
    ) where

import Data.Hashable
import Control.Monad.Ref

import Data.Array.MArray

-- For specialisation purposes:
import Data.Array.IO ( IOArray )
import Data.Array.ST ( STArray )

import Data.Bits ( (.&.) )
import Data.Int  ( Int32 )
import Data.List ( maximumBy, partition )

import Control.Monad
import Control.Monad.ST ( ST )

import Prelude hiding ( lookup )


-- | The type of elements the HashTable needs to instantiate the MArray at.
-- Somewhat unfortunately we cannot make this abstract since the calling code
-- needs to instantiate it, so we comprimise by at least giving it a name.
type ArrayElement key val = [(key, val)]

data HashTable (arr :: * -> * -> *) m key val = HashTable !(Ref m (HT arr m key val))
  -- TODO: should the IORef really be an MVar?

data HT (arr :: * -> * -> *) m key val = HT {
        kcount  :: !Int32, -- ^ Total number of keys
        bmask   :: !Int32, -- ^ Mask to clip hashes to the number of buckets
        buckets :: !(arr Int32 (ArrayElement key val))
    }


-- -----------------------------------------------------------------------------
-- Parameters

-- | Maximum size, in keys, of the hash table
tABLE_MAX :: Int32
tABLE_MAX  = 32 * 1024 * 1024

tABLE_MIN :: Int32
tABLE_MIN  = 8

-- | Maximum average load of a single hash bucket
hLOAD :: Int32
hLOAD = 7

-- | Entries to ignore in load computation. Hysteresis favors long
-- association-list-like behavior for small tables.
hYSTERESIS :: Int32
hYSTERESIS = 64


-- -----------------------------------------------------------------------------
-- Creating a new hash table

-- | Creates a new, empty, hash table
new :: (MonadRef m, MArray arr (ArrayElement key val) m)
    => m (HashTable arr m key val)
{-# SPECIALIZE new :: IO (HashTable IOArray IO key val) #-}
{-# SPECIALIZE new :: ST s (HashTable (STArray s) (ST s) key val) #-}
new = do
    -- Make a new hash table with a single, empty, segment
    let mask = tABLE_MIN - 1
    bkts <- newArray (0, mask) []

    -- Create a reference to the hash table and return it as a HashTable instance
    liftM HashTable $ newRef $ HT { buckets = bkts, kcount = 0, bmask = mask }


-- -----------------------------------------------------------------------------
-- Inserting a key\/value pair into the hash table

-- | Inserts a key\/value mapping into the hash table.
--
-- Note that 'insert' doesn't remove the old entry from the table -
-- the behaviour is like an association list, where 'lookup' returns
-- the most-recently-inserted mapping for a key in the table.  The
-- reason for this is to keep 'insert' as efficient as possible.  If
-- you need to update a mapping, then we provide 'update'.
insert :: (Hashable key, MonadRef m, MArray arr (ArrayElement key val) m)
       => HashTable arr m key val -> key -> val -> m ()
{-# SPECIALIZE insert :: Hashable key => HashTable IOArray IO key val -> key -> val -> IO () #-}
{-# SPECIALIZE insert :: Hashable key => HashTable (STArray s) (ST s) key val -> key -> val -> ST s () #-}
{-# SPECIALIZE insert :: HashTable IOArray IO Int val -> Int -> val -> IO () #-}
{-# SPECIALIZE insert :: HashTable (STArray s) (ST s) Int val -> Int -> val -> ST s () #-}
{-# SPECIALIZE insert :: Hashable key => HashTable IOArray IO [key] val -> [key] -> val -> IO () #-}
{-# SPECIALIZE insert :: Hashable key => HashTable (STArray s) (ST s) [key] val -> [key] -> val -> ST s () #-}
insert ht key val = updatingBucket CanInsert bucket_fn ht key
  where bucket_fn bucket = ((key,val):bucket, 1, ())


-- ------------------------------------------------------------
-- The core of the implementation is lurking down here, in findBucket,
-- updatingBucket, and expandHashTable.

-- | Given a number of keys and a number of buckets, ask whether the hashtable is too big
tooBig :: Int32 -> Int32 -> Bool
tooBig k b = k-hYSTERESIS > hLOAD * b

-- | Find the index of a bucket within table given a mask and hash
bucketIndex :: Int32 -> Int32 -> Int32
bucketIndex mask h = h .&. mask

-- | Find the bucket in which the key belongs.
-- Returns (key equality, bucket index, bucket)
--
-- This rather grab-bag approach gives enough power to do pretty much
-- any bucket-finding thing you might want to do.  We rely on inlining
-- to throw away the stuff we don't want.  I'm proud to say that this
-- plus updatingBucket below reduce most of the other definitions to a
-- few lines of code, while actually speeding up the hashtable
-- implementation when compared with a version which does everything
-- from scratch.
findBucket :: (Hashable key, MonadRef m, MArray arr (ArrayElement key val) m)
           => HashTable arr m key val -> key -> m (HT arr m key val, Int32, [(key, val)])
{-# INLINE findBucket #-}
findBucket (HashTable table_ref) key = do
  table@HT{ buckets=bkts, bmask=b } <- readRef table_ref
  let indx = bucketIndex b (int32Hash key)
  bucket <- readArray bkts indx
  return (table, indx, bucket)

-- | Data structure used to indicate to 'updatingBucket' whether the current
-- update is able to insert into the table. If it can't insert, no hashtable
-- enlargement will be required.
data Inserts = CanInsert   -- Used by update and insert
             | CantInsert -- Used by delete
             deriving (Eq)

-- | 'updatingBucket' is the real workhorse of all single-element table
-- updates.  It takes a hashtable and a key, along with a function
-- describing what to do with the bucket in which that key belongs.  A
-- flag indicates whether this function may perform table insertions.
-- The function returns the new contents of the bucket, the number of
-- bucket entries inserted (negative if entries were deleted), and a
-- value which becomes the return value for the function as a whole.
-- The table sizing is enforced here, calling out to expandSubTable as
-- necessary.
--
-- This function is intended to be inlined and specialized for every
-- calling context (eg every provided bucket_fn).
updatingBucket :: (Hashable key, MonadRef m, MArray arr (ArrayElement key val) m)
               => Inserts                                  -- ^ Whether the bucket function can insert
               -> ([(key,val)] -> ([(key,val)], Int32, a)) -- ^ Bucket modification function
               -> HashTable arr m key val                  -- ^ Hashtable to change
               -> key                                      -- ^ Key of the bucket to mess with
               -> m a                                      -- ^ Value from the bucket function
{-# INLINE updatingBucket #-}
updatingBucket can_enlarge bucket_fn ht@(HashTable tab_ref) key = do
  -- Obtain the bucket to be modified and run the supplied function on it
  (table@HT{ kcount=k, buckets=bkts, bmask=b }, indx, bckt) <- findBucket ht key
  (bckt', inserts, result) <- return $ bucket_fn bckt
  
  -- Update the bucket using the value return by the function
  let k' = k + inserts
      table' = table { kcount=k' }
  writeArray bkts indx bckt'
  
  -- Check if we need to expand the hash table due to a new insertion
  table'' <- if can_enlarge == CanInsert && inserts > 0 && tooBig k' b
             then expandHashTable table'
             else return table'
  
  -- Update the mutable reference to a hashtable
  writeRef tab_ref table''
  return result

-- | Rehashes the hashtable into a new one to reduce the load factor
expandHashTable :: (Hashable key, MArray arr (ArrayElement key val) m)
                => HT arr m key val -> m (HT arr m key val)
{-# INLINE expandHashTable #-} -- INLINE into updatingBucket use site to benefit from specialisation there
expandHashTable table@(HT{ buckets=bkts, bmask=mask })
  | newmask > tABLE_MAX-1 = return table -- We don't allow the number of buckets to expand beyond a certain size
  | otherwise = do
    -- Create the new buckets array
    newbkts <- newArray (0, newmask) []
    
    -- Bucket splitting: rehash all the elements in each bucket and put them into one of two
    -- new buckets corresponding to one old bucket depending on the value of the topmost bit
    -- under the expanded mask 'newmask'
    let splitBucket oldindex = do
          bucket <- readArray bkts oldindex
          let (oldb,newb) = partition ((oldindex==) . bucketIndex newmask . int32Hash . fst) bucket
          writeArray newbkts oldindex oldb
          writeArray newbkts (oldindex + oldsize) newb
    
    -- Split every bucket in the old hashtable and return the modified table
    mapM_ splitBucket [0..mask]
    return $ table { buckets=newbkts, bmask=newmask }
  where
   oldsize = mask + 1
   newmask = mask + mask + 1


-- -----------------------------------------------------------------------------
-- Deleting a mapping from the hash table

-- | Remove a key from a bucket
deleteBucket :: (key -> Bool) -> [(key,val)] -> ([(key, val)], Int32, ())
deleteBucket _   [] = ([], 0, ())
deleteBucket del (pair@(k,_):bucket) =
  case deleteBucket del bucket of
    (bucket', dels, _) | del k     -> dels' `seq` (bucket', dels', ())
                       | otherwise -> (pair:bucket', dels, ())
      where dels' = dels - 1

-- | Remove an entry from the hash table
delete :: (Hashable key, MonadRef m, MArray arr (ArrayElement key val) m)
       => HashTable arr m key val -> key -> m ()
{-# SPECIALIZE delete :: Hashable key => HashTable IOArray IO key val -> key -> IO () #-}
{-# SPECIALIZE delete :: Hashable key => HashTable (STArray s) (ST s) key val -> key -> ST s () #-}
{-# SPECIALIZE delete :: HashTable IOArray IO Int val -> Int -> IO () #-}
{-# SPECIALIZE delete :: HashTable (STArray s) (ST s) Int val -> Int -> ST s () #-}
{-# SPECIALIZE delete :: Hashable key => HashTable IOArray IO [key] val -> [key] -> IO () #-}
{-# SPECIALIZE delete :: Hashable key => HashTable (STArray s) (ST s) [key] val -> [key] -> ST s () #-}
delete ht key = updatingBucket CantInsert (deleteBucket (key ==)) ht key


-- -----------------------------------------------------------------------------
-- Updating a mapping in the hash table

-- | Updates an entry in the hash table, returning 'True' if there was
-- already an entry for this key, or 'False' otherwise.  After 'update'
-- there will always be exactly one entry for the given key in the table.
--
-- 'insert' is more efficient than 'update' if you don't care about
-- multiple entries, or you know for sure that multiple entries can't
-- occur.  However, 'update' is more efficient than 'delete' followed
-- by 'insert'.
update :: (Hashable key, MonadRef m, MArray arr (ArrayElement key val) m)
       => HashTable arr m key val -> key -> val -> m Bool
{-# SPECIALIZE update :: Hashable key => HashTable IOArray IO key val -> key -> val -> IO Bool #-}
{-# SPECIALIZE update :: Hashable key => HashTable (STArray s) (ST s) key val -> key -> val -> ST s Bool #-}
{-# SPECIALIZE update :: HashTable IOArray IO Int val -> Int -> val -> IO Bool #-}
{-# SPECIALIZE update :: HashTable (STArray s) (ST s) Int val -> Int -> val -> ST s Bool #-}
{-# SPECIALIZE update :: Hashable key => HashTable IOArray IO [key] val -> [key] -> val -> IO Bool #-}
{-# SPECIALIZE update :: Hashable key => HashTable (STArray s) (ST s) [key] val -> [key] -> val -> ST s Bool #-}
update ht key val = updatingBucket CanInsert bucket_fn ht key
  where bucket_fn bucket = let (bucket', dels, _) = deleteBucket (key ==) bucket
                           in  ((key, val):bucket', 1 + dels, dels /= 0)


-- -----------------------------------------------------------------------------
-- Looking up an entry in the hash table

-- | Looks up the value of a key in the hash table.
lookup :: (Hashable key, MonadRef m, MArray arr (ArrayElement key val) m)
       => HashTable arr m key val -> key -> m (Maybe val)
{-# SPECIALIZE lookup :: Hashable key => HashTable IOArray IO key val -> key -> IO (Maybe val) #-}
{-# SPECIALIZE lookup :: Hashable key => HashTable (STArray s) (ST s) key val -> key -> ST s (Maybe val) #-}
{-# SPECIALIZE lookup :: HashTable IOArray IO Int val -> Int -> IO (Maybe val) #-}
{-# SPECIALIZE lookup :: HashTable (STArray s) (ST s) Int val -> Int -> ST s (Maybe val) #-}
{-# SPECIALIZE lookup :: Hashable key => HashTable IOArray IO [key] val -> [key] -> IO (Maybe val) #-}
{-# SPECIALIZE lookup :: Hashable key => HashTable (STArray s) (ST s) [key] val -> [key] -> ST s (Maybe val) #-}
lookup ht key = do
  (_, _, bucket) <- findBucket ht key
  let firstHit (k,v) r | key == k  = Just v
                       | otherwise = r
  return (foldr firstHit Nothing bucket)


-- -----------------------------------------------------------------------------
-- Converting to/from lists

-- | Convert a list of key\/value pairs into a hash table
fromList :: (Hashable key, MonadRef m, MArray arr (ArrayElement key val) m)
         => [(key, val)] -> m (HashTable arr m key val)
{-# SPECIALIZE fromList :: Hashable key => [(key, val)] -> IO (HashTable IOArray IO key val) #-}
{-# SPECIALIZE fromList :: Hashable key => [(key, val)] -> ST s (HashTable (STArray s) (ST s) key val) #-}
{-# SPECIALIZE fromList :: [(Int, val)] -> IO (HashTable IOArray IO Int val) #-}
{-# SPECIALIZE fromList :: [(Int, val)] -> ST s (HashTable (STArray s) (ST s) Int val) #-}
{-# SPECIALIZE fromList :: Hashable key => [([key], val)] -> IO (HashTable IOArray IO [key] val) #-}
{-# SPECIALIZE fromList :: Hashable key => [([key], val)] -> ST s (HashTable (STArray s) (ST s) [key] val) #-}
fromList list = do
  table <- new
  sequence_ [ insert table k v | (k, v) <- list ]
  return table

-- | Converts a hash table to a list of key\/value pairs
toList :: (MonadRef m, MArray arr (ArrayElement key val) m)
       => HashTable arr m key val -> m [(key, val)]
{-# SPECIALIZE toList :: HashTable IOArray IO key val -> IO [(key, val)] #-}
{-# SPECIALIZE toList :: HashTable (STArray s) (ST s) key val -> ST s [(key, val)] #-}
toList = mapReduce id concat

mapReduce :: (MonadRef m, MArray arr (ArrayElement key val) m)
          => ([(key, val)] -> r) -- ^ Mapping function - process bucket
          -> ([r] -> r)          -- ^ Reducing function - combine buckets
          -> HashTable arr m key val
          -> m r
{-# INLINE mapReduce #-}
mapReduce m r (HashTable tab_ref ) = do
  HT { buckets=bckts, bmask=b } <- readRef tab_ref
  liftM r (mapM (liftM m . readArray bckts) [0..b])


-- -----------------------------------------------------------------------------
-- Diagnostics

-- | This function is useful for determining whether your hash
-- function is working well for your data set.  It returns the longest
-- chain of key\/value pairs in the hash table for which all the keys
-- hash to the same bucket.
--
-- If this chain is particularly long (say, longer than 14 elements or
-- so), then it might be a good idea to try a different hash function.
longestChain :: (MonadRef m, MArray arr (ArrayElement key val) m)
             => HashTable arr m key val -> m [(key,val)]
longestChain = mapReduce id (maximumBy lengthCmp)
  where lengthCmp (_:x)(_:y) = lengthCmp x y
        lengthCmp []   []    = EQ
        lengthCmp []   _     = LT
        lengthCmp _    []    = GT
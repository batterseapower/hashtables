module Data.Hashable (
        Hashable(..)
    ) where

import Data.Bits    ( shiftR )
import Data.Int     ( Int32, Int64 )
import Data.List    ( foldl' )
import Data.Char    ( ord )


-- | The golden ratio, used in the hash functions below
golden :: Int32
golden = 1013904242 -- = round ((sqrt 5 - 1) * 2^32) :: Int32
    -- was -1640531527 = round ((sqrt 5 - 1) * 2^31) :: Int32
    -- but that has bad mulHi properties (even adding 2^32 to get its inverse)
    -- Whereas the above works well and contains no hash duplications for
    -- [-32767..65536]

-- | Obtains the high 32 bits of a (x-bit * 32 bit = 64-bit) multiply operation
mulHi :: Int32 -> Int32 -> Int32
mulHi a b = fromIntegral (r `shiftR` 32)
   where r :: Int64 = fromIntegral a * fromIntegral b


class Eq a => Hashable a where
    -- | Obtain an 'Int' hash value from a value of the 'Hashable' type, suitable for
    -- use in a hash table or similar.
    --
    -- Should satisfy the law:
    --
    -- > a == b  =>  int32Hash a == int32Hash b
    int32Hash :: a -> Int32

instance Hashable Int where
    -- This implementation was lifted from Data.HashTable.
    int32Hash x = int32Hash (fromIntegral x :: Int32)

instance Hashable Int32 where
    -- This implementation was lifted from Data.HashTable.
    --
    -- A sample (and useful) hash function for Int and Int32,
    -- implemented by extracting the uppermost 32 bits of the 64-bit
    -- result of multiplying by a 33-bit constant.  The constant is from
    -- Knuth, derived from the golden ratio:
    --
    -- > golden = round ((sqrt 5 - 1) * 2^32)
    --
    -- We get good key uniqueness on small inputs
    -- (a problem with previous versions):
    --  (length $ group $ sort $ map hashInt [-32767..65536]) == 65536 + 32768
    int32Hash x = mulHi x golden + x

instance Hashable Bool where
    -- Can't really do much of interest here
    int32Hash b = fromIntegral (fromEnum b)

instance Hashable Char where
    -- This implementation was lifted from the hashString function of Data.HashTable.
    --
    -- Here we know that individual characters c are often small, and this
    -- produces frequent collisions if we use ord c alone.  A
    -- particular problem are the shorter low ASCII and ISO-8859-1
    -- character strings.  We pre-multiply by a magic twiddle factor to
    -- obtain a good distribution.  In fact, given the following test:
    --
    -- > testp :: Int32 -> Int
    -- > testp k = (n - ) . length . group . sort . map hs . take n $ ls
    -- >   where ls = [] : [c : l | l <- ls, c <- ['\0'..'\xff']]
    -- >         hs = foldl' f golden
    -- >         f m c = fromIntegral (ord c) * k + hashInt32 m
    -- >         n = 100000
    --
    -- We discover that testp magic = 0.
    int32Hash c = fromIntegral (ord c) * magic
      where magic = 0xdeadbeef

instance Hashable a => Hashable [a] where
    -- This implementation was lifted from the hashString function of Data.HashTable.
    --
    -- A sample hash function for Strings.  We keep multiplying by the
    -- golden ratio and adding.  The implementation is:
    --
    -- > hashString = foldl' f golden
    -- >   where f m c = fromIntegral (ord c) * magic + hashInt32 m
    -- >         magic = 0xdeadbeef
    --
    -- Where hashInt32 works just as hashInt shown above.
    --
    -- Knuth argues that repeated multiplication by the golden ratio
    -- will minimize gaps in the hash space, and thus it's a good choice
    -- for combining together multiple keys to form one.
    int32Hash xs = foldl' go golden xs
      where go h x = int32Hash x + int32Hash h
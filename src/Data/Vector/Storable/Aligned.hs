{-# LANGUAGE ScopedTypeVariables #-}
-- | Create mutable 'VM.Vector's whose data begin at a specified
-- alignment.
module Data.Vector.Storable.Aligned (Alignment, align, alignPow2, alignKB,
                                     getAlignment, new) where
import Control.Monad (liftM)
import Control.Monad.Primitive
import qualified Data.Vector.Storable.Mutable as VM
import Data.Word (Word8)
import Foreign.ForeignPtr.Unsafe
import Foreign.Ptr
import Foreign.Storable

-- | Alignment in bytes. Use the smart constructors 'align' and
-- 'alignKB' to specify valid alignments.
newtype Alignment = Alignment Int deriving (Eq, Ord, Show)

-- | Get an alignment in bytes.
getAlignment :: Alignment -> Int
getAlignment (Alignment n) = n

isPowerOfTwo :: Int -> Bool
isPowerOfTwo n
  | n < 1 = False
  | n == 1 = True
  | odd n = False
  | otherwise = isPowerOfTwo $ n `quot` 2

-- | An alignment in bytes.
align :: Int -> Maybe Alignment
align n
  | isPowerOfTwo n = Just (Alignment n)
  | otherwise = Nothing

-- | Create an 'Alignment' at the given power of two. For example,
-- @alignPow2 3@ will give an alignment at an 8 byte boundary.
alignPow2 :: Int -> Alignment
alignPow2 = Alignment . (2^)

-- | An alignment in kilobytes.
alignKB :: Int -> Alignment
alignKB = Alignment . (* 1024)

alignedVector :: PrimMonad m
              => Alignment -> Int -> m (VM.MVector (PrimState m) Word8)
alignedVector (Alignment a) numBytes =
  do v <- VM.new (numBytes + a)
     let ptr = unsafeForeignPtrToPtr . fst $ VM.unsafeToForeignPtr0 v
         ptr' = alignPtr ptr a
         diff = minusPtr ptr' ptr
     return $ VM.unsafeSlice diff numBytes v

-- | @new aligned n@ creates a mutable vector of length @n@ with the
-- given alignment.
new :: forall a m. (Storable a, PrimMonad m)
    => Alignment -> Int -> m (VM.MVector (PrimState m) a)
new a n = liftM VM.unsafeCast $ alignedVector a (n * sizeOf (undefined::a))

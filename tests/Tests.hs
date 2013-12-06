import Control.Applicative
import Data.Maybe (fromJust, isJust, isNothing)
import qualified Data.Vector.Storable.Aligned as VA
import qualified Data.Vector.Storable as V
import Foreign.ForeignPtr.Unsafe
import Foreign.Ptr
import Test.Tasty
import Test.Tasty.HUnit

simpleVector :: VA.Alignment -> V.Vector Int
simpleVector a = V.create $ 
                 do v <- VA.new a 10
                    V.copy v $ V.fromList [1..10]
                    return v

checkAlignment :: V.Storable a => VA.Alignment -> V.Vector a -> Bool
checkAlignment a v = r == 0
  where (fp, _) = V.unsafeToForeignPtr0 v
        addr = fromIntegral . ptrToIntPtr $ unsafeForeignPtrToPtr fp
        (_,r) = addr `quotRem` VA.getAlignment a

main :: IO ()
main = defaultMain $ testGroup "Tests"
       [ testGroup "Alignment"
         [ testCase "Odd alignment" . assert $ isNothing (VA.align 3) 
         , testCase "Zero alignment" . assert $ isNothing (VA.align 0)
         , testCase "No alignment" . assert $ isJust (VA.align 1)
         , testCase "2^7 alignment" . assert $
             VA.getAlignment (VA.alignPow2 7) == 128
         , testCase "8 KB alignment" . assert $
             VA.getAlignment (VA.alignKB 8) == 8*1024 ]

       , testGroup "Vector tests"
         [ testCase "No alignment" $ checkVec (fromJust $ VA.align 1)
         , testCase "Aligned 8 bytes" $ checkVec (fromJust $ VA.align 8)
         , testCase "Aligned 2^4 bytes" $ checkVec (VA.alignPow2 4) 
         , testCase "4 KB alignment" $ checkVec (VA.alignKB 4) ] ]
  where vref = V.fromList [1..10]
        notEq = assertFailure "Aligned vector not equal to reference vector"
        notAligned = assertFailure "Aligned vector is not aligned"
        checkVec a
          | v /= vref = notEq
          | not (checkAlignment a v) = notAligned
          | otherwise = return ()
          where v = simpleVector a

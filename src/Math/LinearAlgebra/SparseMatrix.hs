module Math.LinearAlgebra.SparseMatrix where

import qualified Data.Vector.Unboxed as V
import           Data.Vector.Unboxed (Vector)
import           Data.List (sortBy)
import           Data.Function (on)

type Index = Int

type CSR = CompressedRowMatrix

data CompressedRowMatrix = CompressedRowMatrix { csrVals :: Vector Double -- contains the value of the non-zero elements
                                               , rowPtrs :: Vector Index -- contains the row-index range of the non-zero elements
                                               , cols    :: Vector Index -- contains the column-index of the non-zero elements
                                               } deriving Show

{-

Multiply matrix (stored with Compressed Sparse Row method) with vector d[N]
   for (k = 0; k < N; k = k + 1)
       result[i] = 0;

   for (i = 0; i < N; i = i + 1)
   {
      for (k = RowPtr[i]; k < RowPtr[i+1]; k = k + 1)
      {
         result[i] = result[i] + Val[k]*d[Col[k]];
      }
   }

-}

mul :: CSR -> CSR -> CSR
mul a b = emptyMat
    where numRows = V.maximum $ rowPtrs b

emptyMat :: CSR
emptyMat = CompressedRowMatrix V.empty V.empty V.empty

example :: CSR
example = fromAssocList [ ((0, 1000), 2)
                        , ((0, 5054), 4)
                        , ((1, 3000), 3)
                        ]

-- full matrix we're considering: [[0,0,0],[0,0,0],[0,0,0],[0,0,0],[1,3,0],[4,0,0]]
-- rowPtrs : [0, 0, 0, 0, 0, 2, 3]
-- cols :    [0, 1, 0]
-- vs :      [1, 3, 4]
-- TODO fix this...ERROR
example5 :: CSR
example5 = fromAssocList [ ((0, 1000), 1)
                         , ((0, 5054), 3)
                         , ((1, 3000), 3)
                         ]


-- should produce:
-- vs :      [ 1 , 1 , 1 , 1 ]
-- cols :    [ 0 , 1 , 2 , 4 ]
-- rowPtrs : [ 0 , 1 , 2 , 3 , 3 , 4 ]
-- ^-- this differs because I don't add len + 1th index on the end...
example2 :: CSR
example2 = fromAssocList [ ((0, 0), 1)
                         , ((1, 1), 1)
                         , ((2, 2), 1)
                         , ((4, 4), 1)
                         ]

example3 :: CSR
example3 = mul example2 example2

fromAssocList :: [((Index, Index), Double)] -> CSR
fromAssocList coos = CompressedRowMatrix vs rowPs cs
    where sortedCoos = sortBy (compare `on` fst) coos
          vs = V.fromList $ fmap snd sortedCoos
          cs = V.fromList $ fmap (snd . fst) sortedCoos
          -- I could use a lens and go down that rabbit hole...
          rowPs = V.fromList . (\(_, _, l) -> l) . foldl buildRowPtr (-1, 0, []) $ fmap (fst . fst) sortedCoos
          buildRowPtr :: (Int, Int, [Int]) -> Int -> (Int, Int, [Int])
          buildRowPtr (pr, cidx, ps) r | pr == r   = (pr, succ cidx, ps)
                                       | otherwise = (r, succ cidx, ps ++ replicate (r - pr) cidx)

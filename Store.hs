module Store where

import qualified Data.Map as M

type I = (Int,Int)
type M = (I, Int)
type S = M.Map I Int

type Q = M.Map Int S


-- compute the set of messages to 
diff :: S -> S -> [(Int,Int, Int)]
diff = undefined 
new = M.empty
insertion :: Int -> M -> Q -> Q
insertion i (k,v) = M.insertWith M.union i (M.singleton k v)
{-
data B = B {
	base :: S,
	diffs :: [Q]
	}
-}





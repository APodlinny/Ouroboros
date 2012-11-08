module Ouroboros.Common (
	catchBottom,
	applySetters,
	join,
	split,
	spanSplit,
	startswith,
	endswith,
	logger,
	groupby,
    mapIndex,
    unpackEither
) where

import Control.Exception
import System.IO.Unsafe
import Data.List

catchBottom :: a -> Maybe a
catchBottom x = unsafePerformIO $ handle stub $ evalSafe x
    where
        stub :: SomeException -> IO (Maybe a)
        stub _ = return Nothing

        evalSafe :: a -> IO (Maybe a)
        evalSafe x = evaluate x >>= (return . Just)

applySetters :: [a -> a] -> a -> a
applySetters [] x = x
applySetters (f : fs) x = applySetters fs (f x)

join :: [a] -> [[a]] -> [a]
join separator items = foldl1 joiner items
    where
        joiner a b = a ++ separator ++ b
        
split :: Eq a => a -> [a] -> [[a]]
split separator xs = reverse $ iterate spanResult []
    where
        spanResult = spanSplit separator xs
        iterate (elt, []) result = elt : result
        iterate (elt, other) result = iterate (spanSplit separator other) (elt : result)

spanSplit :: Eq a => a -> [a] -> ([a], [a])
spanSplit separator xs = (element, other)
    where
        result = span (/= separator) xs
        element = fst result
        other = drop 1 $ snd result
        
startswith :: Eq a => [a] -> [a] -> Bool
startswith begining xs = begining == (take (length begining) xs)

endswith :: Eq a => [a] -> [a] -> Bool
endswith ending xs = ending == drop (length xs - length ending) xs

logger :: (a -> String) -> a -> a
logger f x = unsafePerformIO $ do
    putStrLn $ f x
    return x

groupby :: (Ord k, Eq k) => (v -> k) -> [v] -> [[v]]
groupby selector values = 
        groupBy (equality selector) $
        sortBy (ordering selector) values
    where
        equality s a b = (s a) == (s b)
        ordering s a b = compare (s a) (s b)

mapIndex :: (Int -> a -> b) -> [a] -> [b]
mapIndex f xs = 
    map (uncurry f) $ 
    zipWith (,) [0 .. length xs - 1] xs

unpackEither :: (Show e) => Either e a -> a
unpackEither (Left err) = error $ "Error: " ++ (show err)
unpackEither (Right x) = x
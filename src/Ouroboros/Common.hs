module Ouroboros.Common (
	applySetters
) where

applySetters :: [a -> a] -> a -> a
applySetters [] x = x
applySetters (f : fs) x = applySetters fs (f x)
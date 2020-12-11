{-# LANGUAGE BangPatterns #-}
module Data.Vector.Extended
    ( module Data.Vector
    , generate'
    ) where

import Data.Vector
import qualified Data.Vector.Mutable as VM

-- | Strict version of 'generate'
generate' :: Int -> (Int -> a) -> Vector a
generate' len f = create $ do
    v <- VM.new len
    let go i
            | i >= len  = pure v
            | otherwise = do
                let !x = f i
                VM.unsafeWrite v i x
                go (i + 1)
    go 0

-- I wanted to write this in C, but I couldn't really be bothered, so this is
-- Haskell that tries to do it the way I would do it in C.

import           AdventOfCode.Main           (simpleMain)
import           Control.Monad.Primitive     (PrimMonad (..), PrimState (..))
import           Control.Monad.ST            (runST)
import           Data.Char                   (digitToInt, isDigit)
import           Data.Foldable               (for_)
import           Data.Int                    (Int64)
import qualified Data.Vector.Unboxed.Mutable as VUM

type FileID = Int

noFileID :: FileID
noFileID = -1

type DiskMap = [Int]

data Disk s = Disk
    { dBlocks    :: VUM.MVector s FileID
    , dFiles     :: VUM.MVector s (Int, Int)  -- Position, length
    , dMaxFileID :: FileID
    }

diskMapToDisk :: PrimMonad m => DiskMap -> m (Disk (PrimState m))
diskMapToDisk diskMap = do
    blocks <- VUM.replicate 1 noFileID
    files <- VUM.replicate 1 (0, 0)
    go True 0 0 diskMap $ Disk blocks files noFileID
  where
    go _      _   _   []           d = pure d
    go isFile pos fid (len : lens) d
        | pos + len - 1 >= VUM.length (dBlocks d) = do
            let l = VUM.length (dBlocks d)
            v <- VUM.grow (dBlocks d) l
            VUM.set (VUM.slice l l v) noFileID
            go isFile pos fid (len : lens) d {dBlocks = v}
        | fid >= VUM.length (dFiles d) = do
            let l = VUM.length (dFiles d)
            v <- VUM.grow (dFiles d) l
            VUM.set (VUM.slice l l v) (0, 0)
            go isFile pos fid (len : lens) d {dFiles = v}
        | isFile = do
            VUM.set (VUM.slice pos len (dBlocks d)) fid
            VUM.write (dFiles d) fid (pos, len)
            go False (pos + len) (fid + 1) lens d {dMaxFileID = fid}
        | otherwise = do
            go True (pos + len) fid lens d

compact1 :: PrimMonad m => Disk (PrimState m) -> m ()
compact1 d = go 0 (VUM.length (dBlocks d) - 1)
  where
    go i j
        | i >= j    = pure ()
        | otherwise = do
            l <- VUM.read (dBlocks d) i
            if l /= noFileID then
                go (i + 1) j
            else do
                r <- VUM.read (dBlocks d) j
                if r == noFileID then
                    go i (j - 1)
                else do
                    VUM.write (dBlocks d) i r
                    VUM.write (dBlocks d) j noFileID
                    go (i + 1) (j - 1)

findFreeSpace
    :: PrimMonad m => Int -> Int -> Int -> Disk (PrimState m) -> m (Maybe Int)
findFreeSpace startIndex stopIndex len d = go startIndex
  where
    go i
        | i >= stopIndex                        = pure Nothing
        | i + len - 1 >= VUM.length (dBlocks d) = pure Nothing
        | otherwise                             = do
            mbJ <- used 0
            case mbJ of
                Nothing -> pure (Just i)
                Just j  -> go (i + j + 1)
      where
        used j
            | j >= len  = pure Nothing
            | otherwise = do
                fid <- VUM.read (dBlocks d) (i + j)
                if fid == noFileID then used (j + 1) else pure (Just j)

compact2 :: PrimMonad m => Disk (PrimState m) -> m ()
compact2 disk = do
    starts <- VUM.replicate 10 0  -- Max length is 9
    for_ [dMaxFileID disk, dMaxFileID disk - 1 .. 0] $ \fid -> do
        (pos, len) <- VUM.read (dFiles disk) fid
        start <- VUM.read starts len
        mbNewPos <- findFreeSpace start pos len disk
        for_ mbNewPos $ \newPos -> do
            VUM.write starts len (newPos + len)
            VUM.set (VUM.slice pos len (dBlocks disk)) noFileID
            VUM.set (VUM.slice newPos len (dBlocks disk)) fid

checksum :: PrimMonad m => Disk (PrimState m) -> m Int64
checksum disk = go 0 0
  where
    go !acc i
        | i >= VUM.length (dBlocks disk) = pure acc
        | otherwise                      = do
            fid <- VUM.read (dBlocks disk) i
            let increment = if fid == noFileID then 0 else fid * i
            go (acc + fromIntegral increment) (i + 1)

main :: IO ()
main = simpleMain $ \str ->
    let diskMap = map digitToInt $ filter isDigit str
        part1 = runST $ do
            d <- diskMapToDisk diskMap
            compact1 d
            checksum d
        part2 = runST $ do
            d <- diskMapToDisk diskMap
            compact2 d
            checksum d in
    (part1, part2)

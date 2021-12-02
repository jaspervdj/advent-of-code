module Main where

import           AdventOfCode.Main
import qualified AdventOfCode.NanoParser as P
import           Control.Applicative     (many, (<|>))
import           Data.List               (foldl')

data Command = Forward !Int | Down !Int | Up !Int deriving (Show)

parseCommands :: P.Parser Char [Command]
parseCommands = many $
    (P.string "forward" *> P.spaces *> (Forward <$> P.decimal <* P.spaces)) <|>
    (P.string "down"    *> P.spaces *> (Down    <$> P.decimal <* P.spaces)) <|>
    (P.string "up"      *> P.spaces *> (Up      <$> P.decimal <* P.spaces))

data Pos = Pos {pDepth :: !Int, pHorizontal :: !Int} deriving (Show)

updatePos :: Command -> Pos -> Pos
updatePos cmd (Pos d h) = case cmd of
    Forward x -> Pos  d      (h + x)
    Down    x -> Pos (d + x)  h
    Up      x -> Pos (d - x)  h

data AimPos = AimPos !Int !Pos deriving (Show)

updateAimPos :: Command -> AimPos -> AimPos
updateAimPos cmd (AimPos aim (Pos d h)) = case cmd of
    Down    x -> AimPos (aim + x) $ Pos  d             h
    Up      x -> AimPos (aim - x) $ Pos  d             h
    Forward x -> AimPos  aim      $ Pos (d + aim * x) (h + x)

main :: IO ()
main = pureMain $ \input -> do
    commands <- P.runParser parseCommands input
    let pos1          = foldl' (flip updatePos)    (Pos 0 0)            commands
        AimPos _ pos2 = foldl' (flip updateAimPos) (AimPos 0 (Pos 0 0)) commands
        mul (Pos d h) = d * h
    pure (pure (mul pos1), pure (mul pos2))

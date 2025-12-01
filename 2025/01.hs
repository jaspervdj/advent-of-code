import           AdventOfCode.Main       (pureMain)
import qualified AdventOfCode.NanoParser as NP
import           Control.Applicative     (many, (<|>))
import           Control.Monad           (when)
import           Data.List               (mapAccumL)

data Rotation = L Int | R Int deriving (Show)

parseRotations :: NP.Parser Char [Rotation]
parseRotations = many $ rotation <* NP.spaces
  where
    rotation =
        (L <$> (NP.char 'L' *> NP.decimal)) <|>
        (R <$> (NP.char 'R' *> NP.decimal))

stepRotate :: Int -> Int -> Rotation -> (Int, Int)
stepRotate size p rot =
    let q = case rot of
            L x -> p - x
            R x -> p + x
        (zeroes, remainder) = q `divMod` size
        offByOnes = case rot of
            R _ -> 0
            L _ -> if remainder == 0 then 1 else 0 - if p == 0 then 1 else 0 in
    (remainder, abs zeroes + offByOnes)

stepRotations :: Int -> Int -> [Rotation] -> [(Int, Int)]
stepRotations size pos = snd . mapAccumL
    (\p rot -> let (q, zeroes) = stepRotate size p rot in (q, (q, zeroes)))
    pos

main :: IO ()
main = do
    sanityCheckRotate
    pureMain $ \str -> do
        rots <- NP.runParser parseRotations str
        pure
            ( pure $ length $ filter ((== 0) . fst) $ stepRotations 100 50 rots
            , pure $ sum $ map snd $ stepRotations 100 50 rots
            )

sanityCheckRotate :: IO ()
sanityCheckRotate = do
    assertRotate 50 (L 68) (82, 1)
    assertRotate 82 (L 30) (52, 0)
    assertRotate 52 (R 48) ( 0, 1)
    assertRotate  0 (L  5) (95, 0)
    assertRotate 95 (R 60) (55, 1)
    assertRotate 55 (L 55) ( 0, 1)
    assertRotate  0 (L  1) (99, 0)
    assertRotate 99 (L 99) ( 0, 1)  -- Exception?
    assertRotate  0 (R 14) (14, 0)
    assertRotate 14 (L 82) (32, 1)

    assertRotate  0 (R 100) (0, 1)
  where
    assertRotate p r e =
        let actual = stepRotate 100 p r in
        when (actual /= e) $ fail $
            "stepRotate 100 " ++ show p ++ " " ++ show r ++ ": expected " ++
            show e ++ " but got " ++ show actual

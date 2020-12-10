import           Data.List   (sort)
import qualified Data.Set    as Set
import qualified Data.Vector as V

arrangements :: Int -> Set.Set Int -> Int
arrangements target adapters =
    let out = V.generate (target + 1) $ \n -> case n of
            0 -> 1
            _ | not (Set.member n adapters) -> 0
            _ -> sum [out V.! i | i <- [max 0 (n - 3) .. n - 1]] in
    out V.! target

main :: IO ()
main = do
    inputs <- sort . map read . lines <$> getContents
    let target = last inputs + 3
        jolts  = [0] <> inputs <> [target]
        delta  = zipWith (-) (drop 1 jolts) jolts
    print $ length (filter (== 1) delta) * length (filter (== 3) delta)
    print $ arrangements target (Set.fromList jolts)

import           AdventOfCode.Main (simpleMain)
import           Data.Bits         (xor)
import           Data.Int          (Int64)
import           Data.List         (foldl')
import qualified Data.Map          as M
import qualified Data.Set          as S

-- | Bit more typing effort to not mix up numbers and guarantee window
-- size.
newtype SecretNumber = SecretNumber {unSecretNumber :: Int64} deriving (Show)
newtype Price = Price Int deriving (Show)
newtype Delta = Delta Int deriving (Eq, Ord, Show)
data Window = Window !Delta !Delta !Delta !Delta deriving (Eq, Ord, Show)

nextSecretNumber :: SecretNumber -> SecretNumber
nextSecretNumber (SecretNumber s01) =
    let !s02 = prune $ mix s01 (s01 * 64)
        !s03 = prune $ mix s02 $ s02 `div` 32
        !s04 = prune $ mix s03 $ s03 * 2048 in
    SecretNumber s04
  where
    prune = (`mod` 16777216)
    mix   = xor

price :: SecretNumber -> Price
price (SecretNumber s) = Price $ fromIntegral (s `mod` 10)

delta :: Price -> Price -> Delta
delta (Price prev) (Price curr) = Delta (curr - prev)

buyer :: SecretNumber -> [(SecretNumber, Price, Maybe Window)]
buyer s0 =
    (s0, price s0, Nothing) : (s1, price s1, Nothing) :
    (s2, price s2, Nothing) : (s3, price s3, Nothing) :
    (s4, price s4, Just w4) : go w4 s4
  where
    s1 = nextSecretNumber s0
    s2 = nextSecretNumber s1
    s3 = nextSecretNumber s2
    s4 = nextSecretNumber s3

    p0 = price s0
    p1 = price s1
    p2 = price s2
    p3 = price s3
    p4 = price s4

    d1 = delta p0 p1
    d2 = delta p1 p2
    d3 = delta p2 p3
    d4 = delta p3 p4

    w4 = Window d1 d2 d3 d4

    go (Window _ wd1 wd2 wd3) ps =
        let !s = nextSecretNumber ps
            !p = price s
            !w = Window wd1 wd2 wd3 (delta (price ps) p) in
        (s, p, Just w) : go w s

type Auction = M.Map Window Int

auction :: [(SecretNumber, Price, Maybe Window)] -> Auction -> Auction
auction = go S.empty
  where
    go _    []                         !acc = acc
    go seen ((_, _, Nothing) : h)      !acc = go seen h acc
    go seen ((_, Price p, Just w) : h) !acc
        | w `S.member` seen = go seen h acc
        | otherwise         = go (S.insert w seen) h (M.insertWith (+) w p acc)

main :: IO ()
main = simpleMain $ \str ->
    let initial = map (SecretNumber . read) $ lines str
        part1   = sum $
            map (unSecretNumber . (!! 2000) . iterate nextSecretNumber) initial

        part2 = maximum $ foldl'
            (\acc sn -> auction (take 2000 (buyer sn)) acc)
            M.empty
            initial in
    (part1, part2)

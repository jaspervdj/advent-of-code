import           AdventOfCode.Main       (pureMain)
import           AdventOfCode.NanoParser as NP
import           Control.Applicative     (many, (<|>))
import           Control.Monad           (guard)
import           Data.Foldable           (toList)
import           Data.List               (foldl', intercalate, isPrefixOf)
import qualified Data.Map                as M
import           Data.Maybe              (listToMaybe, maybeToList)
import qualified Data.Set                as S
import           Text.Printf             (printf)

type Var = String
data Binop = And | Or | Xor deriving (Eq, Ord, Show)
data Device v = Device [(v, Bool)] [(v, Binop, v, v)] deriving (Functor, Show)

parseDevice :: NP.Parser Char (Device Var)
parseDevice = Device <$> many initial <*> many gate
  where
    initial = (,) <$> (var <* tok (NP.char ':')) <*> bit
    gate    = (,,,) <$> var <*> binop <*> var <* tok (NP.string "->") <*> var

    bit   = tok $ (True <$ NP.char '1') <|> (False <$ NP.char '0')
    var   = tok $ (:) <$> NP.alpha <*> many NP.alphaNum
    binop = tok $
        (And <$ NP.string "AND") <|>
        (Or  <$ NP.string "OR")  <|>
        (Xor <$ NP.string "XOR")

    tok p = p <* NP.spaces

simulate :: Ord v => Device v -> M.Map v Bool
simulate (Device initial gates) = out
  where
    -- We don't need to care about dependency order, just rely on laziness
    -- to figure this out.
    out = M.union (M.fromList initial) $ M.fromList $ do
        (x, bop, y, o) <- gates
        pure (o, eval bop (out M.! x) (out M.! y))

    eval And = (&&)
    eval Or  = (||)
    eval Xor = (/=)

fromBinary :: [Bool] -> Int
fromBinary = foldl' (\acc b -> acc * 2 + if b then 1 else 0) 0

toDot :: Device Var -> String
toDot (Device _ gates) = unlines $
    ["digraph device {"] ++ zipWith dot [0 :: Int ..] gates ++ ["}"]
  where
    dot i (x, bop, y, o) =
        let gate = show bop ++ "_" ++ show i in
        "  " ++ x ++ " -> " ++ gate ++ "; " ++ y ++ " -> " ++ gate ++ "; " ++
        gate ++ " -> " ++ show o ++ ";"

-- | This representation allows us to quickly look up outputs given some
-- inputs and an operation.
newtype Gates = Gates (M.Map (Var, Binop, Var) Var)

mkGates :: Device Var -> Gates
mkGates (Device _ gates) = Gates $ M.fromList $ do
    (x, bop, y, o) <- gates
    pure ((min x y, bop, max x y), o)

outputs :: Gates -> [Var]
outputs (Gates m) = toList m

swapOutputs :: Var -> Var -> Gates -> Gates
swapOutputs v1 v2 (Gates m) = Gates $ fmap
    (\o -> if o == v1 then v2 else if o == v2 then v1 else o)
    m

lookupOutput :: Gates -> Var -> Binop -> Var -> Maybe Var
lookupOutput (Gates m) x bop y = M.lookup (min x y, bop, max x y) m

-- | Parse a single half adder from the device, given input x/y.
-- Returns the result bit, the carry bit, and all assigned outputs.
parseHalfAdder :: Gates -> Var -> Var -> Maybe (Var, Var, S.Set Var)
parseHalfAdder device x y = do
    z <- lookupOutput device x Xor y
    c <- lookupOutput device x And y
    pure (z, c, S.fromList [z, c])

-- | Parse a single full adder from the device, given input x/y/carry.
-- Returns the result bit, the carry bit, and all assigned outputs.
parseFullAdder :: Gates -> Var -> Var -> Var -> Maybe (Var, Var, S.Set Var)
parseFullAdder device x y c = do
    -- Find pattern
    tmp1 <- lookupOutput device x    Xor y
    tmp2 <- lookupOutput device x    And y
    z    <- lookupOutput device tmp1 Xor c
    tmp3 <- lookupOutput device tmp1 And c
    cf   <- lookupOutput device tmp2 Or  tmp3

    -- Check name expectations
    guard $ "z" `isPrefixOf` z
    guard $ not $ any ("z" `isPrefixOf`) [cf, tmp1, tmp2, tmp3]
    pure (z, cf, S.fromList [z, cf, tmp1, tmp2, tmp3])

fixupAdder :: Int -> Gates -> Maybe (S.Set Var)
fixupAdder swaps gates0 = do
    -- Parse the first half adder, this should be correct.
    (_, c0, assigned) <- parseHalfAdder gates0 "x00" "y00"
    go 1 assigned S.empty gates0 c0
  where
    go :: Int -> S.Set Var -> S.Set Var -> Gates -> Var -> Maybe (S.Set Var)
    go i correct swapped gates c = case parseFullAdder gates x y c of
        -- Found enough swaps
        _ | S.size swapped >= swaps * 2 -> pure swapped
        -- We full adder i looks correct, continue
        Just (_, cf, assigned) ->
            go (i + 1) (correct <> assigned) swapped gates cf
        -- There's something fishy about full adder i
        Nothing -> listToMaybe $ do
            -- Try all possible output swaps
            let canSwap = filter (`S.notMember` correct) $ outputs gates
            (v1, v2) <- [(v1, v2) | v1 <- canSwap, v2 <- canSwap, v1 < v2]
            let gates' = swapOutputs v1 v2 gates

            -- Continue using the first one that we can parse
            _ <- maybeToList $ parseFullAdder gates' x y c
            maybeToList $ go i correct (swapped <> S.fromList [v1, v2]) gates' c
      where
        x = printf "x%02d" i
        y = printf "y%02d" i

main :: IO ()
main = pureMain $ \str -> do
    device <- NP.runParser parseDevice str
    let part1 = fromBinary $ map snd $ filter (isPrefixOf "z" . fst) $
            M.toDescList $ simulate device
        part2 = fmap (intercalate "," . S.toAscList) $
            fixupAdder 4 $ mkGates device
    pure (pure part1, maybe (Left "no solution") pure part2)

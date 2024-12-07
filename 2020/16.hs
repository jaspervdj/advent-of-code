import           AdventOfCode.Main
import qualified AdventOfCode.NanoParser as NP
import           Control.Monad           (guard)
import           Data.Char               (isAlpha)
import           Data.Foldable           (toList)
import qualified Data.List               as L
import qualified Data.Vector             as V

--------------------------------------------------------------------------------

data Constraint = Constraint String Int Int Int Int
    deriving (Eq, Show)

type Ticket = [Int]

data Input = Input
    { inputConstraints   :: [Constraint]
    , inputYourTicket    :: Ticket
    , inputNearbyTickets :: [Ticket]
    } deriving (Show)

--------------------------------------------------------------------------------

parseConstraint :: NP.Parser Char Constraint
parseConstraint = Constraint
    <$> (toList <$> NP.many1 idchar <* NP.char ':' <* NP.spaces)
    <*> (NP.decimal <* NP.char '-')
    <*> (NP.decimal <* NP.spaces <* NP.string "or" <* NP.spaces)
    <*> (NP.decimal <* NP.char '-')
    <*> NP.decimal
  where
    idchar = NP.satisfy "id char" $ \c -> c == ' ' || isAlpha c

parseTicket :: NP.Parser Char Ticket
parseTicket = NP.sepBy1 NP.decimal (NP.char ',')

parseInput :: NP.Parser Char Input
parseInput = Input
    <$> (NP.sepBy1 parseConstraint (NP.char '\n') <* NP.string "\n\n")
    <*> (NP.string "your ticket:\n" *> parseTicket <* NP.string "\n\n")
    <*> (NP.string "nearby tickets:\n" *> NP.sepBy1 parseTicket (NP.char '\n'))

--------------------------------------------------------------------------------

check :: Int -> Constraint -> Bool
check x (Constraint _ lo0 hi0 lo1 hi1) =
    x >= lo0 && x <= hi0 || x >= lo1 && x <= hi1

orderConstraints :: [Ticket] -> [Constraint] -> Maybe [Constraint]
orderConstraints tickets constraints0 =
    solve (V.replicate (length constraints0) Nothing) constraints0
  where
    values = L.transpose tickets

    solve solved [] = sequence $ V.toList solved
    solve solved constraints
        | null certain = Nothing
        | otherwise    = solve
            (solved V.// [(i, Just c) | (i, c) <- certain])
            (filter (\c -> notElem c $ map snd certain) constraints)
      where
        certain = do
            (i, Nothing) <- zip [0 ..] $ V.toList solved
            let matches = [c | c <- constraints, all (`check` c) (values !! i)]
            case matches of
                [c] -> pure (i, c)
                _   -> []

--------------------------------------------------------------------------------

main :: IO ()
main = pureMain $ \inputstr -> do
    input <- NP.runParser parseInput inputstr

    let part1 :: [(Ticket, [Int])]  -- Tickets and definitely invalid numbers.
        part1 = do
            ticket <- inputNearbyTickets input
            let invalids = do
                    x <- ticket
                    guard . not . any (check x) $ inputConstraints input
                    pure x
            pure (ticket, invalids)

        tickets :: [Ticket]
        tickets = map fst $ filter (null . snd) part1

    pure
        ( pure . sum $ concatMap snd part1
        , do
            order <- maybe (Left "no solution") Right $
                orderConstraints tickets (inputConstraints input)
            pure . product $ do
                (i, Constraint name _ _ _ _) <- zip [0 ..] order
                guard $ "departure" `L.isPrefixOf` name
                pure $ inputYourTicket input !! i
        )

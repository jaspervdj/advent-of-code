import           AdventOfCode.Main
import qualified AdventOfCode.NanoParser as NP
import           Control.Applicative     (many, (<|>))
import Data.Maybe (maybeToList, catMaybes, fromMaybe)
import Data.List (foldl')
import           Control.Monad           (guard)
import qualified Data.Map                as M

--------------------------------------------------------------------------------

type Identifier = String

data Attribute
    = ExtremelyCoolLooking
    | Musical
    | Aerodynamic
    | Shiny
    deriving (Bounded, Enum, Eq, Ord, Show)

data Op = GreaterThan | LessThan deriving (Show)

data Ref a = Lit a | Ref Identifier deriving (Eq, Show)

data Rule = Rule Attribute Op Int (Ref Bool) deriving (Show)

data Workflow = Workflow String [Rule] (Ref Bool) deriving (Show)

type Workflows = M.Map Identifier Workflow

type Part a = M.Map Attribute a

data Input = Input Workflows [Part Int] deriving (Show)

--------------------------------------------------------------------------------

parseIdentifier :: NP.Parser Char Identifier
parseIdentifier = NP.many1 NP.alpha

parseAttribute :: NP.Parser Char Attribute
parseAttribute =
    (ExtremelyCoolLooking <$ NP.char 'x') <|>
    (Musical              <$ NP.char 'm') <|>
    (Aerodynamic          <$ NP.char 'a') <|>
    (Shiny                <$ NP.char 's')

parseOp :: NP.Parser Char Op
parseOp =
    (GreaterThan <$ NP.char '>') <|>
    (LessThan    <$ NP.char '<')

parseAccept :: NP.Parser Char Bool
parseAccept = (True <$ NP.char 'A') <|> (False <$ NP.char 'R')

parseRef :: NP.Parser Char a -> NP.Parser Char (Ref a)
parseRef p = (Lit <$> p) <|> (Ref <$> parseIdentifier)

parseRule :: NP.Parser Char Rule
parseRule = Rule
    <$> parseAttribute
    <*> parseOp
    <*> (NP.decimal <* NP.char ':')
    <*> parseRef parseAccept

parseWorkflow :: NP.Parser Char Workflow
parseWorkflow = Workflow
    <$> (parseIdentifier <* NP.char '{')
    <*> many (parseRule <* NP.char ',')
    <*> (parseRef parseAccept <* NP.char '}')

parsePart :: NP.Parser Char (Part Int)
parsePart = fmap M.fromList $
    NP.char '{' *> NP.sepBy attr (NP.char ',') <* NP.char '}'
  where
    attr = (,)
        <$> (parseAttribute <* NP.char '=')
        <*> NP.decimal

parseInput :: NP.Parser Char Input
parseInput = Input
    <$> (toMap <$> NP.many1 (parseWorkflow <* NP.spaces))
    <*> NP.many1 (parsePart <* NP.spaces)
  where
    toMap ws = M.fromList [(ident, w) | w@(Workflow ident _ _) <- ws]

--------------------------------------------------------------------------------

evalRef :: Workflows -> Ref Bool -> Part Int -> Bool
evalRef _         (Lit x)     _    = x
evalRef workflows (Ref ident) part =
    evalWorkflow workflows (workflows M.! ident) part

evalWorkflow :: Workflows -> Workflow -> Part Int -> Bool
evalWorkflow workflows (Workflow ident rules def) part = case rules of
    []     -> evalRef workflows def part
    r : rs -> fromMaybe
        (evalWorkflow workflows (Workflow ident rs def) part)
        (evalRule workflows r part)

evalRule :: Workflows -> Rule -> Part Int -> Maybe Bool
evalRule workflows (Rule attr op lit ret) part = do
    val <- M.lookup attr part
    guard $ case op of
        GreaterThan -> val > lit
        LessThan    -> val < lit
    pure $ evalRef workflows ret part

--------------------------------------------------------------------------------

data Range a = Range a a deriving (Eq, Show) -- Ordered, both inclusive.
newtype Ranges a = Ranges {unRanges :: [Range a]}

mkRange :: Integral a => a -> a -> Maybe (Range a)
mkRange x y
    | x <= y    = Just $ Range x y
    | otherwise = Nothing

range :: Integral a => a -> a -> Ranges a
range x y = Ranges $ maybeToList $ mkRange x y

empty :: Ranges a
empty = Ranges []

intersectRange :: Integral a => Range a -> Range a -> Maybe (Range a)
intersectRange (Range lo0 hi0) (Range lo1 hi1)
    | hi0 < lo1 = Nothing
    | lo0 > hi1 = Nothing
    | otherwise = Just $ Range (max lo0 lo1) (min hi0 hi1)

intersect :: Integral a => Ranges a -> Ranges a -> Ranges a
intersect (Ranges ls) (Ranges rs) = Ranges $ do
    l <- ls
    r <- rs
    maybeToList $ intersectRange l r

differenceRange :: Integral a => Range a -> Range a -> [Range a]
differenceRange (Range lo0 hi0) (Range lo1 hi1)
    -- Cut range is completely outside
    | hi0 < lo1  || lo0 > hi1  = [Range lo0 hi0]
    -- Cut range encompasses everything
    | lo0 >= lo1 && hi0 <= hi1 = []
    -- Cut range cuts left part
    | lo1 <= lo0               = maybeToList $ mkRange (hi1 + 1) hi0
    -- Cut range cuts right part
    | hi1 >= hi0               = maybeToList $ mkRange lo0 (lo1 - 1)
    -- Cuts in the middle
    | otherwise                = catMaybes
        [mkRange lo0 (lo1 - 1), mkRange (hi1 + 1) hi0]
    {-
    | lo0 < lo1 = Just $ Range lo0 (min hi0 (lo1 - 1))
    | hi0 > hi1 = Just $ Range (max (hi1 + 1) lo0) hi0
    -}

differenceRange_test =
    [ check (Range 5 10) (Range  0  1) [Range 5 10]
    , check (Range 5 10) (Range 11 12) [Range 5 10]
    , check (Range 5 10) (Range  8 12) [Range 5  7]
    , check (Range 5 10) (Range  0  5) [Range 6 10]
    , check (Range 5 10) (Range  6  8) [Range 5  5, Range 9 10]
    ]
  where
    check :: Range Int -> Range Int -> [Range Int] -> Bool
    check l r expect = differenceRange l r == expect

difference :: Integral a => Ranges a -> Ranges a -> Ranges a
difference (Ranges ls) (Ranges rs) = Ranges $ do
    l <- ls
    r <- rs
    differenceRange l r

unionRange :: Integral a => Range a -> Range a -> Maybe (Range a)
unionRange (Range lo0 hi0) (Range lo1 hi1)
    | hi0 + 1 < lo1 || lo0 > hi1 + 1 = Nothing
    | otherwise                      = Just $ Range (min lo0 lo1) (max hi0 hi1)

insertRange :: Integral a => Range a -> Ranges a -> Ranges a
insertRange x = Ranges . go [] . unRanges
  where
    go acc [] = x : acc
    go acc (y : ys) = case unionRange x y of
        Nothing -> go (y : acc) ys
        Just z  -> unRanges . insertRange z . Ranges $ acc ++ ys

union :: Integral a => Ranges a -> Ranges a -> Ranges a
union xs = foldl' (\ys x -> insertRange x ys) xs . unRanges

size :: Num a => Ranges a -> a
size (Ranges rs) = sum [hi - lo + 1 | Range lo hi <- rs]

--------------------------------------------------------------------------------

attributeLowerBound, attributeUpperBound :: Int
attributeLowerBound = 1
attributeUpperBound = 4000

acceptsAll :: Part (Ranges Int)
acceptsAll = M.fromList $ do
    attr <- [minBound .. maxBound]
    pure (attr, range attributeLowerBound attributeUpperBound)

acceptsRef :: Workflows -> Part (Ranges Int) -> Ref Bool -> [Part (Ranges Int)]
acceptsRef _         rng (Lit x)     = [rng | x]
acceptsRef workflows rng (Ref ident) =
    acceptsWorkflow workflows rng $ workflows M.! ident

acceptsWorkflow
    :: Workflows -> Part (Ranges Int) -> Workflow -> [Part (Ranges Int)]
acceptsWorkflow workflows rng (Workflow _ [] def) =
    acceptsRef workflows rng def
acceptsWorkflow workflows rng (Workflow ident (rule : rules) def) =
    acceptsRef workflows lhs ref <>
    acceptsWorkflow workflows rhs (Workflow ident rules def)
  where
    Rule attr op lit ref = rule
    lhs                  = M.adjust (`intersect` match) attr rng
    rhs                  = M.adjust (`difference` match) attr rng

    match = case op of
        GreaterThan -> range (lit + 1) attributeUpperBound
        LessThan    -> range attributeLowerBound (lit - 1)

--------------------------------------------------------------------------------

main :: IO ()
main = pureMain $ \str -> do
    Input workflows parts <- NP.runParser parseInput str
    let part1 = sum $ do
            part <- parts
            guard $ evalRef workflows (Ref "in") part
            pure $ sum part
        part2 = sum . fmap (product . fmap size) $
            acceptsRef workflows acceptsAll (Ref "in")
    pure (pure part1, pure part2)

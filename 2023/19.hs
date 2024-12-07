import           AdventOfCode.Main
import qualified AdventOfCode.NanoParser as NP
import qualified AdventOfCode.Ranges     as R
import           Control.Applicative     (many, (<|>))
import           Control.Monad           (guard)
import           Data.Foldable           (toList)
import qualified Data.Map                as M
import           Data.Maybe              (fromMaybe)

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
parseIdentifier = toList <$> NP.many1 NP.alpha

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
    <$> (toMap . toList <$> NP.many1 (parseWorkflow <* NP.spaces))
    <*> (toList <$> NP.many1 (parsePart <* NP.spaces))
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

attributeLowerBound, attributeUpperBound :: Int
attributeLowerBound = 1
attributeUpperBound = 4000

acceptsAll :: Part (R.Ranges Int)
acceptsAll = M.fromList $ do
    attr <- [minBound .. maxBound]
    pure (attr, R.range attributeLowerBound attributeUpperBound)

acceptsRef
    :: Workflows -> Part (R.Ranges Int) -> Ref Bool -> [Part (R.Ranges Int)]
acceptsRef _         rng (Lit x)     = [rng | x]
acceptsRef workflows rng (Ref ident) =
    acceptsWorkflow workflows rng $ workflows M.! ident

acceptsWorkflow
    :: Workflows -> Part (R.Ranges Int) -> Workflow -> [Part (R.Ranges Int)]
acceptsWorkflow workflows rng (Workflow _ [] def) =
    acceptsRef workflows rng def
acceptsWorkflow workflows rng (Workflow ident (rule : rules) def) =
    acceptsRef workflows lhs ref <>
    acceptsWorkflow workflows rhs (Workflow ident rules def)
  where
    Rule attr op lit ref = rule
    lhs                  = M.adjust (`R.intersection` match) attr rng
    rhs                  = M.adjust (`R.difference`   match) attr rng

    match = case op of
        GreaterThan -> R.range (lit + 1) attributeUpperBound
        LessThan    -> R.range attributeLowerBound (lit - 1)

--------------------------------------------------------------------------------

main :: IO ()
main = pureMain $ \str -> do
    Input workflows parts <- NP.runParser parseInput str
    let part1 = sum $ do
            part <- parts
            guard $ evalRef workflows (Ref "in") part
            pure $ sum part
        part2 = sum . fmap (product . fmap R.size) $
            acceptsRef workflows acceptsAll (Ref "in")
    pure (pure part1, pure part2)

import           AdventOfCode.Main
import qualified AdventOfCode.NanoParser as NP
import           Control.Applicative     (many, (<|>))
import           Control.Monad           (guard)
import qualified Data.Map                as M

--------------------------------------------------------------------------------

type Identifier = String

data Attribute
    = ExtremelyCoolLooking
    | Musical
    | Aerodynamic
    | Shiny
    deriving (Eq, Show)

data Op = GreaterThan | LessThan deriving (Show)

data Target = Accept | Reject | RuleTarget Identifier deriving (Eq, Show)

data Rule = Rule Attribute Op Int Target deriving (Show)

data Workflow = Workflow String [Rule] Target deriving (Show)

type Workflows = M.Map Identifier Workflow

type Part = [(Attribute, Int)]

data Input = Input Workflows [Part] deriving (Show)

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

parseTarget :: NP.Parser Char Target
parseTarget =
    (Accept     <$  NP.char 'A') <|>
    (Reject     <$  NP.char 'R') <|>
    (RuleTarget <$> parseIdentifier)

parseRule :: NP.Parser Char Rule
parseRule = Rule
    <$> parseAttribute
    <*> parseOp
    <*> (NP.decimal <* NP.char ':')
    <*> parseTarget

parseWorkflow :: NP.Parser Char Workflow
parseWorkflow = Workflow
    <$> (parseIdentifier <* NP.char '{')
    <*> many (parseRule <* NP.char ',')
    <*> (parseTarget <* NP.char '}')

parsePart :: NP.Parser Char Part
parsePart = NP.char '{' *> NP.sepBy attr (NP.char ',') <* NP.char '}'
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

evalWorkflows :: Part -> Workflows -> Target
evalWorkflows part workflows = go (RuleTarget "in")
  where
    go Accept             = Accept
    go Reject             = Reject
    go (RuleTarget ident) = case M.lookup ident workflows of
        Nothing -> error $ "workflow " <> ident <> " not found"
        Just w  -> go $ evalWorkflow part w

evalWorkflow :: Part -> Workflow -> Target
evalWorkflow _ (Workflow _ [] def) = def
evalWorkflow part (Workflow ident (rule : rules) def) =
    case evalRule part rule of
        Just target -> target
        Nothing     -> evalWorkflow part (Workflow ident rules def)

evalRule :: Part -> Rule -> Maybe Target
evalRule part (Rule attr op lit target) = do
    val <- lookup attr part
    guard $ case op of
        GreaterThan -> val > lit
        LessThan    -> val < lit
    pure target

--------------------------------------------------------------------------------

partSum :: Part -> Int
partSum = sum . map snd

main :: IO ()
main = pureMain $ \str -> do
    Input workflows parts <- NP.runParser parseInput str
    let part1 = sum $ do
            part <- parts
            guard $ evalWorkflows part workflows == Accept
            pure $ partSum part
    pure (pure "ok", pure (show part1))

-- | I was mainly trying to roll an Applicative-only parser.
{-# LANGUAGE DeriveFunctor #-}
import           Control.Applicative (Alternative (..))
import           Data.List           (intercalate)

data ParseResult t a
    = ParseSuccess !a !Int [t]
    | ParseError [(Int, String)]
    deriving (Functor, Show)

newtype Parser t a = Parser (Int -> [t] -> ParseResult t a)

instance Functor (Parser t) where
    fmap f (Parser g) = Parser (\i ts -> fmap f (g i ts))

instance Applicative (Parser t) where
    pure x                = Parser (\i ts -> ParseSuccess x i ts)
    Parser f <*> Parser g = Parser (\i ts ->
        case f i ts of
            ParseError errs        -> ParseError errs
            ParseSuccess x i' ts'  -> case g i' ts' of
                ParseError errs         -> ParseError errs
                ParseSuccess y i'' ts'' -> ParseSuccess (x y) i'' ts'')

instance Alternative (Parser t) where
    empty                 = Parser (\_ _ -> ParseError [])
    Parser f <|> Parser g = Parser (\i ts ->
        case f i ts of
            success@(ParseSuccess _ _ _) -> success
            ParseError errs1             -> case g i ts of
                success@(ParseSuccess _ _ _) -> success
                ParseError errs2             -> ParseError (errs1 ++ errs2))

anychar :: Parser t t
anychar = Parser (\i ts -> case ts of
    []        -> ParseError [(i, "any character")]
    (t : ts') -> ParseSuccess t (i + 1) ts')

char :: (Eq t, Show t) => t -> Parser t ()
char c = Parser (\i ts -> case ts of
    (t : ts') | t == c -> ParseSuccess () (i + 1) ts'
    _         -> ParseError [(i, show c)])

runParser :: Parser t a -> [t] -> Either String a
runParser (Parser g) ts = case g 0 ts of
    ParseSuccess x _ _ -> Right x
    ParseError errs    -> Left $ "Expecting " ++ intercalate " OR "
        [ err ++ " at position " ++ show i
        | (i, err) <- errs
        ]

data Group
    = Group [Group]
    | Garbage String
    deriving (Show)

group :: Parser Char Group
group =
    (Garbage <$> (char '<' *> garbage)) <|>
    (Group   <$> (char '{' *> commaSep group <* char '}'))
  where
    garbage =
        (char '!' *> anychar *> garbage) <|>
        (char '>' *> pure []) <|>
        ((:) <$> anychar <*> garbage)

    commaSep p =
        ((:) <$> p <*> many (char ',' *> p)) <|> pure []

score :: Group -> Int
score = go 1
  where
    go level (Group stream) = level + sum (map (go (level + 1)) stream)
    go _     (Garbage _)    = 0

count :: Group -> Int
count (Garbage str)  = length str
count (Group stream) = sum (map count stream)

main :: IO ()
main = do
    input <- getContents
    case runParser group input of
        Left err -> fail $ "Parse failed: " ++ err
        Right x  -> do
            putStrLn $ "Score: " ++ show (score x)
            putStrLn $ "Garbage count: " ++ show (count x)

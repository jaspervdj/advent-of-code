{-# LANGUAGE DeriveFunctor #-}
module AdventOfCode.NanoParser
    ( Parser

    , anyChar
    , satisfy
    , satisfyMaybe
    , char
    , string

    , many1
    , sepBy
    , sepBy1
    , chainl1

    , alpha
    , alphaNum
    , newline
    , digit
    , lower
    , upper
    , spaces
    , horizontalSpaces
    , decimal
    , signedDecimal

    , runParser
    , hRunParser
    ) where

import           Control.Applicative (Alternative (..), optional)
import           Control.Monad       (void)
import qualified Data.Char           as Char
import           Data.Foldable       (toList)
import           Data.Functor        (($>))
import           Data.List           (foldl', intercalate)
import           Data.List.NonEmpty  (NonEmpty (..))
import           Data.Maybe          (fromMaybe)
import qualified System.IO           as IO

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

satisfyMaybe :: String -> (t -> Maybe a) -> Parser t a
satisfyMaybe descr p = Parser (\i ts -> case ts of
    (t : ts') | Just x <- p t -> ParseSuccess x (i + 1) ts'
    _                         -> ParseError [(i, descr)])

satisfy :: String -> (t -> Bool) -> Parser t t
satisfy descr p = satisfyMaybe descr (\t -> if p t then Just t else Nothing)

anyChar :: Parser t t
anyChar = satisfy "any character" (const True)

char :: (Eq t, Show t) => t -> Parser t ()
char c = void $ satisfy (show c) (== c)

string :: (Eq t, Show t) => [t] -> Parser t ()
string []       = pure ()
string (x : xs) = char x *> string xs

many1 :: Parser t a -> Parser t (NonEmpty a)
many1 p = (:|) <$> p <*> many p

sepBy :: Parser t a -> Parser t b -> Parser t [a]
sepBy p s = (toList <$> sepBy1 p s) <|> pure []

sepBy1 :: Parser t a -> Parser t b -> Parser t (NonEmpty a)
sepBy1 p s = (:|) <$> p <*> many (s *> p)

-- | Parse a left-associative chain of terms and operators.
chainl1 :: Parser t a -> Parser t (a -> a -> a) -> Parser t a
chainl1 t op = foldl' (\x (f, y) -> f x y) <$> t <*> many ((,) <$> op <*> t)

alpha :: Parser Char Char
alpha = satisfy "alpha" Char.isAlpha

alphaNum :: Parser Char Char
alphaNum = satisfy "alphaNum" (\c -> Char.isAlpha c || Char.isDigit c)

newline :: Parser Char ()
newline = char '\n' <|> (char '\r' *> char '\n')

digit :: Parser Char Char
digit = satisfy "digit" Char.isDigit

lower :: Parser Char Char
lower = satisfy "lower" Char.isLower

upper :: Parser Char Char
upper = satisfy "upper" Char.isUpper

spaces :: Parser Char ()
spaces = void $ many $ satisfy "whitespace" Char.isSpace

horizontalSpaces :: Parser Char ()
horizontalSpaces = void . many $ satisfy "horizontal whitespace" $ \c ->
    Char.isSpace c && c /= '\n' && c /= '\r'

decimal :: (Integral a, Read a) => Parser Char a
decimal = read . toList <$> many1 digit

signedDecimal :: Parser Char Int
signedDecimal = fmap (fromMaybe id) (optional (char '-' $> negate)) <*> decimal

runParser :: Parser t a -> [t] -> Either String a
runParser (Parser g) ts = case g 0 ts of
    ParseSuccess x _ _ -> Right x
    ParseError errs    -> Left $ "Expecting " ++ intercalate " OR "
        [ err ++ " at position " ++ show i
        | (i, err) <- errs
        ]

hRunParser :: IO.Handle -> Parser Char a -> IO a
hRunParser h p = IO.hGetContents h >>= either fail pure . runParser p

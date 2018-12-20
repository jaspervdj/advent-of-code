{-# LANGUAGE DeriveFunctor #-}
module AdventOfCode.NanoParser
    ( Parser
    , anyChar
    , char
    , sepBy
    , runParser
    ) where

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

anyChar :: Parser t t
anyChar = Parser (\i ts -> case ts of
    []        -> ParseError [(i, "any character")]
    (t : ts') -> ParseSuccess t (i + 1) ts')

char :: (Eq t, Show t) => t -> Parser t ()
char c = Parser (\i ts -> case ts of
    (t : ts') | t == c -> ParseSuccess () (i + 1) ts'
    _                  -> ParseError [(i, show c)])

sepBy :: Parser t a -> Parser t b -> Parser t [a]
sepBy p s = ((:) <$> p <*> many (s *> p)) <|> pure []

runParser :: Parser t a -> [t] -> Either String a
runParser (Parser g) ts = case g 0 ts of
    ParseSuccess x _ _ -> Right x
    ParseError errs    -> Left $ "Expecting " ++ intercalate " OR "
        [ err ++ " at position " ++ show i
        | (i, err) <- errs
        ]


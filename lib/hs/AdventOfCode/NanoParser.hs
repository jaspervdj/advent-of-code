{-# LANGUAGE DeriveFunctor #-}
module AdventOfCode.NanoParser
    ( Parser
    , anyChar
    , satisfy
    , char
    , many1
    , sepBy
    , runParser
    ) where

import           Control.Applicative (Alternative (..))
import           Control.Monad       (void)
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

satisfy :: String -> (t -> Bool) -> Parser t t
satisfy descr p = Parser (\i ts -> case ts of
    (t : ts') | p t -> ParseSuccess t (i + 1) ts'
    _               -> ParseError [(i, descr)])

anyChar :: Parser t t
anyChar = satisfy "any character" (const True)

char :: (Eq t, Show t) => t -> Parser t ()
char c = void $ satisfy (show c) (== c)

many1 :: Parser t a -> Parser t [a]
many1 p = (:) <$> p <*> many p

sepBy :: Parser t a -> Parser t b -> Parser t [a]
sepBy p s = ((:) <$> p <*> many (s *> p)) <|> pure []

runParser :: Parser t a -> [t] -> Either String a
runParser (Parser g) ts = case g 0 ts of
    ParseSuccess x _ _ -> Right x
    ParseError errs    -> Left $ "Expecting " ++ intercalate " OR "
        [ err ++ " at position " ++ show i
        | (i, err) <- errs
        ]


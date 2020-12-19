{-# LANGUAGE DeriveFunctor #-}
import           AdventOfCode.Main
import qualified AdventOfCode.NanoParser as P
import           Control.Applicative     ((<|>))
import           Data.Functor.Compose
import           Data.Map                (Map)
import qualified Data.Map                as Map

data Rule c a = Or a a | Seq a a | Lit c | Ref a deriving (Functor, Show)

newtype Fix f = Fix {unFix :: f (Fix f)}
type ParsedRule c i = Fix (Compose (Rule c) (Either i))
type ResolvedRule c = Fix (Rule c)

parse :: P.Parser Char (Int, ParsedRule Char Int)
parse = (,) <$> P.decimal <* P.char ':' <* P.spaces <*>
    P.chainl1 term ((\x -> fc . fmap Right . Or x) <$ P.char '|' <* P.spaces)
  where
    fc   = Fix . Compose
    term = pseq <|> lit
    pseq = foldr1 (\x -> fc . fmap Right . Seq x) <$> P.many1 ref
    ref  = fc . Ref . Left <$> P.decimal <* P.spaces
    lit  = fc . Lit <$> (P.char '"' *> P.alpha <* P.char '"')

resolve :: Ord i => Map i (ParsedRule c i) -> Map i (ResolvedRule c)
resolve unresolved = resolved
  where
    resolved = fmap go unresolved
    go = Fix . fmap (either (resolved Map.!) id) . getCompose . fmap go . unFix

match :: Eq c => ResolvedRule c -> [c] -> Bool
match rule0 = any null . go rule0
  where
    go (Fix _)    []          = []
    go (Fix rule) str@(h : t) = case rule of
        Lit c   -> [t | h == c]
        Or x y  -> go x str ++ go y str
        Ref r   -> go r str
        Seq x y -> go x str >>= go y

main :: IO ()
main = pureMain $ \inputstr -> do
    let (rulesstr, messages) = fmap (drop 1) . break null $ lines inputstr
    rules0 <- Map.fromList <$> traverse (P.runParser parse) rulesstr
    pure
        ( pure . length $ filter (match (resolve rules0 Map.! 0)) messages
        , do
            extraRules <- Map.fromList <$> traverse (P.runParser parse)
                ["8: 42 | 42 8", "11: 42 31 | 42 11 31"]
            let rules1 = extraRules <> rules0
            pure . length $ filter (match (resolve rules1 Map.! 0)) messages
        )


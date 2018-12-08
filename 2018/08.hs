{-# LANGUAGE LambdaCase #-}
import           Control.Monad        (replicateM)
import           Control.Monad.Except (throwError)
import           Control.Monad.State  (StateT, evalStateT, get, put)
import           Data.List.Extended   ((!!?))
import           Data.Maybe           (maybeToList)
import qualified Data.Tree            as T

type Parser a = StateT [Int] (Either String) a

parseInt :: Parser Int
parseInt = get >>= \case
    []       -> throwError "parseInt: No numbers left"
    (x : xs) -> put xs >> return x

parseTree :: Parser (T.Tree [Int])
parseTree = do
    numChildren <- parseInt
    numEntries  <- parseInt
    children    <- replicateM numChildren parseTree
    entries     <- replicateM numEntries parseInt
    return $ T.Node entries children

value :: T.Tree [Int] -> Int
value (T.Node entries []) = sum entries
value (T.Node entries children) = sum $
    [value tree | i <- entries, tree <- maybeToList $ children !!? (i - 1)]

main :: IO ()
main = do
    numbers <- map read . words <$> getContents
    tree    <- either fail return $ evalStateT parseTree numbers
    print $ sum $ concat tree
    print $ value tree

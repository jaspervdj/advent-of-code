module Control.Monad.Extra
    ( ifM
    ) where

ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM mp mx my = mp >>= \p -> if p then mx else my

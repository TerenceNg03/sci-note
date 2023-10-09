module Utils (lift2) where
import Control.Monad.Trans (MonadTrans, lift)

lift2 :: (MonadTrans t1, MonadTrans t2, Monad m, Monad (t2 m)) => m a -> t1 (t2 m) a
lift2 a = lift $ lift a


module Flow where

import Control.Monad
    ((>=>))
import Prelude

-- What about splitting, joining, and cycling.
data Flow m i o where
    -- Linear composition of flows.
    Compose :: Flow m i a -> Flow m a o -> Flow m i o

    -- Parallel
    Join    :: Flow m i1 o1 -> Flow m i2 o2 -> Flow m (i1, i2) (o1, o2)

    -- Is this actually useful?
    Duplicate :: Flow m i o -> Flow m i (o, o)

    IfElse  :: Flow m i (Either a b) -> Flow m a o -> Flow m b o -> Flow m i o
    Alt     :: Flow m a b -> Flow m a c -> Flow m a (Either b c)

    -- While loop of sorts.
    Cycle   :: Flow m i (Either i o) -> Flow m i o

    Map     :: (o1 -> o2) -> Flow m i o1 -> Flow m i o2

    Pure    :: (a -> m b) -> Flow m a b

instance Functor m => Functor (Flow m i) where
    fmap f = \case
        Compose left right       -> Compose left (f <$> right)
        Join left right          -> Map f $ Join left right
        Duplicate inner          -> Map f $ Duplicate inner
        IfElse branch left right -> IfElse branch (f <$> left) (f <$> right)
        Alt left right           -> Map f $ Alt left right
        Cycle loop               -> Cycle (fmap f <$> loop)
        Map g inner              -> Map (f . g) inner
        Pure inner               -> Pure $ fmap f <$> inner

flow1 :: Int -> IO String
flow1 i = do
    putStrLn "f1"
    pure $ show i

flow2 :: String -> IO String
flow2 s = do
    putStrLn "f2"
    pure $ s <> "!"

flow :: Flow IO Int String
flow =
    Compose
        (Pure flow1)
        (Pure flow2)

process :: Monad m => Flow m a b -> a -> m b
process = \case
    Pure f -> f
    Compose f g -> process f >=> process g

run :: IO ()
run =
    process flow 1 >>= putStrLn


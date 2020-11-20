module Flow where

import Control.Monad
    ((>=>))
import Prelude

data Flow m i o where
    --    Compose
    -- i ---- a ---- o
    Compose :: Flow m i a -> Flow m a o -> Flow m i o

    --    Join
    -- i1 ---- o1----\
    --                ----- (o1, o2)
    -- i2 ---- o2 ---/
    Join    :: Flow m i1 o1 -> Flow m i2 o2 -> Flow m (i1, i2) (o1, o2)

    -- Is this actually useful?
    -- Duplicate         Join
    --            /---- i ---- o1----\
    -- i ---------                    ----- (o1, o2)
    --            \---- i ---- o2 ---/
    Duplicate :: Flow m i (i, i)

    --           IfElse
    --                   /---- Flow m a o ----\
    -- i ---- Either a b                       ---- o
    --                   \---- Flow m b o ----/
    IfElse  :: Flow m i (Either a b) -> Flow m a o -> Flow m b o -> Flow m i o

    --     Cycle
    -- /<<<<<<<<<<<<<\
    -- i ---- Either i o ---- o
    Cycle   :: Flow m i (Either i o) -> Flow m i o

    Map     :: (o1 -> o2) -> Flow m i o1 -> Flow m i o2

    Pure    :: (a -> m b) -> Flow m a b

instance Functor m => Functor (Flow m i) where
    fmap f = \case
        Compose left right       -> Compose left (f <$> right)
        Join left right          -> Map f $ Join left right
        Duplicate                -> Map f Duplicate
        IfElse branch left right -> IfElse branch (f <$> left) (f <$> right)
        Cycle loop               -> Cycle (fmap f <$> loop)
        Map g inner              -> Map (f . g) inner
        Pure inner               -> Pure $ fmap f <$> inner

flow1 :: Flow IO () String
flow1 = Pure $ const getLine

flow2 :: Flow IO String Int
flow2 = Pure $ pure . read

flow3 :: Flow IO (Int, Int) Int
flow3 = Pure $ pure . uncurry (+)

flow4 :: Flow IO Int String
flow4 = Pure $ pure . show

-- () ---- String ---- Int ----\
--                              ---- Int ---- String
-- () ---- String ---- Int ----/
flow :: Flow IO () String
flow =
    let
        u = Pure $ const (pure ())
        s0 = Compose u Duplicate
        s1 = flow1 `Compose` flow2
        s2 = Join s1 s1
        s3 = Compose s2 flow3
        s4 = Compose s3 flow4
    in Compose s0 s4

fib0 :: Monad m => () -> m (Int, Int)
fib0 _ = pure (1, 1)

fibNext :: Monad m => (Int, Int, Int) -> m (Either (Int, Int, Int) Int)
fibNext (n, a, b)
  | n <= 2 = pure $ Right b
  | otherwise = pure $ Left (n - 1, b, a + b)

fibFlow :: Flow IO () String
fibFlow =
    let
        s1 :: Flow IO () Int
        s1 = Compose flow1 flow2
        -- seed for fib
        s2 :: Flow IO () (Int, Int)
        s2 = Pure fib0
        -- all inputs, before map
        s3 :: Flow IO ((), ()) (Int, (Int, Int))
        s3 = Join s1 s2
        --
        s4 :: Flow IO ((), ()) (Int, Int, Int)
        s4 = (\(a, (b, c)) -> (a, b, c)) <$> s3
        --
        s5 :: Flow IO (Int, Int, Int) Int
        s5 = Cycle (Pure fibNext)
        --
        s6 :: Flow IO ((), ()) Int
        s6 = Compose s4 s5
    in Compose Duplicate (Compose s6 flow4)

--                              Duplicate
-- () ---- String ---- Int ---- (Int, Int) ---- Int ---- String
flow' :: Flow IO () String
flow' =
    let
        s0 = Compose flow2 Duplicate
        s1 = flow1 `Compose` s0
        s2 = Compose s1 flow3
        s3 = Compose s2 flow4
    in s3

runFlow :: Monad m => Flow m a b -> a -> m b
runFlow = \case
    Compose f g ->
        runFlow f >=> runFlow g
    Join left right -> \a -> do
        o1 <- runFlow left (fst a)
        o2 <- runFlow right (snd a)
        pure (o1, o2)
    Duplicate -> \i ->
        pure (i, i)
    IfElse branch left right ->
        runFlow branch >=> either (runFlow left) (runFlow right)
    l@(Cycle loop) ->
        runFlow loop >=> either (runFlow l) pure
    Map g inner ->
        fmap g . runFlow inner
    Pure f ->
        f

run :: IO ()
run = runFlow fibFlow () >>= putStrLn


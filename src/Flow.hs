{-# language Arrows #-}

module Flow where

import           Control.Applicative
    (liftA2)
import qualified Control.Arrow          as Arr
import qualified Control.Category       as Cat
import           Control.Monad
    ((>=>))
import           Data.Profunctor
    (Profunctor, dimap)
import           Data.Profunctor.Choice
    (Choice, left')
import           Data.Profunctor.Strong
    (Strong, first')
import           Prelude


data Flow m i o where
    -- This are the basic constructors.
    Eff    :: (a -> m b) -> Flow m a b
    Pure   :: (a ->   b) -> Flow m a b

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
    --       /---- i ----\
    -- i ----             ---- (i, i)
    --       \---- i ----/
    Duplicate :: Flow m i (i, i)

    --           IfElse
    --                       /---- a ---- o ----\
    -- i ---- Either a b ----                    ---- o
    --                       \---- b ---- o ----/
    IfElse  :: Flow m i (Either a b) -> Flow m a o -> Flow m b o -> Flow m i o

    --     Cycle
    -- /<<<<<<<<<<<<<\
    -- i ---- Either i o ---- o
    Cycle   :: Flow m i (Either i o) -> Flow m i o

    -- This just allows us to define a 'Functor' instance.
    -- Dimap   :: (i2 -> i1) -> (o1 -> o2) -> Flow m i1 o1 -> Flow m i2 o2


instance Functor (Flow m i) where
    fmap f = flip Compose (Pure f)

instance Applicative m => Applicative (Flow m i) where
    pure o = Eff $ const (pure o)

    --       / i ---- a ---- \
    -- i ----                  ---- c
    --       \ i ---- b ---- /
    liftA2 f f1 f2 =
        Compose Duplicate $ uncurry f <$> Join f1 f2

{-
    I don't think we want a monad instance. The `a -> Flow m i o` is
    problematic: it's some sort of `Flow` generator at runtime. This
    means a lot more power than I thought I would want to provide
    to the API for now.
-}

instance Cat.Category (Flow m) where
   id :: Flow m a a
   id = Pure id

   (.) :: Flow m b c -> Flow m a b -> Flow m a c
   (.) = flip Compose

instance Arr.Arrow (Flow m) where
    arr = Pure
    (***) = Join

instance Profunctor (Flow m) where
    dimap :: (b -> a) -> (c -> d) -> Flow m a c -> Flow m b d
    dimap f g p = Pure g Cat.. p Cat.. Pure f

instance Strong (Flow m) where
    first' :: Flow m i o -> Flow m (i, a) (o, a)
    first' f = Join f Cat.id

instance Choice (Flow m) where
    left' :: Flow m i o -> Flow m (Either i a) (Either o a)
    left' f = IfElse Cat.id (Left <$> f) (Pure Right)

--------------------------------------------------------------
-- Examples

input :: Flow IO () String
input = Eff $ const getLine

string2Int :: Flow IO String Int
string2Int = Pure read

add :: Flow IO (Int, Int) Int
add = Pure $ uncurry (+)

output :: Flow IO Int String
output = Pure show

{-
Cute pattern to remember/use.
-----------------------------

Given some flow `f :: Flow m a b`, if we do:

Compose Duplicate f :: Flow (a, a) b
Compose f Duplicate :: Flow a (b, b)

-}

-- () ---- String ---- Int ----\
--                              ---- (Int, Int) ---- Int ---- String
-- () ---- String ---- Int ----/
flow :: Flow IO () String
flow =
    let
        s1 = input Cat.>>> string2Int
        s2 = Join s1 s1
        s3 = s2 Cat.>>> add
        s4 = s3 Cat.>>> output
    in Duplicate Cat.>>> s4

-- Example contributed by Ibot02
arrowFlow :: Flow IO () String
arrowFlow =
    let readInt = input Cat.>>> string2Int
    in proc () -> do
        n <- readInt -< ()
        m <- readInt -< ()
        result <- add -< (n, m)
        output -< result

--                              Duplicate
-- () ---- String ---- Int ---- (Int, Int) ---- Int ---- String
flow' :: Flow IO () String
flow' =
    input
        Cat.>>> string2Int
        Cat.>>> Duplicate
        Cat.>>> add
        Cat.>>> output

-------------------------------------------------------------

fib0 :: () -> (Int, Int)
fib0 = const (0, 1)

fibNext :: (Int, Int, Int) -> Either (Int, Int, Int) Int
fibNext (n, a, b)
  | n <= 2    = Right b
  | otherwise = Left (n - 1, b, a + b)

fibFlow :: Flow IO () String
fibFlow =
    let
        -- read from console, convert to int
        readInt :: Flow IO () Int
        readInt = input Cat.>>> string2Int
        -- all inputs, before map
        curriedInputs :: Flow IO () (Int, (Int, Int))
        curriedInputs = Duplicate Cat.>>> Join readInt (Pure fib0)
        -- massage outputs
        inputs :: Flow IO () (Int, Int, Int)
        inputs = (\(a, (b, c)) -> (a, b, c)) <$> curriedInputs
        -- print
        fib :: Flow IO () Int
        fib = inputs Cat.>>> Cycle (Pure fibNext)
    in fib Cat.>>> output

-- Example contributed by Ibot02
fibArrow :: Flow IO () String
fibArrow =
    let readInt = input Cat.>>> string2Int
    in proc () -> do
        n <- readInt -< ()
        (i0, i1) <- Pure fib0 -< ()
        fib <- Cycle (Pure fibNext) -< (n, i0, i1)
        output -< fib

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
    Eff f ->
        f
    Pure f ->
        pure . f

run :: IO ()
run = runFlow fibArrow () >>= putStrLn


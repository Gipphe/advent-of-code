module Day
    ( SomeDay(..)
    , runSomeDay
    , Day
    , runDay'
    , runDay
    , Task
    , runTask'
    , runTask
    ) where

import Control.DeepSeq (NFData, force)
import Control.Exception (evaluate)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Proxy (Proxy(..))
import Data.Time.Clock.POSIX (getPOSIXTime)
import GHC.TypeLits (KnownNat, Nat, natVal)

---------------------------------------------
-- * Nifty monads to automate console logging
---------------------------------------------

-- | Exsistential type to hold each day, since their day numbers are a part of
-- their type.
data SomeDay = forall n . KnownNat n => SomeDay (Day n ())

runSomeDay :: SomeDay -> IO ()
runSomeDay (SomeDay day) = runDay day

-- | A given day with its day number.
newtype Day (n :: Nat) a = Day { runDay' :: IO a }
    deriving (Functor, Applicative, Monad, MonadIO) via IO

-- | Run a day, logging the day number.
runDay :: forall n a . KnownNat n => Day n a -> IO a
runDay m = do
    putStrLn $ "### Day " <> show (natVal (Proxy @n)) <> "\n"
    runDay' m

-- | A day's task with the task number. Usually 1 or 2, since Advent of Code
-- only has 2 tasks per day.
newtype Task (n :: Nat) a = Task { runTask' :: IO a }
    deriving (Functor, Applicative, Monad, MonadFail, MonadIO) via IO

-- | Run a task and time its execution, rather roughly, logging the execution
-- time.
runTask :: forall n m a
         . (KnownNat n, Show a, NFData a)
        => Task n a
        -> Day m ()
runTask task = do
    startTime <- liftIO getPOSIXTime
    !res      <- liftIO $ evaluate . force =<< runTask' task
    endTime   <- liftIO getPOSIXTime
    liftIO
        $  putStrLn
        $  "#### Task "
        <> taskNum
        <> "\n- Result: "
        <> show res
        <> "\n- "
        <> "Time: "
        <> show (endTime - startTime)
    where taskNum = show (natVal (Proxy @n))

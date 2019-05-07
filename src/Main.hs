module Main where

import           Data.List                                ( isInfixOf )
import           Data.Validation
import           Control.Monad.Random.Strict              ( RandomGen
                                                          , Rand
                                                          , RandT
                                                          , evalRandIO
                                                          , getRandomR
                                                          )
import qualified Data.List.NonEmpty            as NE
import           Data.List.NonEmpty                       ( NonEmpty )
import           Control.Monad.Except                     ( ExceptT
                                                          , runExceptT
                                                          , liftEither
                                                          )
import           Control.Monad.Identity                   ( Identity
                                                          , runIdentity
                                                          )
import           System.Random                            ( getStdGen )

newtype AtString = AtString String deriving Show
newtype PeriodString = PeriodString String deriving Show
newtype NonEmptyString = NonEmptyString String deriving Show

newtype Email = Email String deriving Show

data VError = MustNotBeEmpty
            | MustContainAt
            | MustContainPeriod
            deriving (Show)

data Day = Monday
         | Tuesday
         | Wednesday
         | Thursday
         | Friday
         | Saturday
         | Sunday
         deriving (Show, Enum)

data Appointment = Appointment
  { email :: Email
  , day   :: Day
  } deriving Show

days :: NonEmpty Day
days = NE.fromList (enumFrom $ toEnum 0)

choose :: RandomGen g => NonEmpty a -> Rand g a
choose xs = (xs NE.!!) <$> getRandomR (0, length xs - 1)

atString :: String -> Validation [VError] AtString
atString x =
  if "@" `isInfixOf` x then Success (AtString x) else Failure [MustContainAt]

periodString :: String -> Validation [VError] PeriodString
periodString x = if "." `isInfixOf` x
  then Success (PeriodString x)
  else Failure [MustContainPeriod]

nonEmptyString :: String -> Validation [VError] NonEmptyString
nonEmptyString x =
  if x /= [] then Success (NonEmptyString x) else Failure [MustNotBeEmpty]

mkEmail :: String -> Validation [VError] Email
mkEmail x = Email x <$ nonEmptyString x <* atString x <* periodString x

chooseDay :: RandomGen g => Rand g Day
chooseDay = choose days

type App a = ExceptT [VError] Identity a

runApp :: App a -> Either [VError] a
runApp = runIdentity . runExceptT

app1 :: String -> App Appointment
app1 e = do
  e' <- liftEither (toEither $ mkEmail e)
  return $ Appointment e' Monday

main :: IO ()
main = do
  g <- getStdGen
  print $ runApp (app1 "bob@gmail.com")
  print $ runApp (app1 "bobgmail.com")
  print $ runApp (app1 "bob@gmailcom")
  print $ runApp (app1 "")

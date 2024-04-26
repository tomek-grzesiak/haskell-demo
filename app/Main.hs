module Main where

import Data.Aeson
import Lib (test)
import Protolude
import TextShow
import TextShow.Generic
import Control.Monad.Except.CoHas (CoHas, inject)

main :: IO ()
main = do
  -- void $ flip runReaderT Ctx2 $ runExceptT test3
  print $ encode $ Err2 . Err1 $ "Hello, Haskell!"

data Err1 = Err1 Text 
    deriving (Generic, ToJSON, FromJSON)
    deriving TextShow  via FromGeneric  (Err1)

data Err2 = Err2
  { err1 :: Err1
  }
  deriving (Generic, ToJSON, FromJSON, CoHas Err1)
  deriving TextShow  via FromGeneric  (Err2)

data Ctx = Ctx

test1 :: ExceptT Err2 (ReaderT Ctx IO) ()
test1 = undefined

test2 :: (MonadError  e m, CoHas Err1 e , MonadReader Ctx m) => m ()
test2 = throwError . inject $ Err1 "err1"

test3 :: ExceptT Err2 (ReaderT Ctx IO) ()
test3 = do
  test1
  test2
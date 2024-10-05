module Main where

import Data.Aeson
import Lib (test)
import Protolude hiding (ask)
import TextShow
import TextShow.Generic
import Control.Monad.Except.CoHas (CoHas, inject)
import Control.Monad.Reader.Has

main :: IO ()
main = do
  res <-  flip runReaderT Ctx {ctx1 = Ctx1, ctx2 = Ctx2} $ runExceptT test3
  print .  encode $ res 

newtype Err1 = Err1 Text 
    deriving (Generic, ToJSON, FromJSON)
    deriving TextShow  via FromGeneric  Err1

data Err2 = Err2
  { err1 :: Err1
  }
  deriving (Generic, ToJSON, FromJSON, CoHas Err1)
  deriving TextShow  via FromGeneric  Err2

data Ctx = Ctx {
    ctx1 :: Ctx1
  , ctx2 :: Ctx2
} deriving (Generic, Has Ctx1, Has Ctx2)

data Ctx1 = Ctx1
  deriving (Generic, ToJSON, FromJSON)
  deriving TextShow  via FromGeneric  Ctx1
data Ctx2 = Ctx2
  deriving (Generic, ToJSON, FromJSON)
  deriving TextShow  via FromGeneric  Ctx2


test1 :: ExceptT Err2 (ReaderT Ctx IO) ()
test1 = throwError $ Err2 { err1 = Err1 "err" }

class HasCtx

test2 :: (Has Ctx1 ctx, MonadError e m, CoHas Err1 e , MonadReader ctx m) => m ()
test2 = do
  ctx :: Ctx1 <- ask
  throwError . inject $ Err1 "err1"

test3 :: ExceptT Err2 (ReaderT Ctx IO) ()
test3 = do
  test1
  test2

{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}

import GHC.Generics
import Protolude hiding (get, put)

class Encrypt a where
  enc :: a -> a
  default enc :: (Generic a, GEncrypt (Rep a)) => a -> a
  enc a = to $ genc (from a)

class GEncrypt f where
  genc :: f a -> f a

instance (GEncrypt a, GEncrypt b) => GEncrypt (a :*: b) where
  genc (a :*: b) = genc a :*: genc b

instance (GEncrypt a, GEncrypt b) => GEncrypt (a :+: b) where
  genc (L1 x) = L1 $ genc x
  genc (R1 x) = R1 $ genc x

instance (GEncrypt a) => GEncrypt (M1 i c a) where
  genc (M1 x) = M1 $ genc x

instance (Encrypt a) => GEncrypt (K1 i a) where
  genc (K1 x) = K1 $ enc x

instance GEncrypt U1 where
  genc U1 = U1

data Test = Test Int ByteString
  deriving (Generic, Show)

instance Encrypt Int where
  enc = identity

instance Encrypt ByteString where
  enc x = ("enc" :: ByteString) <> x

instance Encrypt Test

main :: IO ()
main = print . enc $ Test 1 "Tomek"
{-# LANGUAGE DefaultSignatures, DeriveGeneric, TypeOperators, TypeFamilies #-}

import GHC.Generics
import Protolude hiding (put, get)

class Encrypt a where
  enc :: a -> a

  default enc :: (Generic a, GEncrypt (Rep a), GEnc (Rep a) ~ Rep a) =>  a -> a
  enc a = to $ genc (from a)

class GEncrypt (f :: Type -> Type) where
  type GEnc f :: Type -> Type
  genc :: f a -> GEnc f a


-- | Products: encode multiple arguments to constructors
instance (GEncrypt a, GEncrypt b) => GEncrypt (a :*: b) where
  type GEnc (a :*: b) = GEnc a :*: GEnc b
  genc (a :*: b) = genc a :*: genc b

-- | Sums: encode choice between constructors
instance (GEncrypt a, GEncrypt b) => GEncrypt (a :+: b) where
  type GEnc (a :+: b) = GEnc a :+: GEnc b
  genc (L1 x) = L1 $ genc x
  genc (R1 x) = R1 $ genc x

-- | Meta-information (constructor names, etc.)
instance (GEncrypt a) => GEncrypt (M1 i c a) where
  type GEnc (M1 i c a) = M1 i c (GEnc a)
  genc (M1 x) = M1 $ genc x
-- | Constants, additional parameters and recursion of kind *
instance (Encrypt a) => GEncrypt (K1 i a) where
  type GEnc(K1 i a) = K1 i a
  genc (K1 x) = K1 $ enc x


--
-- Try it out. (Normally this would be in a separate module.)
--

data Test = Test Int ByteString
  deriving (Generic, Show)

instance Encrypt Int where
  enc = identity

instance Encrypt ByteString where
  enc x = ("enc" :: ByteString) <> x

instance Encrypt Test

main :: IO ()
main = print . enc $ Test 1 "Tomek"
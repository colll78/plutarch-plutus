module Plutarch.Script (Script (..), serialiseScript, deserialiseScript, hashScriptWithPrefix) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Short (ShortByteString, fromShort)
import Data.Word (Word8)
import GHC.Generics (Generic)
import PlutusCore.Crypto.Hash qualified as Hash
import PlutusLedgerApi.Common (serialiseUPLC, uncheckedDeserialiseUPLC)
import UntypedPlutusCore qualified as UPLC

newtype Script = Script {unScript :: UPLC.Program UPLC.DeBruijn UPLC.DefaultUni UPLC.DefaultFun ()}
  deriving newtype (Eq)
  deriving stock (Show, Generic)

serialiseScript :: Script -> ShortByteString
serialiseScript = serialiseUPLC . unScript

deserialiseScript :: ShortByteString -> Script
deserialiseScript = Script . uncheckedDeserialiseUPLC

hashScriptWithPrefix :: Word8 -> Script -> ByteString
hashScriptWithPrefix prefix scr =
  Hash.blake2b_224 $
    BS.singleton prefix <> (fromShort . serialiseUPLC . unScript $ scr)

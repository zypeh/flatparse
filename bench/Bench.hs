{-# options_ghc -Wno-unused-imports #-}

module Main where

import Data.Primitive.ByteArray
import Gauge

import qualified Data.ByteString.Char8 as B
import qualified Z.Data.Vector         as Vec

import qualified Attoparsec
import qualified Megaparsec
import qualified Parsec
import qualified FPStateful
import qualified FPBasic
import qualified Bytesmith
import qualified ZDataParser
import qualified ReadInteger

sexpInp :: B.ByteString
sexpInp =
  B.concat $ "(" : replicate 33333 "(foo (foo (foo ((bar baza)))))" ++ [")"]

longwsInp :: B.ByteString
longwsInp = B.concat $ replicate 55555 "thisisalongkeyword   "

numcsvInp :: B.ByteString
numcsvInp = B.concat ("0" : [B.pack (",  " ++ show n) | n <- [1..100000::Int]])

sexpInp' :: ByteArray
sexpInp' = Bytesmith.strToByteArray $ B.unpack sexpInp

longwsInp' :: ByteArray
longwsInp' = Bytesmith.strToByteArray $ B.unpack longwsInp

numcsvInp' :: ByteArray
numcsvInp' = Bytesmith.strToByteArray $ B.unpack numcsvInp

sexpInpBytes :: Vec.Bytes
sexpInpBytes = Vec.packASCII $ B.unpack numcsvInp

longwsInpBytes :: Vec.Bytes
longwsInpBytes = Vec.packASCII $ B.unpack longwsInp

numcsvInpBytes :: Vec.Bytes
numcsvInpBytes = Vec.packASCII $ B.unpack numcsvInp

readIntInp :: B.ByteString
readIntInp = "12345678910"

main :: IO ()
main = defaultMain [
  bgroup "sexp" [
    bench "fpbasic"     $ whnf FPBasic.runSexp     sexpInp,
    bench "fpstateful"  $ whnf FPStateful.runSexp  sexpInp,
    bench "bytesmith"   $ whnf Bytesmith.runSexp   sexpInp',
    bench "attoparsec"  $ whnf Attoparsec.runSexp  sexpInp,
    bench "megaparsec"  $ whnf Megaparsec.runSexp  sexpInp,
    bench "parsec"      $ whnf Parsec.runSexp      sexpInp,
    bench "ZDataParser" $ whnf ZDataParser.runSexp sexpInpBytes
  ],

  bgroup "long keyword" [
    bench "fpbasic"     $ whnf FPBasic.runLongws     longwsInp,
    bench "fpstateful"  $ whnf FPStateful.runLongws  longwsInp,
    bench "bytesmith"   $ whnf Bytesmith.runLongws   longwsInp',
    bench "attoparsec"  $ whnf Attoparsec.runLongws  longwsInp,
    bench "megaparsec"  $ whnf Megaparsec.runLongws  longwsInp,
    bench "parsec"      $ whnf Parsec.runLongws      longwsInp,
    bench "ZDataParser" $ whnf ZDataParser.runLongws longwsInpBytes
  ],

  bgroup "numeral csv" [
    bench "fpbasic"     $ whnf FPBasic.runNumcsv     numcsvInp,
    bench "fpstateful"  $ whnf FPStateful.runNumcsv  numcsvInp,
    bench "bytesmith"   $ whnf Bytesmith.runNumcsv   numcsvInp',
    bench "attoparsec"  $ whnf Attoparsec.runNumcsv  numcsvInp,
    bench "megaparsec"  $ whnf Megaparsec.runNumcsv  numcsvInp,
    bench "parsec"      $ whnf Parsec.runNumcsv      numcsvInp,
    bench "ZDataParser" $ whnf ZDataParser.runNumcsv numcsvInpBytes
  ],

  bgroup "readInt/readInteger" [
    bench "readInt"      $ whnf ReadInteger.readInt     readIntInp,
    bench "readInteger"  $ whnf ReadInteger.readInteger readIntInp
    ]
 ]

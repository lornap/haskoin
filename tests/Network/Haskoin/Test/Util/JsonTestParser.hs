{-|

Module containing parsing helper functions to read the *.json test files
taken from the Bitcoin Core project.  This are use in tests.

-}

module Network.Haskoin.Test.Util.JsonTestParser (
  parseScript
, parseFlags
) where

import qualified Data.ByteString.Lazy as LBS
    ( pack
    , unpack
    )

import qualified Data.ByteString as BS ( pack )
import Data.Maybe ( catMaybes )
import Data.List (isPrefixOf)
import Data.List.Split ( splitOn )

import Data.Binary
    ( Word8
    , encode
    , decode
    , decodeOrFail)

import Text.Read (readMaybe)
import Data.Char (ord)
import Data.Int (Int64)

import Control.Applicative ((<$>))
import Control.Monad (when)

import Numeric (readHex)

import Network.Haskoin.Script
import Network.Haskoin.Internals ( encodeInt, Flag )

type ParseError = String

parseHex' :: String -> Maybe [Word8]
parseHex' (a:b:xs) = case readHex $ [a, b] :: [(Integer, String)] of
                      [(i, "")] -> case parseHex' xs of
                                    Just ops -> Just $ fromIntegral i:ops
                                    Nothing -> Nothing
                      _ -> Nothing
parseHex' [_] = Nothing
parseHex' [] = Just []

parseScript :: String -> Either ParseError Script
parseScript scriptString =
      do bytes <- LBS.pack <$> parseBytes scriptString
         script <- decodeScript bytes
         when (encode script /= bytes) $
            Left "encode script /= bytes"
         when (decode (encode script) /= script) $
            Left "decode (encode script) /= script"
         return script
      where
          decodeScript bytes = case decodeOrFail bytes of
            Left (_, _, e) -> Left $ "decode error: " ++ e
            Right (_, _, Script s) -> Right $ Script s
          parseBytes :: String -> Either ParseError [Word8]
          parseBytes string = concat <$> mapM parseToken (words string)
          parseToken :: String -> Either ParseError [Word8]
          parseToken tok =
              case alternatives of
                    (ops:_) -> Right ops
                    _ -> Left $ "unknown token " ++ tok
              where alternatives :: [[Word8]]
                    alternatives = catMaybes  [ parseHex
                                              , parseInt
                                              , parseQuote
                                              , parseOp
                                              ]
                    parseHex | "0x" `isPrefixOf` tok = parseHex' (drop 2 tok)
                             | otherwise = Nothing
                    parseInt = fromInt . fromIntegral <$>
                               (readMaybe tok :: Maybe Integer)
                    parseQuote | tok == "''" = Just [0]
                               | (head tok) == '\'' && (last tok) == '\'' =
                                 Just $ encodeBytes $ opPushData $ BS.pack
                                      $ map (fromIntegral . ord)
                                      $ init . tail $ tok
                               | otherwise = Nothing
                    fromInt :: Int64 -> [Word8]
                    fromInt n | n ==  0 = [0x00]
                              | n == -1 = [0x4f]
                              | 1 <= n && n <= 16 = [0x50 + fromIntegral n]
                              | otherwise = encodeBytes
                                                $ opPushData $ BS.pack
                                                $ encodeInt n
                    parseOp = encodeBytes <$> (readMaybe $ "OP_" ++ tok)
                    encodeBytes = LBS.unpack . encode

parseFlags :: String -> [ Flag ]
parseFlags "" = []
parseFlags s = map read . splitOn "," $ s

module Network.Haskoin.Validation.Transaction
(
  checkTransaction
)
where

import Network.Haskoin.Transaction ( Tx(..)
                                   , TxIn(..)
                                   , TxOut(..)
                                   )
import Network.Haskoin.Util ( encode' )
import Network.Haskoin.Constants (maxSatoshi, maxBlockSize)

import qualified Data.ByteString as BS
import Control.Monad
import Data.List ( sort )

import Network.Haskoin.Validation.Error

checkNoDuplicates :: ( Ord a ) => [a] -> Bool
checkNoDuplicates as = let sortedAs = sort as in
  sortedCheckNoDuplicates sortedAs

sortedCheckNoDuplicates :: ( Eq a ) => [ a ] -> Bool
sortedCheckNoDuplicates [] = True
sortedCheckNoDuplicates [_] = True
sortedCheckNoDuplicates (a:b:bs) = a /= b && sortedCheckNoDuplicates (b:bs)

isEmpty :: [a]->Bool
isEmpty [] = True
isEmpty _  = False

checkTxOutput :: TxOut -> VError Bool
checkTxOutput out = do
  if outValue out > maxSatoshi
    then throwError $ TxInvalid "Txn output too large"
    else return True

-- | Transaction checks that don't depend on context ( no-mempool,
-- block chain queries )
checkTransaction :: Tx -> VError Bool
checkTransaction tx = do
  if isEmpty . txIn $ tx
    then throwError $ TxInvalid "txIn empty"
    else return ()

  if isEmpty. txOut $ tx
    then throwError $ TxInvalid "txOut empty"
    else return ()
  
  if ( BS.length . encode' $ tx ) > maxBlockSize
     then throwError $ TxInvalid "size limits failed"
     else return ()

  forM_ ( txOut tx ) checkTxOutput

  let totOutput = sum . ( map outValue ) $ txOut tx
  if totOutput > maxSatoshi
    then throwError $ TxInvalid "Total output sum too large"
    else return ()

  if not . checkNoDuplicates $ map prevOutput $ txIn tx
    then throwError $ TxInvalid "Duplicate input points"
    else return ()

  return True
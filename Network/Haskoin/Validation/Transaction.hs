{-|
  Validation routines for transactions.
-}

module Network.Haskoin.Validation.Transaction
(
  checkTransaction
, checkCBTransaction
, validateAllInputs
)
where

import Network.Haskoin.Transaction ( Tx(..)
                                   , TxIn(..)
                                   , TxOut(..)
                                   , CoinbaseTx(..)
                                   )
import Network.Haskoin.Util ( encode' )
import Network.Haskoin.Constants (maxSatoshi, maxBlockSize)

import qualified Data.ByteString as BS
import Control.Monad
import Data.List ( sort )

import Network.Haskoin.Validation.Error
import Network.Haskoin.Data.Blockchain (
    BlockChainDataRequest
  , pubScriptFromOutPoint )
import Network.Haskoin.Script.Evaluator ( Flag, verifySpend )

checkNoDuplicates :: ( Ord a ) => [a] -> Bool
checkNoDuplicates as = let sortedAs = sort as in
  sortedCheckNoDuplicates sortedAs

sortedCheckNoDuplicates :: ( Eq a ) => [ a ] -> Bool
sortedCheckNoDuplicates [] = True
sortedCheckNoDuplicates [_] = True
sortedCheckNoDuplicates (a:b:bs) = a /= b && sortedCheckNoDuplicates (b:bs)

checkTxOutput :: TxOut -> VError Bool
checkTxOutput out = do
  if outValue out > maxSatoshi
    then throwError $ TxInvalid "Txn output too large"
    else return True

-- | Transaction checks that don't depend on context ( no-mempool,
-- block chain queries )
checkTransaction :: Tx -> VError Bool
checkTransaction tx = do
  if null . txIn $ tx
    then throwError $ TxInvalid "txIn empty"
    else return ()

  if null. txOut $ tx
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

checkCBTransaction :: CoinbaseTx -> VError Bool
checkCBTransaction tx = do
  -- Some code duplication here
  if BS.null . cbData $ tx
    then throwError $ TxInvalid "cbData empty"
    else return ()

  if null. cbOut $ tx
    then throwError $ TxInvalid "cbOut empty"
    else return ()
  
  if ( BS.length . encode' $ tx ) > maxBlockSize
     then throwError $ TxInvalid "size limits failed"
     else return ()

  forM_ ( cbOut tx ) checkTxOutput

  let totOutput = sum . ( map outValue ) $ cbOut tx
  if totOutput > maxSatoshi
    then throwError $ TxInvalid "Total output sum too large"
    else return ()

  let l = BS.length . cbData $ tx 
  if l < 2 || l > 100
    then throwError $ TxInvalid "Coinbase script size."
    else return ()

  return True

-- | Validates all inputs for a given trasaction by testing their
-- scripts against the reference output scripts.
validateAllInputs :: [ Flag ]
                  -> Tx 
                  -> BlockChainDataRequest ( Bool )
validateAllInputs flgs tx = do
    let inputs = ( txIn tx )
    scripts <- mapM ( pubScriptFromOutPoint . prevOutput ) inputs
    let enumScripts = zip [0..] scripts
        results = map (\(i,s) -> verifySpend tx i s flgs ) enumScripts
    return $ all id results
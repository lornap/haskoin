{-# LANGUAGE DeriveFunctor #-}
{-|

A monad for making Blockchain Data Requests.

BlockChainDataRequest is a free monad created to separate functions
requiring block chain data from any implementation of block chain data
fetching.

Any back end providing block chain data fetching services should
provide an interpreter for this free monad.  A simple interpreter is
provided by runBlockChainRequestFromMap where in-memory Data.Maps
supply the blockchain data.

-}

module Network.Haskoin.Data.Blockchain
(
  BlockChainDataRequestF(..)
, BlockChainDataRequest
, txFromHash
, pubScriptFromOutPoint
, runBlockChainRequestFromMap
) where

import Control.Monad.Free

import qualified Data.Map as Map

import Network.Haskoin.Transaction.Types ( OutPoint, Tx )
import Network.Haskoin.Script.Types ( Script )
import Network.Haskoin.Crypto ( TxHash )

data BlockChainDataRequestF x
    = TxFromHash TxHash ( Tx -> x )
    | PubScriptFromOutPoint OutPoint ( Script -> x )
    deriving ( Functor )

type BlockChainDataRequest = Free BlockChainDataRequestF

-- | Request a transaction given it's hash
txFromHash :: TxHash -> BlockChainDataRequest Tx
txFromHash h = liftF $ TxFromHash h id

-- | Request a pubKey script given it's OutPoint
pubScriptFromOutPoint :: OutPoint -> BlockChainDataRequest ( Script )
pubScriptFromOutPoint op = liftF $ PubScriptFromOutPoint op id

type OutPointMap = Map.Map OutPoint Script
type HashMap = Map.Map TxHash Tx

-- | A basic, pure, in-memory interpreter of BlockChainDataRequests.
-- Useful for tests.
runBlockChainRequestFromMap :: OutPointMap              -- ^  Map of outpoint to Script
                            -> HashMap                  -- ^  Map of Hash to Tx
                            -> BlockChainDataRequest a  -- ^  Request to run
                            -> Either String a
runBlockChainRequestFromMap _ _ ( Pure a ) = Right a
runBlockChainRequestFromMap omap hmap ( Free ( TxFromHash h k ) ) =
    case Map.lookup h hmap of
        Nothing -> Left "TxHash not found."
        Just tx -> runBlockChainRequestFromMap omap hmap $ k tx

runBlockChainRequestFromMap omap hmap ( Free ( PubScriptFromOutPoint o k ) ) =
    case Map.lookup o omap of
        Nothing -> Left "OutPoint not found."
        Just sc -> runBlockChainRequestFromMap omap hmap $ k sc

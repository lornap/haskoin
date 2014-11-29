module Network.Haskoin.Validation.Error
(
  ValidationError (..)
, VError
, throwError
)
where

import Control.Monad.Except
import Control.Monad.Identity

data ValidationError = BadResponse String
                       | TxInvalid String
                       | OtherError String -- FIXME
                       deriving (Show)

type VError = ExceptT ValidationError Identity


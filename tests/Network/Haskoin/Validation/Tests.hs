module Network.Haskoin.Validation.Tests 
(
 tests
) where

import Network.Haskoin.Script.Evaluator ( Flag )
import Network.Haskoin.Script ( Script(..) )
import Network.Haskoin.Transaction ( Tx (..), OutPoint(..) )
import Network.Haskoin.Crypto ( TxHash )

import Network.Haskoin.Data.Blockchain (
  runBlockChainRequestFromMap )

import Network.Haskoin.Test.Util.JsonTestParser

import Network.Haskoin.Validation.Transaction (
   checkTransaction
 , validateAllInputs )

import Test.Framework ( Test, buildTest, testGroup )
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit.Base ( assertEqual, assertFailure )

import Control.Applicative ( (<$>), (<*>) )
import Control.Monad ( mzero )
import qualified Data.ByteString.Lazy as BSL (readFile, ByteString)
import Data.Aeson

import Control.Monad.Except ( runExcept )

import qualified Data.Vector as V
import Data.Vector ( (!) )
import qualified Data.Text as T
import Data.Word (Word32)
import Data.Maybe (mapMaybe)
import qualified Data.Map as M

validFname :: String
validFname = "tests/data/tx_valid.json"
invalidFname :: String
invalidFname = "tests/data/tx_invalid.json"

data FileItem = Comment String | Test FileTestCase
    deriving ( Show )

instance FromJSON FileItem where
   parseJSON x@( Array v ) =
     if V.length v > 1
       then Test <$> parseJSON x
       else Comment <$> parseJSON ( V.head v )

   parseJSON _ = mzero

data FileTestCase = FileTestCase { inputs :: [ TCInput ]
                                 , testTx :: Tx
                                 , flags  :: [ Flag ]
                                 } deriving ( Show )

instance FromJSON FileTestCase where
    parseJSON ( Array v ) = FileTestCase
                        <$> parseJSON ( v ! 0 )
                        <*> parseJSON ( v ! 1 )
                        <*> ( parseFlags <$> parseJSON ( v ! 2 ) )
    parseJSON _ = mzero

data TCInput = TCInput { prevOutHash :: TxHash
                       , prevOutIndx :: Word32
                       , prevOutScriptPubKey :: Script
                       } deriving ( Show )

instance FromJSON TCInput where
    parseJSON ( Array v ) = TCInput
                        <$> parseJSON ( v ! 0 )
                        <*> parseJSON ( v ! 1 )
                        <*> ( parseJSON ( v ! 2 ) >>= parseScript' )
    parseJSON _ = mzero

-- | A wrapper around parseScript to handle the potential error output
-- and slight format change from script to tx json files.
parseScript' :: (Monad m ) => T.Text -> m Script
parseScript' str = 
   -- Need this hack in the let because of the slight format change
   -- between script_*valid.json and tx_*valid.json. The tx version
   -- has OP_ before ops.
   let str' = T.unpack $ T.replace ( T.pack "OP_" ) T.empty str in
   case parseScript str' of
     Left e -> fail e
     Right s -> return s

parseFile :: BSL.ByteString -> Maybe [ FileItem ]
parseFile dat = decode dat

-- | For each test case in the file, add a label and an index number
-- to print out for the test.  May not label perfectly but it should be
-- enough to track down the failing test.  The label will be the
-- preceeding comment, if it exists.
labelItems :: [ FileItem ] -> [ ( Int, Maybe String, FileTestCase ) ]
labelItems [] = []
labelItems x@( _:ls ) =
    let pairs = zip x ls -- zip with shift by one
        checkType ( Comment s , Test t ) = Just ( Just s, t )
        checkType ( Test _ , Test t )    = Just ( Nothing, t )
        checkType _                      = Nothing
        labeledPairs = mapMaybe checkType pairs
    in zipWith ( \a (b,c) -> (a,b,c) ) [0..] labeledPairs

prepTestData :: FileTestCase -> ( M.Map OutPoint Script, Tx )
prepTestData ftc = let m = buildMap ( inputs ftc ) in ( m, testTx ftc )
    where buildMap = M.fromList . ( map toPairs )
          toPairs tci = ( OutPoint ( prevOutHash tci ) ( prevOutIndx tci )
                        , prevOutScriptPubKey tci )

twoPartTxCheck :: M.Map OutPoint Script -- ^ Prev input data
               -> Tx                    -- ^ Transaction under test
               -> [ Flag ]              -- ^ Script Flags
               -> Bool                  -- ^ Result
twoPartTxCheck d tx flgs = structureCheck && spendCheck
    where structureCheck = case runExcept $ checkTransaction tx of
              Right True -> True
              _ -> False
          spendCheck = case runBlockChainRequestFromMap d M.empty ( validateAllInputs flgs tx ) of
              Right True -> True
              _ -> False

toTest :: Bool                                 -- ^ Expected results
       -> ( Int, Maybe String, FileTestCase )  -- ^ Test item from file
       -> Test
toTest exptd (idx, mlabl, ftc) = 
    let label = case mlabl of
                    Just com -> (show idx) ++ ": " ++ com
                    Nothing  -> (show idx) ++ ": Unlabeled."
        ( d, tx ) = prepTestData ftc
        r = twoPartTxCheck d tx ( flags ftc )
     in
     testCase label $ assertEqual ( "Did not match expected result." ) exptd r

fileTests :: BSL.ByteString  -- ^ File contents
          -> Bool            -- ^ Expected Result
          -> [ Test ]
fileTests dat exptd =
   let mItems = parseFile dat
       testCases = labelItems <$> mItems
       mTests = map (toTest exptd) <$> testCases
   in
       case mTests of
           Nothing -> [ testCase "Failed to parse." $ assertFailure "Failed to parse the file." ]
           Just ts -> ts

groupTest :: Test
groupTest = buildTest $ do
    validContents <- BSL.readFile validFname
    invalidContents <- BSL.readFile invalidFname
    let validTests = fileTests validContents True
        invalTests = fileTests invalidContents False
        validGroup = testGroup "Valid Transactions" validTests
        invalGroup = testGroup "Invalid Transactions" invalTests
    return $ testGroup "Transaction Validation" [ validGroup, invalGroup ]

tests :: [ Test ]
tests = [ groupTest ]

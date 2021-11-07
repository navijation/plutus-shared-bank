-- Contract to create a shared bank account with specified owners and allow withdrawal requests
-- to be settled upon acceptance by all owners. Anyone can initiate withdrawal requests and
-- deposits. Only owners can accept or deny withdrawal requests. All pending requests are stored
-- in the shared account state UTxO.
--
--   Datum: SharedBankAccountDatum
--   Redeemer: SharedBankingAction
-- 
--   Endpoints:
--      * open
--      * request
--      * respond
{-# LANGUAGE StandaloneDeriving #-}
import           Data.Text            
import           Text.Printf
import           Control.Monad        (void)
import           Ledger               (Address, ScriptContext, PubKeyHash, txId)
import qualified Ledger.Constraints   as Constraints
import qualified Ledger.Typed.Scripts as Scripts
import           Ledger.Value         (Value)
import           Playground.Contract
import           Plutus.Contract
import qualified PlutusTx
import qualified PlutusTx.AssocMap    as Map
import           PlutusTx.Prelude     hiding (Applicative (..))
import           qualified Prelude    as Haskell

data WithdrawalAnswer = Accept | Deny
    deriving stock (Haskell.Eq, Haskell.Show, Generic)
    deriving anyclass (FromJSON, ToJSON, ToSchema, ToArgument)

PlutusTx.unstableMakeIsData ''WithdrawalAnswer
PlutusTx.makeLift ''WithdrawalAnswer

data PendingWithdrawal = PendingWithdrawal {
    id :: Integer,
    signatures :: [(PubKeyHash, WithdrawalAnswer)],
    pendingPayouts :: !(Map.Map PubKeyHash Value)
}   deriving stock (Haskell.Eq, Haskell.Show, Generic)
    deriving anyclass (FromJSON, ToJSON, ToSchema)

PlutusTx.unstableMakeIsData ''PendingWithdrawal
PlutusTx.makeLift ''PendingWithdrawal

data SharedBankAccountDatum = SharedBankAccountDatum {
    datOwners :: ![PubKeyHash],
    datWithdrawalReqCounter :: Integer,
    datPendingWithdrawals:: (Map.Map Integer PendingWithdrawal)
}   deriving stock (Haskell.Eq, Haskell.Show, Generic)
    deriving anyclass (FromJSON, ToJSON, ToSchema)

PlutusTx.unstableMakeIsData ''SharedBankAccountDatum
PlutusTx.makeLift ''SharedBankAccountDatum

data WithdrawalRequest = WithdrawalRequest {
    payouts :: !(Map.Map PubKeyHash Value)
}   deriving stock (Haskell.Eq, Haskell.Show, Generic)
    deriving anyclass (FromJSON, ToJSON, ToSchema)

PlutusTx.unstableMakeIsData ''WithdrawalRequest
PlutusTx.makeLift ''WithdrawalRequest

data SharedBankingAction = RequestWithdrawal WithdrawalRequest | RespondWithdrawal Integer WithdrawalAnswer
	deriving Show

PlutusTx.unstableMakeIsData ''SharedBankingAction
PlutusTx.makeLift ''SharedBankingAction


validateSharedBankAccountAction :: SharedBankAccountDatum -> SharedBankingAction -> ScriptContext -> Bool
validateSharedBankAccountAction sharedAccount accountAction ctx =
    traceIfFalse "not implemented yet" False

data SharedBanking
instance Scripts.ValidatorTypes SharedBanking where
    type instance RedeemerType SharedBanking = SharedBankingAction
    type instance DatumType SharedBanking = SharedBankAccountDatum

sharedBankAccountTypedValidator :: Scripts.TypedValidator SharedBanking 
sharedBankAccountTypedValidator = Scripts.mkTypedValidator @SharedBanking
    $$(PlutusTx.compile [|| validateSharedBankAccountAction ||])
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @SharedBankAccountDatum @SharedBankingAction


sharedBankAccountInstance :: Scripts.TypedValidator SharedBanking
sharedBankAccountInstance = Scripts.mkTypedValidator @SharedBanking
    $$(PlutusTx.compile [|| validateSharedBankAccountAction ||])
    $$(PlutusTx.compile [|| wrap ||]) where
        wrap = Scripts.wrapValidator @SharedBankAccountDatum @SharedBankingAction

data OpenParams = OpenParams {
    opInitialFund :: Value,
    opOwners :: ![PubKeyHash]
} deriving (Generic, ToJSON, FromJSON, ToSchema)

data RequestParams = RequestParams {
    rpRequest:: WithdrawalRequest
} deriving (Generic, ToJSON, FromJSON, ToSchema)

data RespondParams = RespondParams {
    answer:: WithdrawalAnswer
} deriving (Generic, ToJSON, FromJSON, ToSchema)


-- | shared account endpoints

type SharedBankAccountSchema =
	Endpoint "open" OpenParams
    .\/ Endpoint "request" RequestParams
    .\/ Endpoint "respond" RespondParams

contract :: AsContractError e => Contract () SharedBankAccountSchema e ()
contract = selectList [open, request, respond]

open :: AsContractError e => Promise () SharedBankAccountSchema e ()
open = endpoint @"open" $ \openParams -> do
    let tx = Constraints.mustPayToTheScript (SharedBankAccountDatum (opOwners openParams) 0 Map.empty) (opInitialFund openParams) 
    ledgerTx <- submitTxConstraints sharedBankAccountTypedValidator tx
    void $ awaitTxConfirmed $ txId ledgerTx
    logInfo @Haskell.String $ printf "New bank account created"

request :: AsContractError e => Promise () SharedBankAccountSchema e ()
request = endpoint @"request" $ \requestParams -> do
    logInfo @Haskell.String $ printf "Called request endpoint"

respond :: AsContractError e => Promise () SharedBankAccountSchema e ()
respond = endpoint @"respond" $ \respondParams -> do 
    logInfo @Haskell.String $ printf "Called respond endpoint"

endpoints :: AsContractError e => Contract () SharedBankAccountSchema e ()
endpoints = contract

mkSchemaDefinitions ''SharedBankAccountSchema

$(mkKnownCurrencies [])

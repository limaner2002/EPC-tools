{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

import ClassyPrelude
import Appian
import Appian.Client hiding (login)
import Appian.Instances
import Appian.Types
import Appian.Internal.Updates
import Control.Lens
import Appian.Lens
import Scripts.Execute
import Scripts.ReviewCommon
import Scripts.Common
import Scripts.Test
import Stats.CsvStream
import qualified Data.Csv as Csv
import Data.Aeson
import Data.Aeson.Lens
import NewFunctions

data InvoiceDetailsConf = InvoiceDetailsConf
    { _invoiceCaseNum :: CaseNumber
    , _invLogin :: Login
    } deriving Show

makeLenses ''InvoiceDetailsConf

instance HasLogin InvoiceDetailsConf where
    getLogin details = details ^. invLogin
    
instance Csv.FromNamedRecord InvoiceDetailsConf where
    parseNamedRecord r = InvoiceDetailsConf
        <$> r Csv..: "caseNum"
        <*> Csv.parseNamedRecord r

viewInvoiceDetails :: (RapidFire m, MonadGen m, MonadIO m) => InvoiceDetailsConf -> AppianT m Value
viewInvoiceDetails details = do
    (rid, v) <- myAssignedReport (details ^? invoiceCaseNum) comadInitial2017
    return v
    rref <- getRecordRefFromGrid v
    dashState <- runDashboardByName rref "COMAD Reviewers Summary"

    execDashboardFormT invoiceDashboard dashState
    use appianValue

invoiceDashboard :: DashboardForm ()
invoiceDashboard = do
  update "Select FRN Checkbox" (gridFieldUpdateWithF getGridFieldCell 0)
  update "Click 'Invoice Information'" (componentUpdateWithF "Could not find '+ Invoice Information' dynamic link" invoiceDynamicLink)

invoiceDynamicLink :: Fold Value DynamicLink
invoiceDynamicLink = deep (filtered $ has $ key "values" . key "#v" . _String . only "+ Invoice Information") . key "link" . _JSON

getRecordRefFromGrid :: RapidFire m => Value -> AppianT m RecordRef
getRecordRefFromGrid v = handleMissing "Could not find recordRef" v $ v ^? getGridFieldCell . traverse . gfColumns . at "Application/Request Number" . traverse . _TextCellLink . _2 . traverse

runViewInvoiceDetails :: Bounds -> HostUrl -> LogMode -> CsvPath -> RampupTime -> NThreads -> IO [Maybe (Either ServantError (Either ScriptError Value))]
runViewInvoiceDetails = runIt viewInvoiceDetails

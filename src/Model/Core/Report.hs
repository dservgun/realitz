{-|
Module      : Model.Core.Report
Description : This module defines the reports, associated contamination limits.
Copyright   : (c) 
License     : GPL-3
Maintainer  : dinkar.ganti@gmail.com
Stability   : 
Portability : 
-}

module Model.Core.Report where 


data Detail = Detail {
  chemicalName :: Text
  , casNumber :: CasNumber
  , contamination :: Contamination
  , _measuredOn :: UTCTiime
  -- | The laboratory performing the tests.
  , _lab :: Lab
  -- | The specific protocol the laboratory is tasked to follow to 
  -- | measure contamination. Typically, these are in the forma
  -- | of a link to a pdf document published by either the state or 
  -- | a federal regulatory body. Smoe protocols could be sufficiently 
  -- | standard that an industry association could be responsible for 
  -- | pubishing the protocol.
  , _protocol :: CotaminantTestProtocol
} deriving (Show)

{-| 
  A summary report of contamination captures state of the contamination.
-}
data Summary = Summary {
  _summary :: Text
  , _details :: Text
  , _preparedBy :: Technician
} deriving (Show)

-- | Given a set of details, is there a trendline demonstrating a decay in the concentrations of the 
-- | contamination. 'trendLineEstablished' should return 'True' for a project to return an NFA from the 
-- | DEP. Fine Print : This method returning 'True' is an internal indicator. THe DEP will have its own
-- | set of rules and conventions before an NFA can be issued for a property.
trendlineEstablished :: Functor f => f Detail -> Boolean 
trendlineEstablished = undefined



data ReportIdentifier = ReportIdentifier {_id :: Text} deriving (Show)
data ReportType = 
  RemedialInvestigation | RiskAssessment | Cleanup deriving (Show)

data Report = Report {
  -- | Date on which the report was prepared.
  preparedDate :: UTCTime 
  -- | The type of report. 
  , reportType :: ReportType
  -- | 'Detail' outlines additional detail based on the phase of the 
  -- | contaminated property. 
  , reportDetails :: Set Detail
  -- | A high level summary of the report. 
  , reportSummary :: Set Summary
  , caseNumber :: ReportIdentifier
  , preparer :: CaseManager
} deriving (Show)




response <- httr::GET("https://api.epa.gov/expertquery/api/attains/assessments?")
data <- httr::content(response, "parsed")



{
  "options": {
    "pageNumber": 0,
    "pageSize": 0,
    "format": "json"
  },
  "columns": [
    "string"
  ],
  "filters": {
    "alternateListingIdentifier": [
      "string"
    ],
    "assessmentBasis": [
      "string"
    ],
    "assessmentDateLo": "2025-01-28",
    "assessmentDateHi": "2025-01-28",
    "assessmentMethods": [
      "string"
    ],
    "assessmentTypes": [
      "string"
    ],
    "assessmentUnitId": [
      "string"
    ],
    "assessmentUnitName": [
      "string"
    ],
    "assessmentUnitStatus": [
      "string"
    ],
    "associatedActionAgency": [
      "string"
    ],
    "associatedActionId": [
      "string"
    ],
    "associatedActionName": [
      "string"
    ],
    "associatedActionStatus": [
      "string"
    ],
    "associatedActionType": [
      "string"
    ],
    "consentDecreeCycleLo": 0,
    "consentDecreeCycleHi": 0,
    "cwa303dPriorityRanking": [
      "string"
    ],
    "cycleExpectedToAttainLo": 0,
    "cycleExpectedToAttainHi": 0,
    "cycleFirstListedLo": 0,
    "cycleFirstListedHi": 0,
    "cycleId": [
      0
    ],
    "cycleLastAssessedLo": 0,
    "cycleLastAssessedHi": 0,
    "cycleScheduledForTmdlLo": 0,
    "cycleScheduledForTmdlHi": 0,
    "delisted": [
      "string"
    ],
    "delistedReason": [
      "string"
    ],
    "epaIrCategory": [
      "string"
    ],
    "monitoringEndDateLo": "2025-01-28",
    "monitoringEndDateHi": "2025-01-28",
    "monitoringStartDateLo": "2025-01-28",
    "monitoringStartDateHi": "2025-01-28",
    "objectId": [
      0
    ],
    "organizationId": [
      "string"
    ],
    "organizationName": [
      "string"
    ],
    "organizationType": [
      "string"
    ],
    "overallStatus": [
      "string"
    ],
    "parameterAttainment": [
      "string"
    ],
    "parameterGroup": [
      "string"
    ],
    "parameterIrCategory": [
      "string"
    ],
    "parameterName": [
      "string"
    ],
    "parameterStateIrCategory": [
      "string"
    ],
    "parameterStatus": [
      "string"
    ],
    "pollutantIndicator": [
      "string"
    ],
    "region": [
      "string"
    ],
    "reportingCycle": 0,
    "seasonEndDateLo": "2025-01-28",
    "seasonEndDateHi": "2025-01-28",
    "seasonStartDateLo": "2025-01-28",
    "seasonStartDateHi": "2025-01-28",
    "state": [
      "string"
    ],
    "stateIrCategory": [
      "string"
    ],
    "useClassName": [
      "string"
    ],
    "useGroup": [
      "string"
    ],
    "useIrCategory": [
      "string"
    ],
    "useName": [
      "string"
    ],
    "useStateIrCategory": [
      "string"
    ],
    "useSupport": [
      "string"
    ],
    "vision303dPriority": [
      "string"
    ],
    "waterType": [
      "string"
    ]
  }
}


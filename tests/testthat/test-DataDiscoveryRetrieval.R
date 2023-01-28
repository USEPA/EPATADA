test_that("TADAdataRetrieval", {
  tada1 <- TADAdataRetrieval(siteid = c("USGS-054064785",
                                        "USGS-430305089260600"),
                             characteristicName = "Phosphorus")
  # you could just pick the important columns:
  expect_true(all(c("OrganizationIdentifier",                           
                    "OrganizationFormalName",                           
                    "ActivityIdentifier",                               
                    "ActivityTypeCode",                                 
                    "ActivityMediaName",                                
                    "ActivityMediaSubdivisionName",                     
                    "ActivityStartDate",                                
                    "ActivityStartTime.Time",                           
                    "ActivityStartTime.TimeZoneCode",                   
                    "ActivityEndDate",                                  
                    "ActivityEndTime.Time",                             
                    "ActivityEndTime.TimeZoneCode",  
                    "ActivityRelativeDepthName",
                    "ActivityDepthHeightMeasure.MeasureValue",          
                    "ActivityDepthHeightMeasure.MeasureUnitCode",       
                    "ActivityDepthAltitudeReferencePointText",          
                    "ActivityTopDepthHeightMeasure.MeasureValue",       
                    "ActivityTopDepthHeightMeasure.MeasureUnitCode",    
                    "ActivityBottomDepthHeightMeasure.MeasureValue",    
                    "ActivityBottomDepthHeightMeasure.MeasureUnitCode", 
                    "ProjectIdentifier",                                
                    "ProjectName", 
                    "ActivityConductingOrganizationText",               
                    "MonitoringLocationIdentifier",                     
                    "MonitoringLocationName", 
                    "ActivityCommentText",                              
                    "SampleAquifer",                                    
                    "HydrologicCondition",                              
                    "HydrologicEvent",                                  
                    "SampleCollectionMethod.MethodIdentifier",          
                    "SampleCollectionMethod.MethodIdentifierContext",   
                    "SampleCollectionMethod.MethodName",                
                    "SampleCollectionEquipmentName",                    
                    "ActivityLocation.LatitudeMeasure",                
                    "ActivityLocation.LongitudeMeasure",
                    "ResultDetectionConditionText",                     
                    "MethodSpeciationName", 
                    "CharacteristicName",                              
                    "ResultSampleFractionText",                         
                    "ResultMeasureValue",                               
                    "ResultMeasureValue.Original",                      
                    "TADA.ResultMeasureValue.Flag",                     
                    "ResultMeasure.MeasureUnitCode",                    
                    "MeasureQualifierCode",                             
                    "ResultStatusIdentifier",                           
                    "StatisticalBaseCode",                              
                    "ResultValueTypeName",                              
                    "ResultWeightBasisText",                            
                    "ResultTimeBasisText",                              
                    "ResultTemperatureBasisText",                       
                    "ResultParticleSizeBasisText",                     
                    "DataQuality.PrecisionValue",                     
                    "DataQuality.BiasValue",                            
                    "DataQuality.ConfidenceIntervalValue",              
                    "DataQuality.UpperConfidenceLimitValue",            
                    "DataQuality.LowerConfidenceLimitValue",
                    "ResultCommentText",                                
                    "USGSPCode",                                        
                    "ResultDepthHeightMeasure.MeasureValue",           
                    "ResultDepthHeightMeasure.MeasureUnitCode",         
                    "ResultDepthAltitudeReferencePointText",            
                    "SubjectTaxonomicName",                             
                    "SampleTissueAnatomyName",                          
                    "BinaryObjectFileName", 
                    "BinaryObjectFileTypeCode",                         
                    "ResultFileUrl", 
                    "ResultAnalyticalMethod.MethodIdentifier",          
                    "ResultAnalyticalMethod.MethodIdentifierContext",   
                    "ResultAnalyticalMethod.MethodName",                
                    "ResultAnalyticalMethod.MethodUrl",                 
                    "ResultAnalyticalMethod.MethodDescriptionText",  
                    "LaboratoryName",                                   
                    "AnalysisStartDate" ,                               
                    "ResultLaboratoryCommentText",                      
                    "ResultDetectionQuantitationLimitUrl", 
                    "DetectionQuantitationLimitTypeName",               
                    "DetectionQuantitationLimitMeasure.MeasureValue",   
                    "DetectionLimitMeasureValue.Original",              
                    "TADA.DetectionLimitMeasureValue.Flag",             
                    "DetectionQuantitationLimitMeasure.MeasureUnitCode",
                    "LabSamplePreparationUrl", 
                    "LastUpdated", 
                    "ProviderName",                                     
                    "timeZoneStart", 
                    "timeZoneEnd", 
                    "ActivityStartDateTime",                            
                    "ActivityEndDateTime" ,                             
                    "MonitoringLocationTypeName",                           
                    "MonitoringLocationDescriptionText",                
                    "HUCEightDigitCode",                                
                    "DrainageAreaMeasure.MeasureValue",                 
                    "DrainageAreaMeasure.MeasureUnitCode",              
                    "ContributingDrainageAreaMeasure.MeasureValue",     
                    "ContributingDrainageAreaMeasure.MeasureUnitCode",  
                    "LatitudeMeasure",                                  
                    "LongitudeMeasure",                                 
                    "SourceMapScaleNumeric",                            
                    "HorizontalAccuracyMeasure.MeasureValue",           
                    "HorizontalAccuracyMeasure.MeasureUnitCode",        
                    "HorizontalCollectionMethodName",                   
                    "HorizontalCoordinateReferenceSystemDatumName",     
                    "VerticalMeasure.MeasureValue",                 
                    "VerticalMeasure.MeasureUnitCode",                 
                    "VerticalAccuracyMeasure.MeasureValue",             
                    "VerticalAccuracyMeasure.MeasureUnitCode",          
                    "VerticalCollectionMethodName",                     
                    "VerticalCoordinateReferenceSystemDatumName",      
                    "CountryCode",                                      
                    "StateCode",                                      
                    "CountyCode",                                     
                    "AquiferName",                                   
                    "LocalAqfrName",                               
                    "FormationTypeText",                                
                    "AquiferTypeName",                            
                    "ConstructionDateText",                   
                    "WellDepthMeasure.MeasureValue",                
                    "WellDepthMeasure.MeasureUnitCode",               
                    "WellHoleDepthMeasure.MeasureValue",           
                    "WellHoleDepthMeasure.MeasureUnitCode" ,            
                    "MethodSpecificationName") %in% names(tada1)))
  
})

#testing that "meters" is successfully replaced with "m". This feature is part of the autoclean function
#which runs automatically when TADAdataRetrieval runs
test_that("TADAdataRetrieval", {
  check_autoclean_meters_works <- TADAdataRetrieval(statecode = "UT",
                                    characteristicName = c("Ammonia", "Nitrate", "Nitrogen"),
                                    startDate = "01-01-2021")
  expect_true(any(check_autoclean_meters_works$ActivityDepthHeightMeasure.MeasureUnitCode!="meters"))
  })


# Testing that the JoinWQPProfiles() function in DataDiscoveryRetrieval.R 
# has the expected number of columns after joining the full physical chemical 
# profile and the sites profiles together. This function uses cyanobacteria 
# full phys chem results and station metadata. 


test_that("JoinWQPProfiles", {

  # testthat::test_path() is automatically set to "tests/testthat". To get to the data files, you 
  #only need to add the additional pathway e.g., not the full path i.e., 
  #"tests/testthat/testdata/Cyan_Stations.rds" but "testdata/Cyan_Results.rds"
  physchemresults = readRDS(testthat::test_path("testdata/Cyan_Results.rds"))
  stations = readRDS(testthat::test_path("testdata/Cyan_Stations.rds"))
  
  add_sites_metadata <- JoinWQPProfiles(Sites = stations, 
                                        FullPhysChem = physchemresults)
  expect_true(ncol(add_sites_metadata) == 113)
})


test_that("JoinWQPProfile", {
  data(station)
  data(narrow)
  data(resultphyschem)
  
  #narrow = readRDS(testthat::test_path("testdata/narrow_raw.rds"))
  #resultphyschem = readRDS(testthat::test_path("testdata/resultphyschem_raw.rds"))
  #station = readRDS(testthat::test_path("testdata/station_raw.rds"))
  
  join = TADA::JoinWQPProfiles(FullPhysChem = resultphyschem, 
                               Sites = station, 
                               Narrow = narrow)
  # update in future to pick the important columns:
  expect_true(all(c("OrganizationIdentifier",                           
                    "OrganizationFormalName",                           
                    "ActivityIdentifier",                               
                    "ActivityTypeCode",                                 
                    "ActivityMediaName",                                
                    "ActivityMediaSubdivisionName",                     
                    "ActivityStartDate",                                
                   "ActivityStartTime.Time",                           
                    "VerticalAccuracyMeasure.MeasureValue"           ,  
                    "VerticalAccuracyMeasure.MeasureUnitCode"        ,  
                    "VerticalCollectionMethodName"                  ,   
                    "VerticalCoordinateReferenceSystemDatumName"   ,    
                    "CountryCode"                                   ,   
                    "StateCode"                                     ,   
                    "CountyCode"                                     ,  
                    "AquiferName"                                     , 
                    "LocalAqfrName"           ,                         
                    "FormationTypeText"        ,                        
                    "AquiferTypeName"           ,                       
                    "ConstructionDateText"       ,                      
                    "WellDepthMeasure.MeasureValue"   ,                 
                    "WellDepthMeasure.MeasureUnitCode" ,                
                    "WellHoleDepthMeasure.MeasureValue"   ,             
                    "WellHoleDepthMeasure.MeasureUnitCode",
                    "MethodSpecificationName") %in% names(join)))
  
})
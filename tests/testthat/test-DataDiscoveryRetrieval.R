test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

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
                    "ActivityDepthHeightMeasure.MeasureValue",          
                    "ActivityDepthHeightMeasure.MeasureUnitCode",       
                    "ActivityDepthAltitudeReferencePointText",          
                    "ActivityTopDepthHeightMeasure.MeasureValue",       
                    "ActivityTopDepthHeightMeasure.MeasureUnitCode",    
                    "ActivityBottomDepthHeightMeasure.MeasureValue",    
                    "ActivityBottomDepthHeightMeasure.MeasureUnitCode", 
                    "ProjectIdentifier",                                
                    "ActivityConductingOrganizationText",               
                    "MonitoringLocationIdentifier",                     
                    "ActivityCommentText",                              
                    "SampleAquifer",                                    
                    "HydrologicCondition",                              
                    "HydrologicEvent",                                  
                    "SampleCollectionMethod.MethodIdentifier",          
                    "SampleCollectionMethod.MethodIdentifierContext",   
                    "SampleCollectionMethod.MethodName",                
                    "SampleCollectionEquipmentName",                    
                    "ResultDetectionConditionText",                     
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
                    "ResultParticleSizeBasisText" ,                     
                    "PrecisionValue",                                   
                    "ResultCommentText",                                
                    "USGSPCode",                                        
                    "ResultDepthHeightMeasure.MeasureValue",           
                    "ResultDepthHeightMeasure.MeasureUnitCode",         
                    "ResultDepthAltitudeReferencePointText",            
                    "SubjectTaxonomicName",                             
                    "SampleTissueAnatomyName",                          
                    "ResultAnalyticalMethod.MethodIdentifier",          
                    "ResultAnalyticalMethod.MethodIdentifierContext",   
                    "ResultAnalyticalMethod.MethodName",                
                    "MethodDescriptionText",                            
                    "LaboratoryName",                                   
                    "AnalysisStartDate" ,                               
                    "ResultLaboratoryCommentText",                      
                    "DetectionQuantitationLimitTypeName",               
                    "DetectionQuantitationLimitMeasure.MeasureValue",   
                    "DetectionLimitMeasureValue.Original",              
                    "TADA.DetectionLimitMeasureValue.Flag",             
                    "DetectionQuantitationLimitMeasure.MeasureUnitCode",
                    "PreparationStartDate",                             
                    "ProviderName",                                     
                    "ActivityStartDateTime",                            
                    "ActivityEndDateTime" ,                             
                    "MonitoringLocationName",                           
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


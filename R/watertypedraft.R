org.watertype.cw <- rExpertQuery::EQ_NationalExtract("aus")

org.watertype.cw2 <- org.watertype.cw |>
  dplyr::select(organizationId,
                waterType) |>
  dplyr::distinct()

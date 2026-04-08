vecProvenances <- unique(dtTrajectoires$provenance)
vecDestinations <- unique(dtTrajectoires$destination)
vecProvenancesDestinations <- unique(c(vecProvenances, vecDestinations))

dtProvenanceDestination <- data.table(
  provenancesEtDestinations = c(vecProvenancesDestinations),
  definition = "",
  groupe = "")

openxlsx2::write_xlsx(dtProvenanceDestination,
                      "03_outputFiles/20260401_tableProvenancesDestinations.xlsx")

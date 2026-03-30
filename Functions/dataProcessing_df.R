# Loading libraries
library(dplyr)
library(tidyr)

dataProcessing_df<- function(type, plddt_thresh){
  # load corresponding Data according to type
  # merge with plddt and keep observations with plddt >= 70
  type = match.arg(type, c('af', 'crystal', 'crystal_yes_af_2nd', 'af_not_crystal', 'af_crystal'))

  # 1. All AlphaFold
  af_A <- read.csv("Data/final_dfA02_22_2026.csv", check.names = FALSE)
  af_B <- read.csv("Data/final_dfB02_22_2026.csv")

  # 2. All Crystal
  crystal_A <- read.csv("Data/final_dfA_Crystal02_22_2026.csv", check.names = FALSE)
  crystal_B <- read.csv("Data/final_dfB_Crystal02_22_2026.csv")


  if (type == "af") {
    final_dfA <- af_A
    final_dfB <- af_B
  } else if (type == "crystal") {
    final_dfA <- crystal_A
    final_dfB <- crystal_B
  } else {
    stop("Invalid input_type. Choose from 'af', 'crystal', 'crystal_yes_af_2nd', or 'af_crystal'.")
  }

  # Remove extra space
  final_dfA$uniprotids <- trimws(final_dfA$uniprotids)
  final_dfB$gene <- trimws(final_dfB$gene)


  # Load plddt Data
  plddt <- read.csv("Data/Avg_pLDDTs.csv", header = TRUE, sep = ",")
  plddt <- plddt %>% rename(uniprotids = gene)
  plddt <- plddt %>% rename(plddt = X.pLDDT.)

  # Join plddt Data
  final_dfA <- final_dfA %>%
    left_join(plddt, by = "uniprotids")
  final_dfB <- final_dfB %>%
    left_join(plddt, by = c("gene" = "uniprotids"))

  # Select Thresholds
  if (type %in% c("af", "af_crystal")) {
    final_dfA <- final_dfA %>%
      filter(plddt >= 70)

    final_dfB <- final_dfB %>%
      filter(plddt >= 70)
  }

  return(list(final_dfA = final_dfA, final_dfB= final_dfB))

}




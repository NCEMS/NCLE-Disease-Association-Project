# Loading libraries
library(dplyr)
library(stringr)

#### Create a data frame final_df_updated ####

# Absolute value of Gn and Gc
final_dfB$Gn <- abs(final_dfB$Gn)
final_dfB$Gc <- abs(final_dfB$Gc)

# Subset necessary columns
UniScoDis <- final_dfA[, c("uniprotids", "50th_percentile", "75th_percentile", "95th_percentile")]

# Remove duplicates
UniScoDis <- distinct(UniScoDis)

# Keep Uniprots with the highest score
max_df <- UniScoDis %>%
  group_by(uniprotids) %>%
  summarise(
    `50th_percentile` = ifelse(any(`50th_percentile` == "Yes"), "Yes", "No"),
    `75th_percentile` = ifelse(any(`75th_percentile` == "Yes"), "Yes", "No"),
    `95th_percentile` = ifelse(any(`95th_percentile` == "Yes"), "Yes", "No"),
    .groups = "drop"
  )

# Perform a left join to include the entanglement data
colnames(max_df)[1] <- "gene"
final_df_updated <- left_join(max_df, final_dfB, by = c("gene"))
rm(UniScoDis,max_df)

# keep only entangled proteins
final_df_updated <- final_df_updated %>%
  filter(Entanglement == "Yes")

# check whether any of Gn or Gc values are missing
stopifnot(sum(is.na(final_df_updated$Gn))==0)
stopifnot(sum(is.na(final_df_updated$Gc))==0)

# Create columns for Gsum: |Gn|+|Gc| and Gmax: Max(|Gn|, |Gc|)
final_df_updated$Gsum <- final_df_updated$Gc + final_df_updated$Gn
final_df_updated$Gmax <- pmax(final_df_updated$Gc, final_df_updated$Gn)

# Data preprocessing

cat('date:',format(Sys.time(), "%Y-%m-%d_%H-%M-%S"))
cat("control file input: Controlm1_3\n")
cat("types: af\n")
cat("plddt_thresh: 70\n")

# Data Checks
# Libraries
library(dplyr)
library(stringr)
library(readr)

# Mutation data (pre-cleaned, missense only)
combined_mapped_clean <- read_csv("Data/combined_mapped_clean_missense.csv")

############################################################
# Define mutation identity by gene, position, codon, and amino acid change
# to check for duplicates (same mutation recorded more than once)
################################################################################
mutation_counts <- combined_mapped_clean %>%
  group_by(uniprotids, User_input, Codon_change, Amino_acid_change, Amino_acid_position) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

# Keep only duplicated mutations (count > 1)
mutation_counts <- mutation_counts %>% filter(count > 1)

# Pull all rows in the main mutation table that correspond to those duplicates
# majority reported twice because they come from different sources but are the same
# removed in downstream analysis
repeated_mutations <- combined_mapped_clean %>%
  inner_join(mutation_counts, by = c("uniprotids", "User_input", "Codon_change", "Amino_acid_change", "Amino_acid_position"))

# Among duplicates, extract those from ClinVar pathogenic dataset
# (checking for contradictions where same User_input appears benign vs pathogenic)
repeated_clinvar_pathogenic <- unique(repeated_mutations %>%
                                        filter(source == "ClinVarPathogenic"))

# Save contradictory mutation identifiers (User_input) to remove later
user_inputs_to_remove <- unique(repeated_clinvar_pathogenic$User_input)

################################################################################
# analysis1_1.R -> final_dfB
################################################################################
final_dfB <- read.csv("Data/final_dfB_af_02_22_2026.csv") # Entanglement data

# Disease Class and Entanglement Data
source("Functions/dataProcessingClass.R")

# Apply the function to all columns except the first and second one (uniprotids)
out <- dataProcessing_UniScoDis(type = "af")

UniScoDis50 <- out$UniScoDis50
UniScoDis50  <- UniScoDis50  %>% as_tibble() %>% group_by(uniprotids)

# Summarized Disease data
final_df <- read_csv("Data/final_df_af_0_2026-2-6.csv")

# Keep only matching proteins in final_df
final_df <- final_df %>%
  filter(uniprotids %in% final_dfB$gene)

# Add Length and Essentiality
final_df <- final_df %>%
  left_join(
    final_dfB %>%
      select(gene, Length, Essential) %>%
      distinct(),
    by = c("uniprotids" = "gene")
  )

# Merge with final_df using uniprotids as the matching key
final_df <- merge(final_df,
                  combined_mapped_clean[, c("uniprotids", "User_input", "Consequences","Amino_acid_change", "Amino_acid_position", "Mutation", "Pathogenic")],
                  by = "uniprotids",
                  all.x = TRUE)

# Keep unique rows
final_df <- final_df %>% distinct()

# Remove mutations if they are located outside of the "length" of the protein
final_df <- final_df %>%
  filter(Amino_acid_position <= Length)

# Remove mutations flagged as contradictory (same User_input with pathogenic vs benign)
final_df <- final_df %>%
  filter(!User_input %in% user_inputs_to_remove)

# Keep rows with Mutations only
final_df <- final_df %>% filter(Mutation == "Yes" & Consequences == "missense")

# Count pathogenic and benign entries per uniprotid
path_counts <- final_df %>%
  group_by(uniprotids) %>%
  summarise(
    pathogenic_count = sum(Pathogenic == "Yes"),
    benign_count = sum(Pathogenic == "No")
  )

# Identify uniprotids that meet the criteria (at least 10 missense mutations)
valid_uniprotids <- path_counts %>%
  filter(pathogenic_count + benign_count >= 10) %>%
  pull(uniprotids)

# Filter the original dataset
final_df <- final_df %>%
  filter(uniprotids %in% valid_uniprotids)

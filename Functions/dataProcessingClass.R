library(dplyr)

dataProcessing_UniScoDis <- function(type, data_dir = "Data") {

  type <- match.arg(type, c("af", "crystal"))

  # Helper to find the most recent file matching a pattern
  get_latest_file <- function(prefix, type, data_dir) {
    pattern <- paste0("^", prefix, "_plddt_0_", type, "_\\d{4}-\\d{2}-\\d{2}_\\d{2}-\\d{2}-\\d{2}\\.csv$")
    files <- list.files(path = data_dir, pattern = pattern, full.names = TRUE)

    if (length(files) == 0) {
      stop(paste("No file found for", prefix, "with type =", type))
    }

    files[which.max(file.info(files)$mtime)]
  }

  # Load files
  file_95 <- get_latest_file("UniScoDis95", type, data_dir)
  file_75 <- get_latest_file("UniScoDis75", type, data_dir)
  file_50 <- get_latest_file("UniScoDis50", type, data_dir)

  UniScoDis95 <- read.csv(file_95, check.names = FALSE)
  UniScoDis75 <- read.csv(file_75, check.names = FALSE)
  UniScoDis50 <- read.csv(file_50, check.names = FALSE)

  # Subset by plddt threshold
  UniScoDis95 <- UniScoDis95 %>% filter(plddt >= 70)
  UniScoDis75 <- UniScoDis75 %>% filter(plddt >= 70)
  UniScoDis50 <- UniScoDis50 %>% filter(plddt >= 70)

  # Remove plddt column (last column OR by name)
  UniScoDis95 <- UniScoDis95 %>% select(-plddt)
  UniScoDis75 <- UniScoDis75 %>% select(-plddt)
  UniScoDis50 <- UniScoDis50 %>% select(-plddt)

  return(list(
    UniScoDis95 = UniScoDis95,
    UniScoDis75 = UniScoDis75,
    UniScoDis50 = UniScoDis50
  ))
}

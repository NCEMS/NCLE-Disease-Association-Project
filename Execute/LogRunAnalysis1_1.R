# Generate a log file name with the current date and time
log_file <- paste0("Log/ScriptOutput_", paste0(paste(types, collapse = "_"), "_"), plddt_thresh, "_a1_1", format(Sys.time(), "_%Y-%m-%d_%H-%M-%S"), ".log")

# Redirect console output and messages to the log file
sink(log_file)

# Source the R script, logging all output
source("AnalysisCodes/analysis1_1_bh.R", echo = T, max.deparse.length = 100000)

# Stop logging and return output back to the console
sink()  # Close the standard output sink
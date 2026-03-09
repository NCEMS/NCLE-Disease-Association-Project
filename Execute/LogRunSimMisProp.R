# Generate a log file name with the current date and time
log_file <- paste0("Log/ScriptOutput_", paste0("af_70_sim_mis_prop", format(Sys.time(), "_%Y-%m-%d_%H-%M-%S"), ".log"))

# Redirect console output and messages to the log file
sink(log_file)

# Source the R script, logging all output
source("AnalysisCodes/sim_mis_prop.R", echo = T, max.deparse.length = 100000)

# Stop logging and return output back to the console
sink()  # Close the standard output sink
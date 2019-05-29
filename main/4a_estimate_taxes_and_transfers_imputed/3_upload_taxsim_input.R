library(RCurl)
library(dplyr)

source("functions/upload_taxsim_input_files.R")

taxsim_input_dir <- "main/4a_estimate_taxes_and_transfers_imputed/1_taxsim_input"

upload_taxsim_input_files(taxsim_input_dir)
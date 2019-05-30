library(RCurl)
library(dplyr)

source("functions/download_taxsim_output_files.R")

taxsim_input_dir <- "main/4b_estimate_taxes_and_transfers_non_imputed/1_taxsim_input"
taxsim_output_dir <- "main/4b_estimate_taxes_and_transfers_non_imputed/4_taxsim_output"

if (!dir.exists(taxsim_output_dir)) dir.create(taxsim_output_dir)

download_taxsim_output_files(taxsim_input_dir, taxsim_output_dir)
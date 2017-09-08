# Make sure TAXSIM results files have the same number of lines as TAXSIM 
# input files

source("functions/check_taxsim_input_output.R")

input_dir <- "main/4b_estimate_taxes_and_transfers_imputed/1_taxsim_input"
output_dir <- "main/4b_estimate_taxes_and_transfers_imputed/4_taxsim_output"

check_taxsim_input_output(input_dir, output_dir)
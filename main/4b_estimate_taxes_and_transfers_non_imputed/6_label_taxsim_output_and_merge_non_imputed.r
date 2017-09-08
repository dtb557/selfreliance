#############################################################################
# Label TAXSIM variables, merge with imputed data, save TAXSIM datasets     #
# in R format, and save imputed data with tax info                          #
#############################################################################

library(data.table)

taxsim_output_dir <- "main/4b_estimate_taxes_and_transfers_imputed/4_taxsim_output"

# Load cleaned data
load("main/1_clean_data/cleaned_data_step_5_no_imputation.Rdata")
d[ , id := serial*100 + pernum]
setkey(d, year, id)

# Load TAXSIM estimates
t <- fread(file.path(taxsim_output_dir, "sr1970_2010.taxsim"))
t <- t[ , paste0("V", 1:29), with=FALSE]
setnames(t, names(t), c("id", "tax_year", "state", "fed_inc_tax_liability", 
                        "state_inc_tax_liability", "fica", "fed_rate", "state_rate", 
                        "fica_rate", "fed_agi", "ui_in_agi", "ss_in_agi", 
                        "zero_bracket_amt", "personal_exemptions", "exemp_phaseout", 
                        "deduct_phaseout", "deduct_allowed", "fed_taxable_inc", 
                        "tax_on_inc", "exemp_surtax", "gen_tax_cred", "child_tax_cred", 
                        "addl_child_tax_cred", "child_care_cred", "eitc", "inc_for_amt", 
                        "amt_liability_after_cred", "fed_inc_tax_before_cred", "fica_alt"))
# save(t, file=file.path(taxsim_output_dir, "taxsim1970_2010.Rdata"))
t[ , year := tax_year + 1]
t <- t[ , .(year, 
            id, 
            fed_inc_tax_not_inflation_adj = fed_inc_tax_liability, 
            fica_not_inflation_adj = fica)]
setkey(t, year, id)

# Merge TAXSIM estimates into cleaned data and save
d <- t[d]
save(d, file=file.path("main/4b_estimate_taxes_and_transfers_imputed", 
                       "6_non_imp_data_post_tax.Rdata"))

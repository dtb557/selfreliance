# Make sure TAXSIM results files have the same number of lines as TAXSIM 
# input files

# in_files <- list.files("taxsim/taxsim_input/", pattern="^sr\\d{4}_\\d{1,2}$")
in_files <- list.files("taxsim/taxsim_input/", pattern="^sr2010_\\d{1,2}$")

taxsim_colnames <- c("id", "tax_year", "state_soi", "marital_status", "n_dep_exemp", 
                     "n_txpyrs_over_65", "incwage", "sp_incwage", "inc_dividend", 
                     "inc_oth_prpty", "inc_pension", "incss", "inc_oth_trns", 
                     "rent_paid", "re_taxes", "oth_deduct", "chld_care_exp", 
                     "unemp", "n_dep_under_17", "oth_oth_deduct", "st_cap_gain_loss", 
                     "lt_cap_gain_loss")

for(f in in_files) {
    cat(f, "\n")
    in_f <- read.table(paste0("taxsim/taxsim_input/", f), quote="", skip=1)
    names(in_f) <- taxsim_colnames
    out_f <- read.table(paste0("taxsim/taxsim_input/", f, ".taxsim"), quote="")
    if(nrow(in_f)==nrow(out_f)) next
    else stop(paste0("Length of input and output do not match for ", f))
}
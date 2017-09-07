#############################################################################
# Label TAXSIM variables, merge with imputed data, save TAXSIM datasets     #
# in R format, and save imputed data with tax info                          #
#############################################################################

library(data.table)
library(mice)

create_empty_imp_set <- function() {
    eval(
        parse(
            text=sprintf("data.frame(%s, check.names=F)", 
                         paste0('"', 1:10, '"', "=rep(0.0, nrow(imp$data))", 
                                collapse=", ")
            )
        )
    )
}

for(yr in seq(1970, 2010, 10)) {
    # if(yr %in% c(1970, 1980, 2010)) next
    imp_file <- paste0("Data/imp_iterations/with_ppc/imp_", yr, "_10_extreme_values_transposed.Rdata")
    if(!file.exists(imp_file)) next
    cat(yr, "")
    load(imp_file)
    # imp <- get(paste0("imp_", yr))
    # rm(list=paste0("imp_", yr))
    imp$data <- data.table(imp$data)
    imp$data[ , id := serial*100 + pernum]
    imp$data[ , orig_order := 1:nrow(imp$data)]
    setkey(imp$data, year, id)
    imp$imp$fed_inc_tax_not_inflation_adj <- create_empty_imp_set()
    imp$imp$fica_not_inflation_adj <- create_empty_imp_set()
    for(i in 1:10) {
        cat(i, "")
        t <- data.table(read.table(paste0("taxsim/taxsim_input/sr", yr, "_", i, ".taxsim")))
        t <- t[ , paste0("V", 1:29), with=FALSE]
        setnames(t, names(t), c("id", "tax_year", "state", "fed_inc_tax_liability", 
                     "state_inc_tax_liability", "fica", "fed_rate", "state_rate", 
                     "fica_rate", "fed_agi", "ui_in_agi", "ss_in_agi", 
                     "zero_bracket_amt", "personal_exemptions", "exemp_phaseout", 
                     "deduct_phaseout", "deduct_allowed", "fed_taxable_inc", 
                     "tax_on_inc", "exemp_surtax", "gen_tax_cred", "child_tax_cred", 
                     "addl_child_tax_cred", "child_care_cred", "eitc", "inc_for_amt", 
                     "amt_liability_after_cred", "fed_inc_tax_before_cred", "fica_alt"))
        save(t, file=paste0("taxsim/taxsim_input/taxsim_", yr, "_", i, ".Rdata"))
        t[ , year := tax_year + 1]
        t <- t[ , .(year, id, fed_inc_tax_liability, fica)]
        setkey(t, year, id)
        imp$data <- t[imp$data]
        rm(t)
        setorder(imp$data, orig_order)
        imp$imp$fed_inc_tax_not_inflation_adj[[i]] <- imp$data$fed_inc_tax_liability
        imp$imp$fica_not_inflation_adj[[i]] <- imp$data$fica
        imp$data[ , c("fed_inc_tax_liability", "fica") := NULL, with=FALSE]
    }
    imp$data[ , fed_inc_tax_not_inflation_adj := NA_real_]
    imp$data[ , fica_not_inflation_adj := NA_real_]
    imp$data[ , c("id", "orig_order") := NULL, with=FALSE]
    save(imp, file=paste0("Data/imputed_datasets_post_tax/imp_", yr, ".Rdata"))
    cat("\n")
}




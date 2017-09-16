# Fix sqrt_famsize
library(magrittr)
library(dplyr)

load("main/1_clean_data/cleaned_data_step_5.Rdata")
fixed_sqrt_famsize <- d %>%
    select(-sqrt_famsize) %>%
    group_by(year, serial, subfamid) %>%
    mutate(sqrt_famsize = sqrt(n())) %>%
    ungroup() %>%
    select(year, serial, pernum, sqrt_famsize)
rm(d)

for(yr in seq(1970, 2010, by=10)) {
    load(file.path("main/4a_estimate_taxes_and_transfers_imputed", 
                   sprintf("6_imp_post_tax_%d.Rdata", yr)))
    imp$data <- imp$data %>%
        rename(sqrt_hh_size = sqrt_famsize) %>%
        left_join(fixed_sqrt_famsize, by=c("year", "serial", "pernum"))
    names(imp$imp)[names(imp$imp) == "sqrt_famsize"] <- "sqrt_hh_size"
    save(imp, file=sprintf("main/5_fix_pre_imputation_problems/1_imp_%d_post_tax_fixed.Rdata", yr))
}

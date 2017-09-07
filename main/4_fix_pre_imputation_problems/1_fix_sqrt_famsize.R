# Fix sqrt_famsize
library(magrittr)
library(dplyr)

load("main/1_clean_data/cleaned_data_step_5.Rdata")
subfamid_matcher <- d %>%
    select(year, serial, pernum, subfamid)
rm(d)

for(yr in seq(1970, 2010, by=10)) {
    load(sprintf("main/3_multiply_impute/3_imp_%d_10_extreme_values_transposed.Rdata", yr))
    imp$data <- imp$data %>%
        rename(sqrt_hh_size = sqrt_famsize)
    names(imp$imp)[names(imp$imp) == "sqrt_famsize"] <- "sqrt_hh_size"
    imp$data <- imp$data %>%
        left_join(subfamid_matcher, by=c("year", "serial", "pernum")) %>%
        group_by(year, serial, subfamid) %>%
        mutate(sqrt_famsize=n()) %>%
        ungroup() %>%
        select(-subfamid)
    save(imp, file=sprintf("main/4_fix_pre_imputation_problems/1_imp_%d_10_fixed.Rdata"))
}

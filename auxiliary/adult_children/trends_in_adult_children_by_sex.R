# We have already discussed that we would like to look at the results without excluding the ~4% of families with dependent children older than 25. However, if that is quite involved, a first step could be to look at the proportion of cases that we drop due to that restriction not only by year but also by gender. This may show us that, although we are excluding increasingly large shares of the population over time, the trend is the same for men and women, so presumably the ‘bias’ from excluding these people should be the same across genders and perhaps not influence gender inequality trends.

# The output should be the percentage of households with a child over 25 in 
# the household for each decade year and each gender; something like a 
# 2 x 5 matrix

# I'm calculating the same thing for each gender by year group, so I 
# should make a function for that. The input is a gender 
# and a year, and the output is a data frame with variables gender, year, 
# and proportion of cases living with a child over 25

library(data.table)
library(mice)
library(purrr)

source("functions/limit_age_range.R")


# Functions ---------------------------------------------------------------

get_ppn_for_decade <- function(decade, extra_df, keys) {
    imp <- load_decade(decade)
    long_dt <- map_dfr_mice(
        imp, 
        compute_ppn_w_adult_child, 
        extra_df = extra_df, 
        keys = keys
    )
    long_dt <- long_dt[ , .(ppn_w_adult_child = mean(ppn_w_adult_child), decade = decade[1]), by = "sex"]
    long_dt
}

load_decade <- function(decade) {
    load(sprintf("main/5_fix_pre_imputation_problems/1_imp_%d_post_tax_fixed.Rdata", decade))
    return(imp)
}



map_dfr_mice <- function(imp, fcn, ...) {
    map_dfr(
        1:imp$m, 
        function(i) {
            dimp <- data.table(complete(imp, i))
            fcn(dimp, ...)
        }
    )
}

compute_ppn_w_adult_child <- function(data, extra_df, keys) {
    data <- merge_extra_vars(data, extra_df, keys)
    
    setkey(data, year, serial, subfamid)
    
    data[ , child_25_or_over := relate == "Child" & age >= 25 & marst == "Never married/single"]
    data[ , fam_has_adult_child := any(child_25_or_over), by = key(data)]
    
    data <- heads_and_partners_only(data)
    
    data <- make_partner_age(data)
    
    data <- limit_age_range(data)
    
    data[wtsupp < 0, wtsupp := 0]
    
    data[ , decade := signif(year, 3)]
    
    data[ , .(ppn_w_adult_child = Hmisc::wtd.mean(fam_has_adult_child, wtsupp)), 
          by = .(decade, sex)]
}

merge_extra_vars <- function(data, extra_df, keys) {
    setkeyv(data, keys)
    setkeyv(extra_df, keys)
    return(extra_df[data])
}

heads_and_partners_only <- function(data) {
    data[pernum == subfamid | pnloc > 0, ]
}

make_partner_age <- function(data) {
    partner <- data[pnloc > 0, .(year, serial, pernum=pnloc, pn_age=age)]
    setkey(partner, year, serial, pernum)
    setkey(data, year, serial, pernum)
    partner[data]
}

# Main program ------------------------------------------------------------
load("main/1_clean_data/cleaned_data_step_5.Rdata")
d <- d[ , .(pnloc, subfamid, wtsupp, relate), keyby=.(year, serial, pernum)]

decades <- c(1970, 1980, 1990, 2000, 2010)

out <- map_dfr(
    decades, 
    get_ppn_for_decade, 
    extra_df = d, 
    keys = c("year", "serial", "pernum")
)

readr::write_csv(out, "auxiliary/adult_children/trends_in_adult_children_by_sex.csv")

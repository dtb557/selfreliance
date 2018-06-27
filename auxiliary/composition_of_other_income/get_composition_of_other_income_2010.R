# Determine the composition of other income in 2010
library(purrr)
library(dplyr)
library(data.table)
library(Hmisc)
library(readr)

out_dir <- "auxiliary/composition_of_other_income/"

load("main/6a_make_analysis_dataset_imputed/1_imps_2010_analysis_vars.Rdata")

source("functions/limit_age_range.R")
source("functions/adjust_for_family_size.R")

get_oth_inc_breakdown <- function(data) {
    data <- limit_age_range(data)
    data <- adjust_for_family_size(data)
    data <- data[ , .SD[1], by = .(year, serial, subfamid)]
    data <- data[other_inc != 0, ]
    everyone <- make_pct_vars(data)
    everyone <- get_avg_of_pct_vars(everyone)
    positive <- data[other_inc > 0, ]
    positive <- make_pct_vars(positive)
    positive <- get_avg_of_pct_vars(positive)
    negative <- data[other_inc < 0, ]
    negative <- make_pct_vars(negative)
    negative <- get_avg_of_pct_vars(negative)
    list(everyone = everyone, positive = positive, negative = negative)
}

make_pct_vars <- function(data) {
    data[
        ,
        .(
            pct_unearned_non_gov = 100 * fam_unearned_non_gov / other_inc,
            pct_unearned_gov = 100 * fam_unearned_gov / other_inc, 
            pct_tax = 100 * fam_tax / other_inc,
            pct_oth_labern = 100 * oth_labern / other_inc, 
            wtsupp = wtsupp
        )
    ]
}

get_avg_of_pct_vars <- function(data) {
    data[
        ,
        .(
            pct_unearned_non_gov = wtd.mean(pct_unearned_non_gov, wtsupp),
            pct_unearned_gov = wtd.mean(pct_unearned_gov, wtsupp), 
            pct_tax = wtd.mean(pct_tax, wtsupp), 
            pct_oth_labern = wtd.mean(pct_oth_labern, wtsupp)
        )
    ]
}

combine_fcn <- function(x, y) {
    list(everyone = x$everyone + y$everyone, 
         positive = x$positive + y$positive, 
         negative = x$negative + y$negative)
}

breakdowns <- map(
    imps,
    get_oth_inc_breakdown
)

sum_breakdown <- reduce(breakdowns, combine_fcn)

write_csv(sum_breakdown$everyone / 10, file.path(out_dir, "everyone.csv"))
write_csv(sum_breakdown$positive / 10, file.path(out_dir, "positive.csv"))
write_csv(sum_breakdown$negative / 10, file.path(out_dir, "negative.csv"))
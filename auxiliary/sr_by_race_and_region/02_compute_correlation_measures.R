# Compute "standardized" and "unstandardized" correlations from decomp 
# component tables for men and women among whites only, blacks only, 
# southerners only, and non-southerners only

# Table should have panels:
## Unstandardized
## Standardized
# ...each with rows
### 1970 self-reliance
### 2010 self-reliance
### raw change
### percent change
# ... and columns
### Women, African-American
### Women, White
### Men, African-American
### Men, White
### Women, Southern U.S.
### Women, non-Southern U.S.
### Men, Southern U.S.
### Men, non-Southern U.S.


# Load packages -----------------------------------------------------------
library(tidyverse)


# Specify directories -----------------------------------------------------
out_dir <- "auxiliary/sr_by_race_and_region"
in_dir <- file.path(out_dir, "01_decomp_component_tables")


# Define functions --------------------------------------------------------
sr_table_by_corr_type <- function(dat.decomps, corr_fcn) {
    out <- pmap_dfc(
        list(
            dat.decomps, 
            list(c("African-American", "White"), c("South", "Not South")), 
            list(corr_fcn)
        ), 
        sr_table_by_group
    )
    raw_change <- summarise_all(out, ~ .[2] - .[1])
    pct_change <- summarise_all(out, ~ 100 * (.[2] - .[1]) / .[1])
    out <- bind_rows(out, raw_change, pct_change)
    out <- as.data.frame(out)
    row.names(out) <- c(paste(c(1970, 2010), "self-reliance"), "Raw change", "% change")
    out
}

sr_table_by_group <- function(dat.decomp, groups, corr_fcn) {
    sexes <- c("Female", "Male")
    pmap_dfc(
        list(
            list(dat.decomp),
            rep(sexes, each = 2),
            rep(groups, 2), 
            list(corr_fcn)
        ), 
        make_col
    )
}

make_col <- function(dat.decomp, sex, group, corr_fcn) {
    sex_filter <- dat.decomp[[1]] == sex
    decade_filter <- dat.decomp[[2]] %in% c(1970, 2010)
    group_filter <- dat.decomp[[3]] == group
    d <- dat.decomp[decade_filter & group_filter & sex_filter, ]
    col_name <- paste(sex, group, sep = ", ")
    out_col <- list(corr_fcn(d))
    names(out_col) <- col_name
    out_col
}

unstd_corr <- function(dat.decomp) {
    dat.decomp$r_g
}

std_corr <- function(dat.decomp) { 
    # (calculate group-specific grand-mean-standardized correlations)
    dat.decomp <- mutate(
        dat.decomp, 
        std_corr = 
            (
                (r_g * sigma_gx * sigma_gy + 
                 mu_gx * mu_gy - 
                 mu_gx * mu_y - 
                 mu_x * mu_gy + 
                 mu_x * mu_y) /
                (sigma_x * sigma_y) 
            ) /
            (
                sqrt(sigma_gx^2 + mu_gx^2 - 2 * mu_gx * mu_x + mu_x^2) *
                sqrt(sigma_gy^2 + mu_gy^2 - 2 * mu_gy * mu_y + mu_y^2) /
                (sigma_x * sigma_y)
            )
    )
    dat.decomp$std_corr
}

list_avg <- function(l) {
    purrr::reduce(l, `+`) / length(l)
}



# Compute measures --------------------------------------------------------
data <- map2(
    rep(list(c("race", "region")), 10), 
    1:10,
    function(x, y) {
        map(
            x, 
            ~ suppressMessages(
                read_csv(file.path(in_dir, sprintf("decomp_components_imputed_%s_fa_%d.csv", ., y)))
            ) 
        )
    }
)

tbl_lists <- map(
    list(std_corr, unstd_corr), 
    function(f) {
        map(
            data, 
            ~ sr_table_by_corr_type(., f)
        )
    }
)

tbls <- map(tbl_lists, list_avg)


# Write output ------------------------------------------------------------
walk2(
    tbls, 
    c("standardized_corr", "unstandardized_corr"), 
    ~ write.csv(
        .x, 
        file.path(out_dir, sprintf("02_%s.csv", .y))
    )
)

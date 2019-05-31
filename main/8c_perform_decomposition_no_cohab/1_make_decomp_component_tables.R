# Get quantities of interest for Deirdre's memo with 
# two new decompositions

# Quantities needed

# 1. Group share of population
# 2. Within-group correlation between labern and faminc
# 3. Within-group standard deviation of labern
# 4. Overall standard deviation of labern
# 5. Within-group standard deviation of faminc
# 6. Overall standard deviation of faminc
# 7. Group mean of labern
# 8. Grand mean of labern
# 9. Group mean of faminc
# 10. Grand mean of faminc
# 11. Within-group covariance of labern and faminc
# 12. Within-group variance of labern
# 13. Within-group covariance of labern and pn_labern
# 14. Within-group covariance of labern and other_inc

library(data.table)
library(Hmisc)
library(weights)

source("functions/save_imp_decomp_component_tables.R")

fam_adj <- TRUE
exclude_alloc <- FALSE
exclude_top_2_pct <- FALSE
exclude_top_decile_female_earners <- FALSE
exclude_top_decile_male_earners <- FALSE

IN_DIR <- "main/6c_make_analysis_dataset_no_cohab"
OUT_DIR <- "main/8c_perform_decomposition_no_cohab/1_decomp_component_tables"

if (!dir.exists(OUT_DIR)) dir.create(OUT_DIR)

save_imp_decomp_component_tables(
    IN_DIR, OUT_DIR,
    fam_adj = fam_adj, 
    exclude_alloc = exclude_alloc, 
    exclude_top_2_pct = exclude_top_2_pct, 
    exclude_top_decile_female_earners = exclude_top_decile_female_earners, 
    exclude_top_decile_male_earners = exclude_top_decile_male_earners
)


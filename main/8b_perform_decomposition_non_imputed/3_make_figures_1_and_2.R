source("functions/make_figures_1_2_and_3_non_imputed.R")

IN_DIR <- "main/8b_perform_decomposition_non_imputed/2_qois_for_tables_and_figs"
OUT_DIR <- "main/8b_perform_decomposition_non_imputed/3_figures_1_2_and_3"

if (!dir.exists(OUT_DIR)) dir.create(OUT_DIR)

fam_adj <- TRUE
exclude_alloc <- FALSE
exclude_top_2_pct <- FALSE
exclude_top_decile_female_earners <- FALSE
exclude_top_decile_male_earners <- FALSE

make_figures_1_2_and_3_non_imputed(
    IN_DIR, OUT_DIR,
    fam_adj = fam_adj, 
    exclude_alloc = exclude_alloc, 
    exclude_top_2_pct = exclude_top_2_pct, 
    exclude_top_decile_female_earners = exclude_top_decile_female_earners, 
    exclude_top_decile_male_earners = exclude_top_decile_male_earners
)

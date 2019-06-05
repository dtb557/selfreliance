library(data.table)
library(Hmisc)
library(weights)

source("functions/build_filename_suffix.R")
source("functions/make_qois_for_tables_and_figs.R")

fam_adj <- TRUE
exclude_alloc <- FALSE
exclude_top_2_pct <- FALSE
exclude_top_decile_female_earners <- FALSE
exclude_top_decile_male_earners <- FALSE

IN_DIR <- "main/6b_make_analysis_dataset_non_imputed"
OUT_DIR <- "main/8b_perform_decomposition_non_imputed/2_qois_for_tables_and_figs"

if (!dir.exists(OUT_DIR)) dir.create(OUT_DIR)

save_qois_for_tables_and_figs <- function(IN_DIR, OUT_DIR, 
                                          fam_adj, 
                                          exclude_alloc, 
                                          exclude_top_2_pct, 
                                          exclude_top_decile_female_earners, 
                                          exclude_top_decile_male_earners) {
    suffix <- build_filename_suffix(
        fam_adj, 
        exclude_alloc, 
        exclude_top_2_pct, 
        exclude_top_decile_female_earners, 
        exclude_top_decile_male_earners
    )
    
    data_file <- "1_non_imputed_analysis_vars.Rdata"
    load(file.path(IN_DIR, data_file))
    
    for(yr in seq(1970, 2010, 10)) {
        cat(yr, "")
        qoi <- make_qois_for_tables_and_figs(
            d[year %in% (yr-1):(yr+1), ], 
            fam_adj, 
            exclude_alloc, 
            exclude_top_2_pct, 
            exclude_top_decile_female_earners, 
            exclude_top_decile_male_earners
        )
        
        outfile <- file.path(
            OUT_DIR, 
            sprintf("qoi_non_imp_%d_%s.Rdata", yr, suffix)
        )
        save(qoi, file=outfile)
    }
}

save_qois_for_tables_and_figs(
    IN_DIR, OUT_DIR,
    fam_adj = fam_adj, 
    exclude_alloc = exclude_alloc, 
    exclude_top_2_pct = exclude_top_2_pct, 
    exclude_top_decile_female_earners = exclude_top_decile_female_earners, 
    exclude_top_decile_male_earners = exclude_top_decile_male_earners
)
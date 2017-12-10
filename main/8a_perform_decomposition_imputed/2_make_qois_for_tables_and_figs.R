library(data.table)
library(Hmisc)
library(weights)

source("functions/build_filename_suffix.R")
source("functions/make_qois_for_tables_and_figs.R")

IN_DIR <- "main/6a_make_analysis_dataset_imputed"
OUT_DIR <- "main/8a_perform_decomposition_imputed/2_qois_for_tables_and_figs"

save_qois_for_tables_and_figs <- function(fam_adj = TRUE, 
                                          exclude_alloc = FALSE, 
                                          exclude_top_2_pct = TRUE, 
                                          exclude_top_decile_female_earners = FALSE, 
                                          exclude_top_decile_male_earners = FALSE) {
    suffix <- build_filename_suffix(
        fam_adj, 
        exclude_alloc, 
        exclude_top_2_pct, 
        exclude_top_decile_female_earners, 
        exclude_top_decile_male_earners
    )
    
    for(yr in seq(1970, 2010, 10)) {
        cat(yr, "")
        imp_file <- sprintf("1_imps_%d_analysis_vars.Rdata", yr)
        load(file.path(IN_DIR, imp_file))
        qoi <- lapply(imps, 
                      make_qois_for_tables_and_figs, 
                      fam_adj, 
                      exclude_alloc, 
                      exclude_top_2_pct, 
                      exclude_top_decile_female_earners, 
                      exclude_top_decile_male_earners
        )
        
        outfile <- file.path(
            OUT_DIR, 
            sprintf("qoi_imp_%d_%s.Rdata", yr, suffix)
        )
        save(qoi, file=outfile)
    }
}

save_qois_for_tables_and_figs() # defaults to fam_adj and exclude_top_2_pct
save_qois_for_tables_and_figs(exclude_top_2_pct = FALSE)
save_qois_for_tables_and_figs(fam_adj = FALSE)
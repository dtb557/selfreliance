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

source("functions/make_decomp_component_table.R")
source("functions/build_filename_suffix.R")

out <- vector(mode="list", length=10)

OUT_DIR <- "main/8b_perform_decomposition_imputed/1_decomp_component_tables"



save_non_imp_decomp_component_tables <- function(
    fam_adj = TRUE, 
    exclude_alloc = FALSE, 
    exclude_top_2_pct = TRUE, 
    exclude_top_decile_female_earners = FALSE, 
    exclude_top_decile_male_earners = FALSE
    ) {
    
        suffix <- build_filename_suffix(fam_adj, exclude_alloc, 
                                        exclude_top_2_pct, 
                                        exclude_top_decile_female_earners, 
                                        exclude_top_decile_male_earners)
        
        cat(paste0("Options: ", suffix, "\n\n"))
        
        load("main/6b_make_analysis_dataset_non_imputed/1_non_imputed_analysis_vars.Rdata")
        
        tbl <- make_decomp_component_table(
            d, 
            fam_adj = fam_adj, 
            exclude_alloc = exclude_alloc, 
            exclude_top_2_pct = exclude_top_2_pct, 
            exclude_top_decile_female_earners = exclude_top_decile_female_earners, 
            exclude_top_decile_male_earners = exclude_top_decile_male_earners
        )
        
        outfile <- 
            file.path(OUT_DIR, 
                      sprintf("decomp_components_non_imputed_%s.csv", 
                              suffix)
            )
        
        write.csv(tbl, file=outfile, row.names=FALSE)
        
}

save_imp_decomp_component_tables() # defaults to fam_adj and exclude_top_2_pct
save_imp_decomp_component_tables(fam_adj = FALSE)


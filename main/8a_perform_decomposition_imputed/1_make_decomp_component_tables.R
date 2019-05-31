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

fam_adj <- TRUE
exclude_alloc <- FALSE
exclude_top_2_pct <- FALSE
exclude_top_decile_female_earners <- FALSE
exclude_top_decile_male_earners <- FALSE

source("functions/make_decomp_component_table.R")
source("functions/build_filename_suffix.R")

out <- vector(mode="list", length=10)

IN_DIR <- "main/6a_make_analysis_dataset_imputed"
OUT_DIR <- "main/8a_perform_decomposition_imputed/1_decomp_component_tables"

if (!dir.exists(OUT_DIR)) dir.create(OUT_DIR)

save_imp_decomp_component_tables <- function(
    IN_DIR, OUT_DIR,
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
        
        out <- vector(mode="list", length=10)
        
        for(yr in seq(1970, 2010, 10)) {
            cat(yr, "")
            imp_file <- sprintf("1_imps_%d_analysis_vars.Rdata", yr)
            load(file.path(IN_DIR, imp_file))
            for(i in 1:10) {
                cat(i, "")
                
                tbl <- make_decomp_component_table(
                    imps[[i]], 
                    fam_adj, 
                    exclude_alloc, 
                    exclude_top_2_pct, 
                    exclude_top_decile_female_earners, 
                    exclude_top_decile_male_earners
                )
                
                out[[i]] <- rbindlist(list(out[[i]], tbl))
            }
            cat("\n")
        }
        
        cat("Saving individual component files...\n")
        
        for(i in seq_along(out)) {
            outfile <- 
                file.path(OUT_DIR, 
                          sprintf("decomp_components_imputed_%s_%d.csv", 
                                suffix, i)
            )
            write.csv(out[[i]], file=outfile, row.names=FALSE)
        }
        
}

save_imp_decomp_component_tables(
    IN_DIR, OUT_DIR,
    fam_adj = fam_adj, 
    exclude_alloc = exclude_alloc, 
    exclude_top_2_pct = exclude_top_2_pct, 
    exclude_top_decile_female_earners = exclude_top_decile_female_earners, 
    exclude_top_decile_male_earners = exclude_top_decile_male_earners
)

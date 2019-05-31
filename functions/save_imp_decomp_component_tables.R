source("functions/make_decomp_component_table.R")
source("functions/build_filename_suffix.R")

save_imp_decomp_component_tables <- function(
    IN_DIR, OUT_DIR,
    fam_adj, 
    exclude_alloc, 
    exclude_top_2_pct, 
    exclude_top_decile_female_earners, 
    exclude_top_decile_male_earners
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
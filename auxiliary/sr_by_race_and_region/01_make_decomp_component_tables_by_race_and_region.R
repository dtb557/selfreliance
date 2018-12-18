library(data.table)
library(Hmisc)
library(weights)

source("functions/make_decomp_component_table.R")
source("functions/build_filename_suffix.R")

out <- vector(mode="list", length=10)

IN_DIR <- "main/6a_make_analysis_dataset_imputed"
OUT_DIR <- "auxiliary/sr_by_race_and_region/01_decomp_component_tables"

save_imp_decomp_component_tables <- function(
    group_var,
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
            
            if(group_var == "race") {
                imps[[i]] <- imps[[i]][race %in% c("White", "African-American"), ]
                imps[[i]][ , race := forcats::fct_drop(race)]
            }
            if(group_var == "region") {
                imps[[i]][ , region := as.integer(region)]
                imps[[i]][ , region := factor(ifelse(region %in% 31:33, "South", "Not South"))]
            }
            
            tbl <- make_decomp_component_table(
                imps[[i]], 
                fam_adj, 
                exclude_alloc, 
                exclude_top_2_pct, 
                exclude_top_decile_female_earners, 
                exclude_top_decile_male_earners, 
                group_var, 
                compute_std_corr = TRUE
            )
            
            out[[i]] <- rbindlist(list(out[[i]], tbl))
        }
        cat("\n")
    }
    
    cat("Saving individual component files...\n")
    
    for(i in seq_along(out)) {
        outfile <- 
            file.path(OUT_DIR, 
                      sprintf("decomp_components_imputed_%s_%s_%d.csv", 
                              group_var, suffix, i)
            )
        write.csv(out[[i]], file=outfile, row.names=FALSE)
    }
    
    all <- rbindlist(out)
    
    non_key_vars <- setdiff(names(all), c("sex", "decade", group_var))
    
    cat("Averaging across imputations...\n")
    
    setkeyv(all, c("sex", "decade", group_var))
    
    avg <- all[ , (non_key_vars) := lapply(.SD, mean, na.rm = TRUE), .SDcols = non_key_vars, 
                by = key(all)]
    
    outfile <- file.path(OUT_DIR, 
                         sprintf("decomp_components_imputed_%s_%s_avg.csv", group_var, suffix)
    )
    
    write.csv(avg, file = outfile, row.names = FALSE)
    
}

save_imp_decomp_component_tables(group_var = "race", exclude_top_2_pct = FALSE)
save_imp_decomp_component_tables(group_var = "region", exclude_top_2_pct = FALSE)

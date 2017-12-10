library(data.table)

source("functions/make_decile_groups.R")
source("functions/limit_age_range.R")
source("functions/exclude_top_2_pct.R")
source("functions/adjust_for_family_size.R")

IN_DIR <- "main/6a_make_analysis_dataset_imputed"
IN_FILE_PATTERN <- "1_imps_%d_analysis_vars.Rdata"
OUT_DIR <- "auxiliary/earnings_and_income_means_by_decile"
OUT_FILE_PATTERN <- "%s_means_by_sex_and_decile_%d.csv"

yrs <- c(1970, 2010)

for(yr in yrs) {
    
    cat(yr)
    # Load analysis datasets
    in_file <- file.path(
        IN_DIR, 
        sprintf(
            IN_FILE_PATTERN, 
            yr
        )
    )
    load(in_file)
    
    out <- lapply(
        imps, 
        function(data) {
            data <- limit_age_range(data)
            
            # Set cases with negative weight values to zero
            data[wtsupp < 0, wtsupp := 0]
            
            # fam_adj
            data <- adjust_for_family_size(data)
            
            # exclude_top_2_pct
            data <- exclude_top_2_pct(data, flag_version = "non-imputed")
            
            # Set small labern values to zero
            # data[labern < 1, labern := 0]
            
            # Make decile groups
            data[
                labern > 0, 
                labern_decile := make_decile_groups(labern, wtsupp),
                by = sex
                ]
            
            data[
                fam_inc > 0,
                fam_inc_decile := make_decile_groups(fam_inc, wtsupp), 
                by = sex
                ]
            
            # Compute means by group
            labern_means <- 
                data[
                    labern > 0,
                    .(labern_mean = wtd.mean(labern, wtsupp)), 
                    by = .(sex, labern_decile)
                    ]
            
            fam_inc_means <-
                data[
                    fam_inc > 0,
                    .(fam_inc_mean = wtd.mean(fam_inc, wtsupp)), 
                    by = .(sex, fam_inc_decile)
                    ]
            return(
                list(
                    labern_means = labern_means, 
                    fam_inc_means = fam_inc_means
                )
            )
        }
    )
        
    labern_means <- lapply(
        out, `[[`, "labern_means"
    )
    labern_means <- rbindlist(labern_means)
    labern_means <- 
        labern_means[
            ,
            .(labern_mean = mean(labern_mean)), 
            by = .(sex, labern_decile)
        ]
    
    fam_inc_means <- lapply(
        out, `[[`, "fam_inc_means"
    )
    fam_inc_means <- rbindlist(fam_inc_means)
    fam_inc_means <- 
        fam_inc_means[
            ,
            .(fam_inc_mean = mean(fam_inc_mean)), 
            by = .(sex, fam_inc_decile)
        ]
    
    # Save to csv
    labern_file <- file.path(
        OUT_DIR, 
        sprintf(
            OUT_FILE_PATTERN, 
            "labern", 
            yr
        )
    )
    
    fam_inc_file <- file.path(
        OUT_DIR, 
        sprintf(
            OUT_FILE_PATTERN, 
            "fam_inc", 
            yr
        )
    )
    
    fwrite(labern_means, file = labern_file)
    fwrite(fam_inc_means, file = fam_inc_file)
}

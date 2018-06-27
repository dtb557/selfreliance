library(data.table)

source("functions/make_decile_groups.R")
source("functions/limit_age_range.R")
source("functions/exclude_top_2_pct.R")
source("functions/adjust_for_family_size.R")

IN_DIR <- "main/6a_make_analysis_dataset_imputed"
IN_FILE_PATTERN <- "1_imps_%d_analysis_vars.Rdata"
OUT_DIR <- "auxiliary/earnings_and_income_means_by_decile"
OUT_FILE_PATTERN <- "labern_fam_inc_means_by_sex_and_labern_decile_%d_zero_earners_included.csv"

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
                , 
                labern_decile := ordered(
                    ifelse(
                        labern > 0, 
                        as.character(labern_decile), 
                        "Zero earners"
                    ), 
                    levels = c(
                        "Zero earners", 
                        levels(labern_decile)
                    )
                )]
            
            # data[
            #     fam_inc > 0,
            #     fam_inc_decile := make_decile_groups(fam_inc, wtsupp), 
            #     by = sex
            #     ]
            
            # Compute means by group
            both_means <- 
                data[
                    ,
                    .(labern_mean = wtd.mean(labern, wtsupp), 
                      fam_inc_mean = wtd.mean(fam_inc, wtsupp)), 
                    by = .(sex, labern_decile)
                    ]
            
            # fam_inc_means <-
            #     data[
            #         fam_inc > 0,
            #         .(fam_inc_mean = wtd.mean(fam_inc, wtsupp)), 
            #         by = .(sex, labern_decile)
            #         ]
            return(both_means)
        }
    )
        
    both_means <- rbindlist(out)
    both_means <- 
        both_means[
            ,
            .(labern_mean = mean(labern_mean), 
              fam_inc_mean = mean(fam_inc_mean)), 
            by = .(sex, labern_decile)
        ]
    
    # fam_inc_means <- lapply(
    #     out, `[[`, "fam_inc_means"
    # )
    # fam_inc_means <- rbindlist(fam_inc_means)
    # fam_inc_means <- 
    #     fam_inc_means[
    #         ,
    #         .(fam_inc_mean = mean(fam_inc_mean)), 
    #         by = .(sex, fam_inc_decile)
    #     ]
    
    # Save to csv
    both_file <- file.path(
        OUT_DIR, 
        sprintf(
            OUT_FILE_PATTERN, 
            yr
        )
    )
    
    # fam_inc_file <- file.path(
    #     OUT_DIR, 
    #     sprintf(
    #         OUT_FILE_PATTERN, 
    #         "fam_inc", 
    #         yr
    #     )
    # )
    
    fwrite(both_means, file = both_file)
    # fwrite(fam_inc_means, file = fam_inc_file)
}

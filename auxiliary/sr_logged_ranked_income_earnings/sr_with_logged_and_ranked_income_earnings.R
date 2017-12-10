library(data.table)
library(purrr)
source("functions/safe_log.R")
source("functions/fixed_wtd.rank.R")
source("functions/adjust_for_family_size.R")
source("functions/exclude_top_2_pct.R")
source("functions/limit_age_range.R")

IN_DIR <- "main/6a_make_analysis_dataset_imputed"
OUT_DIR <- "auxiliary/sr_logged_ranked_income_earnings"
OUT_FILE_ALL_YRS <- "sr_four_ways_all_yrs.csv"
OUT_FILE_70_10 <- "sr_four_ways_1970_2010.csv"

yrs <- seq(1970, 2010, 10)

sr_four_ways <- map_dfr(
    yrs, 
    function(yr) {
        cat(yr, "")
        imp_file <- sprintf("1_imps_%d_analysis_vars.Rdata", yr)
        load(file.path(IN_DIR, imp_file))
        
        sr_four_ways <- map_dfr(
            imps, 
            function(d) {
                d <- limit_age_range(d)
                d <- adjust_for_family_size(d)
                
                # Compute transformed labern and fam_inc variables
                d[ , log_labern := safe_log(labern)]
                d[ , log_fam_inc := safe_log(fam_inc)]
                d[ , rank_labern := fixed_wtd.rank(labern, weights = wtsupp), by = sex]
                d[ , rank_fam_inc := fixed_wtd.rank(fam_inc, weights = wtsupp), by = sex]
                
                # Compute SR untransformed; SR log transformed; SR rank transformed
                d[ , sr_raw := weights::wtd.cors(labern, fam_inc, weight=wtsupp)[1], by = sex]
                d[ , sr_log := weights::wtd.cors(log_labern, fam_inc, weight=wtsupp)[1], by = sex]
                d[ , sr_rank := weights::wtd.cors(rank_labern, rank_fam_inc, weight=wtsupp)[1], by = sex]
                d_summary <- d[ , .SD[1], .SDcols = c("sr_raw", "sr_log", "sr_rank"), 
                                keyby = sex]
            
                # Compute SR untransformed, top 2 pct excluded
                d <- exclude_top_2_pct(d)
                
                d <- d[ , sr_top_2_excluded := weights::wtd.cors(labern, fam_inc, weight=wtsupp)[1], by = sex]
                d_summary_2 <- d[ , .(sr_top_2_excluded = sr_top_2_excluded[1]), 
                                  keyby = sex]
                
                d_summary_2[d_summary][ , year := yr]
                
            }
        )
        sr_four_ways[ , .(
            sr_raw = mean(sr_raw), 
            sr_top_2_excluded = mean(sr_top_2_excluded), 
            sr_log = mean(sr_log), 
            sr_rank = mean(sr_rank)
        ), by = .(year, sex)]
    }
)

fwrite(sr_four_ways, file.path(OUT_DIR, OUT_FILE_ALL_YRS))

sr_four_ways_70_10 <- matrix(0.0, nrow = 4, ncol = 4, 
                             dimnames = list(
                                 c(
                                     "Untransformed, full distribution", 
                                     "Untransformed, excluding top 2%", 
                                     "Log transformed", 
                                     "Rank transformed"
                                 ), 
                                 c(
                                     "Men 1970", "Men 2010", 
                                     "Women 1970", "Women 2010"
                                 )
                             )
)

sr_vars <- c("sr_raw", "sr_top_2_excluded", "sr_log", "sr_rank")

sr_four_ways_70_10[ , "Men 1970"] <- as.numeric(
    sr_four_ways[year == 1970 & sex == "Male", sr_vars, with = FALSE]
)
sr_four_ways_70_10[ , "Men 2010"] <- as.numeric(
    sr_four_ways[year == 2010 & sex == "Male", sr_vars, with = FALSE]
)
sr_four_ways_70_10[ , "Women 1970"] <- as.numeric(
    sr_four_ways[year == 1970 & sex == "Female", sr_vars, with = FALSE]
)
sr_four_ways_70_10[ , "Women 2010"] <- as.numeric(
    sr_four_ways[year == 2010 & sex == "Female", sr_vars, with = FALSE]
)

write.csv(sr_four_ways_70_10, file.path(OUT_DIR, OUT_FILE_70_10))

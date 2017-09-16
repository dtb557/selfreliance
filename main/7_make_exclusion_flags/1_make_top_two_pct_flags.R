library(data.table)
library(Hmisc)
library(weights)

# Make non-imputed top two percent flag
load("main/6b_make_analysis_dataset_non_imputed/1_non_imputed_analysis_vars.Rdata")

# Limit ages
d <- d[age %in% 25:54 & (is.na(pn_age) | pn_age %in% 25:54), ]

# Remove top X% to deal with topcoding
make_flags_for_top_2_pct <- function(data) {
    cut_above <- data[ , .(fam_inc=fam_inc[1], wtsupp=wtsupp[1]), by=.(year, serial)][ , wtd.quantile(fam_inc, weights=wtsupp, probs=.98), by=year]
    threshold_by_year <- with(cut_above, structure(V1, names=year))
    data[ , top_2_pct := fam_inc > threshold_by_year[as.character(year)]]
    
    data[ , .(top_2_pct, year, serial, pernum)]
}

top_2_flag_non_imp <- make_flags_for_top_2_pct(d)

save(top_2_flag_non_imp, file="main/7_make_exclusion_flags/1_top_2_pct_flag_non_imp.Rdata")

rm(d)

# Make imputed top two percent flag
top_2_flag_imps <- lapply(seq(1970, 2010, 10), 
    function(yr) {
        load(file.path("main/6a_make_analysis_dataset_imputed", 
                       sprintf("1_imps_%d_analysis_vars.Rdata", yr)))
        fam_inc_dts <- lapply(imps, function(imp) {
            imp[ , .(fam_inc, wtsupp), keyby=.(year, serial, pernum)]
        })
        fam_inc_long <- rbindlist(fam_inc_dts)
        fam_inc_avg <- fam_inc_long[ , .(fam_inc=mean(fam_inc), wtsupp=wtsupp[1]), 
                                     by=.(year, serial, pernum)]
        make_flags_for_top_2_pct(fam_inc_avg)
    }
)

top_2_flag_imp <- rbindlist(top_2_flag_imps)
    
save(top_2_flag_imp, file="main/7_make_exclusion_flags/1_top_2_pct_flag_imp.Rdata")
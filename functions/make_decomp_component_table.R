make_decomp_component_table <- function(data, fam_adj = TRUE, exclude_alloc = FALSE, 
             exclude_top_2_pct = TRUE, exclude_top_decile_female_earners = FALSE, 
             exclude_top_decile_male_earners = FALSE) {
    
    require(Hmisc)
    require(weights)
    source("functions/fixed_wtd.rank.R")
    
    out <- list()
    
    source("functions/limit_age_range.R")
    data <- limit_age_range(data)
    
    if(fam_adj) {
        source("functions/adjust_for_family_size.R")
        data <- adjust_for_family_size(data)
    }
    
    if(exclude_alloc) {
        # Remove cases with allocated own or partner labor earnings
        source("functions/exclude_alloc.R")
        data <- exclude_alloc(data)
    }
    
    if(exclude_top_2_pct) {
        source("functions/exclude_top_2_pct.R")
        data <- exclude_top_2_pct(data, flag_version = "non-imputed")
    }
    
    if(exclude_top_decile_female_earners) {
        source("functions/exclude_hhs_with_top_decile_female_earner.R")
        data <- exclude_hhs_with_top_decile_female_earner(data)
    }
    
    if(exclude_top_decile_male_earners) {
        source("functions/exclude_hhs_with_top_decile_male_earner.R")
        data <- exclude_hhs_with_top_decile_male_earner(data)
    }
    
    data[wtsupp < 0, wtsupp := 0]
    
    data[ , decade := signif(year, 3)]
    
    # First, create population-level (sex and year specific) variables
    setkey(data, sex, decade)
    data[ , sigma_x := sqrt(wtd.var(labern, wtsupp)), by=key(data)]
    data[ , sigma_y := sqrt(wtd.var(fam_inc, wtsupp)), by=key(data)]
    data[ , mu_x := wtd.mean(labern, wtsupp), by=key(data)]
    data[ , mu_y := wtd.mean(fam_inc, wtsupp), by=key(data)]
    data[ , mu_other := wtd.mean(other_inc, wtsupp), by=key(data)]
    data[ , mu_pn := wtd.mean(pn_labern, wtsupp), by=key(data)]
    data[ , total_pop := sum(wtsupp), by=key(data)]
    
    # Now consolidate all into group level (sex, year, and fam-structure specific) variables
    data[ , fam_structure := marr_cohab:num_earners]
    data[ , fam_structure := as.factor(as.character(fam_structure))] # get rid of empty category
    
    setkey(data, sex, decade, fam_structure)
    
    qoi <- data[ , .(
        sigma_x = sigma_x[1],
        sigma_y = sigma_y[1],
        mu_x = mu_x[1],
        mu_y = mu_y[1],
        mu_other = mu_other[1],
        mu_pn = mu_pn[1],
        pi_g = sum(wtsupp) / total_pop[1],
        r_g = wtd.cors(labern, fam_inc, wtsupp)[1],
        sigma_gx = sqrt(wtd.var(labern, wtsupp)),
        sigma_gy = sqrt(wtd.var(fam_inc, wtsupp)),
        mu_gx = wtd.mean(labern, wtsupp),
        mu_gy = wtd.mean(fam_inc, wtsupp),
        mu_g_pn = wtd.mean(pn_labern, wtsupp),
        mu_g_oth = wtd.mean(other_inc, wtsupp),
        var_xg = wtd.var(labern, wtsupp),
        sigma_gpartner = sqrt(wtd.var(pn_labern, wtsupp)),
        sigma_gother = sqrt(wtd.var(other_inc, wtsupp)),
        cor_xg_partnerg = wtd.cors(labern, pn_labern, wtsupp)[1],
        cor_xg_otherg = wtd.cors(labern, other_inc, wtsupp)[1]
        
    ), by=key(data)]
    
    qoi[ , cov_xg_partnerg := cor_xg_partnerg*sigma_gx*sigma_gpartner]
    qoi[ , cov_xg_otherg := cor_xg_otherg*sigma_gx*sigma_gother]
    qoi[ , cov_xg_yg := r_g*sigma_gx*sigma_gy]
    qoi[ , c("cor_xg_partnerg", "cor_xg_otherg") := NULL]
    qoi
}
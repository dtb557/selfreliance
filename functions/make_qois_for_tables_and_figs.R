make_qois_for_tables_and_figs <- function(data, 
                                          fam_adj = TRUE, 
                                          exclude_alloc = FALSE, 
                                          exclude_top_2_pct = TRUE, 
                                          exclude_top_decile_female_earners = FALSE, 
                                          exclude_top_decile_male_earners = FALSE) {
    
    
    source("functions/fixed_wtd.rank.R")
    source("functions/limit_age_range.R")
    
    qoi_labels <- c("self_reliance",
                    "womens_faminc_pctile_by_decile",
                    "mens_faminc_pctile_by_decile",
                    "earnings_rank",
                    "faminc_rank",
                    "wives_share",
                    "female_male_earn_ratio",
                    "earnings",
                    "partner_earnings",
                    "other_income",
                    "share_partnered",
                    "share_dual_earner_couples",
                    "self_reliance_partnered",
                    "self_reliance_dual_earner",
                    "self_reliance",
                    "decomp")
    
    out <- vector(mode = "list", length = length(qoi_labels))
    
    names(out) <- qoi_labels
    
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
    
    # The numbers I need for preliminary tables and figures (all numbers separate
    # for men and women and by decade unless otherwise specified):
    
    # Self-reliance (correlation between own earnings and family income)
    out$self_reliance <- data[ , .(self_reliance=wtd.cors(labern, fam_inc, weight=wtsupp)[1]), by=sex]
    
    # Average family income percentile by earnings decile
    womens_deciles <- data[sex=="Female" & labern > 0, wtd.quantile(labern, weights=wtsupp, probs=seq(0, 1, .1))]
    mens_deciles <- data[sex=="Male" & labern > 0, wtd.quantile(labern, weights=wtsupp, probs=seq(0, 1, .1))]
    
    data[ , faminc_pctile := fixed_wtd.rank(fam_inc, weights=wtsupp, normwt=TRUE), by=sex]
    data[ , max_faminc_pctile := max(faminc_pctile), by=sex]
    data[ , faminc_pctile := 100*faminc_pctile/max_faminc_pctile]
    
    womens_avg_faminc_pctile_by_decile <- numeric(11)
    mens_avg_faminc_pctile_by_decile <- numeric(11)
    
    womens_avg_faminc_pctile_by_decile[1] <- data[sex=="Female" & labern==0, wtd.mean(faminc_pctile, weights=wtsupp)]
    mens_avg_faminc_pctile_by_decile[1] <- data[sex=="Male" & labern==0, wtd.mean(faminc_pctile, weights=wtsupp)]
    
    for(i in 1:10) {
        womens_avg_faminc_pctile_by_decile[i+1] <- data[sex=="Female" & labern <= womens_deciles[i+1] & labern > womens_deciles[i], wtd.mean(faminc_pctile, weights=wtsupp)]
    }
    
    for(i in 1:10) {
        mens_avg_faminc_pctile_by_decile[i+1] <- data[sex=="Male" & labern <= mens_deciles[i+1] & labern > mens_deciles[i], wtd.mean(faminc_pctile, weights=wtsupp)]
    }
    
    out$womens_faminc_pctile_by_decile <- womens_avg_faminc_pctile_by_decile
    out$mens_faminc_pctile_by_decile <- mens_avg_faminc_pctile_by_decile
    
    #         # Rank in gender-specific earnings distribution
    #         out$earnings_rank <- data[ , .(earnings_rank=wtd.rank(labern, weights=wtsupp, normwt=TRUE)), by=sex]
    #         
    #         # Rank in gender-specific family income distribution
    #         out$faminc_rank <- data[ , .(faminc_rank=wtd.rank(fam_inc, weights=wtsupp, normwt=TRUE)), by=sex]
    
    # Share of wives' earnings as part of couples' earnings (men and women combined)
    out$wives_share <- data[marr_cohab=="Married or cohabiting" & sex=="Female" & (labern+pn_labern) > 0, 
                           wtd.mean(labern/(labern+pn_labern), weights=wtsupp)]
    
    
    # Female-male ratio in earnings (partnered and single, but prob limit to ftfyr;
    # men and women combined)
    out$female_male_earn_ratio <- data[full_time_full_year==TRUE & sex=="Female", 
                                      wtd.quantile(labern, weights=wtsupp, probs=.5)] / 
        data[full_time_full_year==TRUE & sex=="Male", 
            wtd.quantile(labern, weights=wtsupp, probs=.5)]
    
    # Own earnings, mean and sd, for:
    # Singles
    # Partnered
    out$earnings <- data[ , .(mean_earnings=wtd.mean(labern, weights=wtsupp), 
                             sd_earnings=sqrt(wtd.var(labern, weights=wtsupp, normwt=TRUE))), 
                         by=.(sex, marr_cohab)]
    
    
    # Partner earnings, mean and sd, for partnered people
    out$partner_earnings <- data[marr_cohab=="Married or cohabiting", 
                                .(mean_pn_earnings=wtd.mean(pn_labern, weights=wtsupp), 
                                  sd_pn_earnings=sqrt(wtd.var(pn_labern, weights=wtsupp, normwt=TRUE))), 
                                by=sex]
    
    
    # Other income, mean and sd
    out$other_income <- data[ , .(mean_oth_inc=wtd.mean(other_inc, weights=wtsupp), 
                                 sd_oth_inc=sqrt(wtd.var(other_inc, weights=wtsupp, normwt=TRUE))), 
                             by=sex]
    
    
    # Share partnered
    out$share_partnered <- data[ , .(share_partnered=wtd.mean(marr_cohab=="Married or cohabiting", 
                                                             weights=wtsupp)), by=sex]
    
    # Share dual earner
    out$share_dual_earner_couples <- data[marr_cohab=="Married or cohabiting", 
                                         .(share_dual_earner=wtd.mean(num_earners=="Two earners", weights=wtsupp)), by=sex]
    
    
    # Self-reliance for married persons only
    out$self_reliance_partnered <- data[marr_cohab=="Married or cohabiting", .(sr=wtd.cors(labern, fam_inc, weight=wtsupp)[1]), by=sex]
    
    # Self-reliance for married persons only
    out$self_reliance_dual_earner <- data[marr_cohab=="Married or cohabiting" & num_earners=="Two earners", .(sr=wtd.cors(labern, fam_inc, weight=wtsupp)[1]), by=sex]
    
    # Contributions to overall (gender-specific) correlations, by "group" variable:
    # Contribution of group-specific correlation
    # Contribution of association between own and partner earnings
    # Contribution of association between own earnings and other income
    # Contribution of labern polarity relative to family income polarity
    data[ , labern_grand_mean := wtd.mean(labern, weights=wtsupp), by=sex]
    data[ , fam_inc_grand_mean := wtd.mean(fam_inc, weights=wtsupp), by=sex]
    data[is.na(pn_labern), pn_labern := 0L]
    data[ , pn_labern_grand_mean := wtd.mean(pn_labern, weights=wtsupp), by=sex]
    data[ , oth_inc_grand_mean := wtd.mean(other_inc, weights=wtsupp), by=sex]
    
    data[ , SD_labern := sqrt(wtd.var(labern, weights=wtsupp)), by=sex]
    data[ , SD_fam_inc := sqrt(wtd.var(fam_inc, weights=wtsupp)), by=sex]
    
    data[ , labern_grand_dev := labern - labern_grand_mean]
    data[ , fam_inc_grand_dev := fam_inc - fam_inc_grand_mean]
    data[ , pn_labern_grand_dev := pn_labern - pn_labern_grand_mean]
    data[ , oth_inc_grand_dev := other_inc - oth_inc_grand_mean]
    
    # Key:
    # EP = expected product
    # RMSD = root mean squared deviations
    # SD = standard deviation
    
    data[ , fam_structure := marr_cohab:num_earners]
    data[ , fam_structure := as.factor(as.character(fam_structure))] # get rid of empty category
    
    decomp <- data[ , .(
        EP_labern_fam_inc = wtd.mean(labern_grand_dev*fam_inc_grand_dev, weights=wtsupp), 
        RMSD_labern = sqrt(wtd.mean(labern_grand_dev^2, weights=wtsupp)),
        RMSD_fam_inc = sqrt(wtd.mean(fam_inc_grand_dev^2, weights=wtsupp)),
        EP_labern_pn_labern = wtd.mean(labern_grand_dev*pn_labern_grand_dev, weights=wtsupp), 
        EP_labern_oth_inc = wtd.mean(labern_grand_dev*oth_inc_grand_dev, weights=wtsupp),
        SD_labern = SD_labern[1], 
        SD_fam_inc = SD_fam_inc[1],
        group_size = sum(wtsupp)
    ), by=.(sex, fam_structure)]
    
    decomp[ , group_polarity := RMSD_labern*RMSD_fam_inc]
    decomp[ , group_corr := EP_labern_fam_inc / group_polarity]
    decomp[ , group_labern_relative_polarity := RMSD_labern / RMSD_fam_inc]
    decomp[ , labern_pn_labern_corr := EP_labern_pn_labern / group_polarity]
    decomp[ , labern_oth_inc_corr := EP_labern_oth_inc / group_polarity]
    decomp[ , group_wt := group_size / sum(group_size), by=sex]
    
    if(sum(abs(out$self_reliance$self_reliance - decomp[ , sum(group_corr*group_wt*(group_polarity/(SD_labern*SD_fam_inc))), by=sex][,V1])) > 1e-6) {
        stop("Product of components does not equal overall correlations by gender.")
    }
    
    #         > decomp[ , sum(group_corr*group_wt*(group_polarity/(SD_labern*SD_fam_inc))), by=sex]
    #         sex        V1
    #         1: Female 0.5979381
    #         2:   Male 0.8090003
    
    if(sum(abs(decomp[ , group_labern_relative_polarity + labern_pn_labern_corr + labern_oth_inc_corr] - decomp$group_corr)) > 1e-6) {
        stop("Sum of components does not equal group correlation for some groups.")
    }
    
    #         > decomp[ , .(group_corr, replicate=group_labern_relative_polarity + labern_pn_labern_corr + labern_oth_inc_corr)]
    #         group_corr  replicate
    #         1: 0.62092898 0.62092898
    #         2: 0.68367022 0.68367022
    #         3: 0.75847246 0.75847246
    #         4: 0.87235358 0.87235358
    #         5: 0.86478385 0.86478385
    #         6: 0.79777738 0.79777738
    #         7: 0.08474493 0.08474493
    #         8: 0.77864326 0.77864326
    #         9: 0.88126071 0.88126071
    #         10: 0.61883868 0.61883868
    
    out$decomp <- decomp
    
    out
}
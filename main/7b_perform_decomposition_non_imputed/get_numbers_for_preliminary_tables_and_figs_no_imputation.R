library(data.table)
library(Hmisc)
library(weights)

load(paste0("Data/non_imputed_post_tax/post_tax_analysis_vars.Rdata"))

YEARS <- seq(1970, 2010, 10)

qoi <- structure(vector(mode="list", length=5), names=YEARS)

out <- list()

d <- d[age %in% 25:54 & (is.na(pn_age) | pn_age %in% 25:54), ]

# Remove top X% to deal with topcoding
top_x <- .02

cut_above <- d[ , .(fam_inc=fam_inc[1], wtsupp=wtsupp[1]), by=.(year, serial)][ , wtd.quantile(fam_inc, weights=wtsupp, probs=1-top_x), by=year]
cut_above <- with(cut_above, structure(V1, names=year))

d <- d[fam_inc < cut_above[as.character(year)], ]

# INCLUDING OR EXCLUDING CASES WITH ALLOCATED OWN OR SPOUSE LABOR EARNINGS?
include_alloc <- TRUE
# include_alloc <- FALSE

if(!include_alloc) {
    d <- d[own_or_pn_labern_alloc==FALSE, ]
}


# ADJUSTING FOR FAMILY / HOUSEHOLD SIZE OR NOT?
fam_adj <- TRUE
# fam_adj <- FALSE

if(fam_adj) {
    d[ , fam_inc := fam_inc / sqrt_famsize]
    d[ , labern := labern / sqrt_famsize]
    d[ , pn_labern := pn_labern / sqrt_famsize]
    d[ , other_inc := other_inc / sqrt_famsize]
    d[ , fam_unearned_non_gov := fam_unearned_non_gov / sqrt_famsize]
    d[ , fam_unearned_gov := fam_unearned_gov / sqrt_famsize]
    d[ , fam_tax := fam_tax / sqrt_famsize]
    d[ , oth_labern := oth_labern / sqrt_famsize]
}

# EXCLUDING TOP DECILE FEMALE EARNERS OR NOT?
exclude_top_decile_female_earners <- TRUE
# exclude_top_decile_female_earners <- FALSE

if(exclude_top_decile_female_earners) {
    source("scripts/exclude_hhs_with_top_decile_female_earner.R")
    d <- exclude_hhs_with_top_decile_female_earner(d)
}

# The numbers I need for preliminary tables and figures (all numbers separate
# for men and women and by decade unless otherwise specified):

for(yr in YEARS) {
    
    dy <- d[year %in% (yr-1):(yr+1), ]
    
    # Self-reliance (correlation between own earnings and family income)
    out$self_reliance <- dy[ , .(self_reliance=wtd.cors(labern, fam_inc, weight=wtsupp)[1]), by=sex]
    
    # Average family income percentile by earnings decile
    womens_deciles <- dy[sex=="Female" & labern > 0, wtd.quantile(labern, weights=wtsupp, probs=seq(0, 1, .1))]
    mens_deciles <- dy[sex=="Male" & labern > 0, wtd.quantile(labern, weights=wtsupp, probs=seq(0, 1, .1))]
    
    dy[ , faminc_pctile := wtd.rank(fam_inc, weights=wtsupp, normwt=TRUE), by=sex]
    dy[ , max_faminc_pctile := max(faminc_pctile), by=sex]
    dy[ , faminc_pctile := 100*faminc_pctile/max_faminc_pctile]
    
    womens_avg_faminc_pctile_by_decile <- numeric(11)
    mens_avg_faminc_pctile_by_decile <- numeric(11)
    
    womens_avg_faminc_pctile_by_decile[1] <- dy[sex=="Female" & labern==0, wtd.mean(faminc_pctile, weights=wtsupp)]
    mens_avg_faminc_pctile_by_decile[1] <- dy[sex=="Male" & labern==0, wtd.mean(faminc_pctile, weights=wtsupp)]
    
    for(i in 1:10) {
        womens_avg_faminc_pctile_by_decile[i+1] <- dy[sex=="Female" & labern <= womens_deciles[i+1] & labern > womens_deciles[i], wtd.mean(faminc_pctile, weights=wtsupp)]
    }
    
    for(i in 1:10) {
        mens_avg_faminc_pctile_by_decile[i+1] <- dy[sex=="Male" & labern <= mens_deciles[i+1] & labern > mens_deciles[i], wtd.mean(faminc_pctile, weights=wtsupp)]
    }
    
    out$womens_faminc_pctile_by_decile <- womens_avg_faminc_pctile_by_decile
    out$mens_faminc_pctile_by_decile <- mens_avg_faminc_pctile_by_decile
    
    #         # Rank in gender-specific earnings distribution
    #         out$earnings_rank <- dy[ , .(earnings_rank=wtd.rank(labern, weights=wtsupp, normwt=TRUE)), by=sex]
    #         
    #         # Rank in gender-specific family income distribution
    #         out$faminc_rank <- dy[ , .(faminc_rank=wtd.rank(fam_inc, weights=wtsupp, normwt=TRUE)), by=sex]
    
    # Share of wives' earnings as part of couples' earnings (men and women combined)
    out$wives_share <- dy[marr_cohab=="Married or cohabiting" & sex=="Female" & (labern+pn_labern) > 0, 
                           wtd.mean(labern/(labern+pn_labern), weights=wtsupp)]
    
    
    # Female-male ratio in earnings (partnered and single, but prob limit to ftfyr;
    # men and women combined)
    out$female_male_earn_ratio <- dy[full_time_full_year==TRUE & sex=="Female", 
                                      wtd.quantile(labern, weights=wtsupp, probs=.5)] / 
        dy[full_time_full_year==TRUE & sex=="Male", 
            wtd.quantile(labern, weights=wtsupp, probs=.5)]
    
    # Own earnings, mean and sd, for:
        # Singles
        # Partnered
    
    if(fam_adj) {
        out$earnings <- dy[ , .(mean_earnings=wtd.mean(labern*sqrt_famsize, weights=wtsupp), 
                                sd_earnings=sqrt(wtd.var(labern*sqrt_famsize, weights=wtsupp, normwt=TRUE))), 
                            by=.(sex, marr_cohab)]
        
        
        # Partner earnings, mean and sd, for partnered people
        out$partner_earnings <- dy[marr_cohab=="Married or cohabiting", 
                                   .(mean_pn_earnings=wtd.mean(pn_labern*sqrt_famsize, weights=wtsupp), 
                                     sd_pn_earnings=sqrt(wtd.var(pn_labern*sqrt_famsize, weights=wtsupp, normwt=TRUE))), 
                                   by=sex]
        
        
        # Other income, mean and sd
        out$other_income <- dy[ , .(mean_oth_inc=wtd.mean(other_inc*sqrt_famsize, weights=wtsupp), 
                                    sd_oth_inc=sqrt(wtd.var(other_inc*sqrt_famsize, weights=wtsupp, normwt=TRUE))), 
                                by=sex]
    } else {
        out$earnings <- dy[ , .(mean_earnings=wtd.mean(labern, weights=wtsupp), 
                                sd_earnings=sqrt(wtd.var(labern, weights=wtsupp, normwt=TRUE))), 
                            by=.(sex, marr_cohab)]
        
        
        # Partner earnings, mean and sd, for partnered people
        out$partner_earnings <- dy[marr_cohab=="Married or cohabiting", 
                                   .(mean_pn_earnings=wtd.mean(pn_labern, weights=wtsupp), 
                                     sd_pn_earnings=sqrt(wtd.var(pn_labern, weights=wtsupp, normwt=TRUE))), 
                                   by=sex]
        
        
        # Other income, mean and sd
        out$other_income <- dy[ , .(mean_oth_inc=wtd.mean(other_inc, weights=wtsupp), 
                                    sd_oth_inc=sqrt(wtd.var(other_inc, weights=wtsupp, normwt=TRUE))), 
                                by=sex]
    }
    
    
    # Share partnered
    out$share_partnered <- dy[ , .(share_partnered=wtd.mean(marr_cohab=="Married or cohabiting", 
                                           weights=wtsupp)), by=sex]
    
    # Share dual earner
    out$share_dual_earner_couples <- dy[marr_cohab=="Married or cohabiting", 
                                         .(share_dual_earner=wtd.mean(num_earners=="Two earners", weights=wtsupp)), by=sex]
    
    
    # Self-reliance for married persons only
    out$self_reliance_partnered <- dy[marr_cohab=="Married or cohabiting", .(sr=wtd.cors(labern, fam_inc, weight=wtsupp)[1]), by=sex]
    
    # Self-reliance for married persons only
    out$self_reliance_dual_earner <- dy[marr_cohab=="Married or cohabiting" & num_earners=="Two earners", .(sr=wtd.cors(labern, fam_inc, weight=wtsupp)[1]), by=sex]
    
    # Contributions to overall (gender-specific) correlations, by "group" variable:
        # Contribution of group-specific correlation
            # Contribution of association between own and partner earnings
            # Contribution of association between own earnings and other income
            # Contribution of labern polarity relative to family income polarity
    dy[ , labern_grand_mean := wtd.mean(labern, weights=wtsupp), by=sex]
    dy[ , fam_inc_grand_mean := wtd.mean(fam_inc, weights=wtsupp), by=sex]
    # dy[is.na(pn_labern), pn_labern := 0L]
    dy[ , pn_labern_grand_mean := wtd.mean(pn_labern, weights=wtsupp, na.rm=TRUE), by=sex]
    dy[ , oth_inc_grand_mean := wtd.mean(other_inc, weights=wtsupp), by=sex]
    
    dy[ , SD_labern := sqrt(wtd.var(labern, weights=wtsupp)), by=sex]
    dy[ , SD_fam_inc := sqrt(wtd.var(fam_inc, weights=wtsupp)), by=sex]
    
    dy[ , labern_grand_dev := labern - labern_grand_mean]
    dy[ , fam_inc_grand_dev := fam_inc - fam_inc_grand_mean]
    dy[ , pn_labern_grand_dev := pn_labern - pn_labern_grand_mean]
    dy[ , oth_inc_grand_dev := other_inc - oth_inc_grand_mean]
    
    # Key:
    # EP = expected product
    # RMSD = root mean squared deviations
    # SD = standard deviation
    
    dy[ , fam_structure := marr_cohab:num_earners]
    dy[ , fam_structure := as.factor(as.character(fam_structure))] # get rid of empty category
    
    # create new fam_structure categories
    dy[ , fam_structure := as.character(fam_structure)]
    dy[sex=="Female" & fam_structure=="Married or cohabiting:One earner" & labern > 0, 
       fam_structure := "Married or cohabiting:One earner, female breadwinner"]
    dy[sex=="Female" & fam_structure=="Married or cohabiting:One earner" & labern==0, 
       fam_structure := "Married or cohabiting:One earner, male breadwinner"]
    dy[sex=="Male" & fam_structure=="Married or cohabiting:One earner" & labern > 0, 
       fam_structure := "Married or cohabiting:One earner, male breadwinner"]
    dy[sex=="Male" & fam_structure=="Married or cohabiting:One earner" & labern==0, 
       fam_structure := "Married or cohabiting:One earner, female breadwinner"]
    dy[ , fam_structure := as.factor(fam_structure)]
    
    decomp <- dy[ , .(
        EP_labern_fam_inc = wtd.mean(labern_grand_dev*fam_inc_grand_dev, weights=wtsupp), 
        RMSD_labern = sqrt(wtd.mean(labern_grand_dev^2, weights=wtsupp)),
        RMSD_fam_inc = sqrt(wtd.mean(fam_inc_grand_dev^2, weights=wtsupp)),
        EP_labern_pn_labern = wtd.mean(labern_grand_dev*pn_labern_grand_dev, weights=wtsupp), 
        EP_labern_oth_inc = wtd.mean(labern_grand_dev*oth_inc_grand_dev, weights=wtsupp),
        SD_labern = SD_labern[1], 
        SD_fam_inc = SD_fam_inc[1],
        group_size = sum(wtsupp)
    ), by=.(sex, fam_structure)]
    
    if(include_alloc & fam_adj) {
        save(dy, file=paste0("output/decomp_components_w_alloc_fam_adj_", yr, ".Rdata"))
    }
    
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
    qoi[[as.character(yr)]] <- out
}

if(include_alloc) {
    if(fam_adj) {
        save(qoi, file="output/qoi_no_imputation_w_alloc_fam_adj_exclude_top_decile_female_earners.Rdata")
    } else {
        save(qoi, file="output/qoi_no_imputation_w_alloc.Rdata")
    }
} else {
    if(fam_adj) {
        save(qoi, file="output/qoi_no_imputation_no_alloc_fam_adj.Rdata")
    } else {
        save(qoi, file="output/qoi_no_imputation_no_alloc.Rdata")
    }
}
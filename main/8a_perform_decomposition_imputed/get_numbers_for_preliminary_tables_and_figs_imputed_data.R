library(data.table)
library(Hmisc)
library(weights)

if(commandArgs()[1] == "RStudio") {
    # ADJUSTING FOR FAMILY / HOUSEHOLD SIZE OR NOT?
    # fam_adj <- TRUE
    fam_adj <- FALSE
    
    # Exclude cases with missing own or partner labor earnings?
    # exclude_alloc <- TRUE
    exclude_alloc <- FALSE
    
    # Exclude households in top 2% of family income?
    exclude_top_2 <- TRUE
    # exclude_top_2 <- FALSE
} else {
    rgs <- commandArgs(trailingOnly = TRUE)
    fam_adj <- "fam_adj" %in% rgs
    exclude_alloc <- "exclude_alloc" %in% rgs
    exclude_top_2 <- "exclude_top_2" %in% rgs
    exclude_top_decile_female_earners <- "exclude_top_decile_female_earners" %in% rgs
}

# Suffixes for filenames
fa <- ifelse(fam_adj, "_fam_adj", "_no_fam_adj")
ea <- ifelse(exclude_alloc, "_exclude_alloc", "")
et2 <- ifelse(exclude_top_2, "_exclude_top_2_pct", "")
etme <- ifelse(exclude_top_decile_female_earners, "_exclude_top_decile_female_earners", "")

fixed_wtd.rank <- function (x, weights = NULL, normwt = FALSE, na.rm = TRUE) 
{
    if (!length(weights)) 
        return(rank(x, na.last = if (na.rm) NA else TRUE))
    tab <- wtd.table(x, weights, normwt = normwt, na.rm = na.rm)
    freqs <- tab$sum.of.weights
    r <- cumsum(freqs) - 0.5 * (freqs - 1)
    approx(tab$x, r, xout = x, rule=2)$y # The fix is to add rule=2
}

if(exclude_alloc) {
    # Make flag to exclude cases with missing 
    # own labor earnings or partner labor earnings
    load("Data/cleaned_data/cleaned_data_step_5.Rdata")
    d[ , labern_alloc := is.na(incwage) | is.na(incbus) | is.na(incfarm)]
    pn_labern_alloc <- d[ , .(pn_labern_alloc=labern_alloc),
                          keyby=.(year, serial, pernum=pnloc)]
    setkey(d, year, serial, pernum)
    d <- pn_labern_alloc[d]
    rm(pn_labern_alloc)
    to_exclude <- d[ , .(alloc=labern_alloc |
                             (marr_cohab=="Married or cohabiting" & pn_labern_alloc)),
                     keyby=.(year, serial, pernum)]
    rm(d)
}

if(exclude_top_2) {
    load("output/top_2_flag.Rdata")
    setkey(top_2_flag, year, serial, pernum)
}

for(yr in seq(1970, 2010, 10)) {
    # if(!(yr %in% c(1970, 1980))) next
    cat(yr, "")
    load(paste0("Data/imputed_datasets_post_tax/imps_", yr, "_analysis_vars.Rdata"))
    qoi <- lapply(imps, function(imp) { # "quantities of interest"
        
        out <- list()
        
        imp <- imp[age %in% 25:54 & (is.na(pn_age) | pn_age %in% 25:54), ]
        
        if(fam_adj) {
            # cat("Adjusting for family size...")
            imp[ , fam_inc := fam_inc / sqrt_famsize]
            imp[ , labern := labern / sqrt_famsize]
            imp[ , pn_labern := pn_labern / sqrt_famsize]
            imp[ , other_inc := other_inc / sqrt_famsize]
            imp[ , fam_unearned_non_gov := fam_unearned_non_gov / sqrt_famsize]
            imp[ , fam_unearned_gov := fam_unearned_gov / sqrt_famsize]
            imp[ , fam_tax := fam_tax / sqrt_famsize]
            imp[ , oth_labern := oth_labern / sqrt_famsize]
        }
        
        if(exclude_alloc) {
            # Remove cases with allocated own or partner labor earnings
            setkey(imp, year, serial, pernum)
            imp <- to_exclude[imp]
            imp <- imp[alloc==FALSE, ]
        }
        
        if(exclude_top_2) {
            setkey(imp, year, serial, pernum)
            imp <- top_2_flag[imp]
            imp <- imp[top_2_pct==FALSE, ]
        }
        
        if(exclude_top_decile_female_earners) {
            source("scripts/exclude_hhs_with_top_decile_female_earner.R")
            imp <- exclude_hhs_with_top_decile_female_earner(imp)
        }
        
        # The numbers I need for preliminary tables and figures (all numbers separate
        # for men and women and by decade unless otherwise specified):
        
        # Self-reliance (correlation between own earnings and family income)
        out$self_reliance <- imp[ , .(self_reliance=wtd.cors(labern, fam_inc, weight=wtsupp)[1]), by=sex]
        
        # Average family income percentile by earnings decile
        womens_deciles <- imp[sex=="Female" & labern > 0, wtd.quantile(labern, weights=wtsupp, probs=seq(0, 1, .1))]
        mens_deciles <- imp[sex=="Male" & labern > 0, wtd.quantile(labern, weights=wtsupp, probs=seq(0, 1, .1))]
        
        imp[ , faminc_pctile := fixed_wtd.rank(fam_inc, weights=wtsupp, normwt=TRUE), by=sex]
        imp[ , max_faminc_pctile := max(faminc_pctile), by=sex]
        imp[ , faminc_pctile := 100*faminc_pctile/max_faminc_pctile]
        
        womens_avg_faminc_pctile_by_decile <- numeric(11)
        mens_avg_faminc_pctile_by_decile <- numeric(11)
        
        womens_avg_faminc_pctile_by_decile[1] <- imp[sex=="Female" & labern==0, wtd.mean(faminc_pctile, weights=wtsupp)]
        mens_avg_faminc_pctile_by_decile[1] <- imp[sex=="Male" & labern==0, wtd.mean(faminc_pctile, weights=wtsupp)]
        
        for(i in 1:10) {
            womens_avg_faminc_pctile_by_decile[i+1] <- imp[sex=="Female" & labern <= womens_deciles[i+1] & labern > womens_deciles[i], wtd.mean(faminc_pctile, weights=wtsupp)]
        }
        
        for(i in 1:10) {
            mens_avg_faminc_pctile_by_decile[i+1] <- imp[sex=="Male" & labern <= mens_deciles[i+1] & labern > mens_deciles[i], wtd.mean(faminc_pctile, weights=wtsupp)]
        }
        
        out$womens_faminc_pctile_by_decile <- womens_avg_faminc_pctile_by_decile
        out$mens_faminc_pctile_by_decile <- mens_avg_faminc_pctile_by_decile
        
#         # Rank in gender-specific earnings distribution
#         out$earnings_rank <- imp[ , .(earnings_rank=wtd.rank(labern, weights=wtsupp, normwt=TRUE)), by=sex]
#         
#         # Rank in gender-specific family income distribution
#         out$faminc_rank <- imp[ , .(faminc_rank=wtd.rank(fam_inc, weights=wtsupp, normwt=TRUE)), by=sex]
        
        # Share of wives' earnings as part of couples' earnings (men and women combined)
        out$wives_share <- imp[marr_cohab=="Married or cohabiting" & sex=="Female" & (labern+pn_labern) > 0, 
                               wtd.mean(labern/(labern+pn_labern), weights=wtsupp)]
        
        
        # Female-male ratio in earnings (partnered and single, but prob limit to ftfyr;
        # men and women combined)
        out$female_male_earn_ratio <- imp[full_time_full_year==TRUE & sex=="Female", 
                                          wtd.quantile(labern, weights=wtsupp, probs=.5)] / 
            imp[full_time_full_year==TRUE & sex=="Male", 
                wtd.quantile(labern, weights=wtsupp, probs=.5)]
        
        # Own earnings, mean and sd, for:
            # Singles
            # Partnered
        out$earnings <- imp[ , .(mean_earnings=wtd.mean(labern, weights=wtsupp), 
                                 sd_earnings=sqrt(wtd.var(labern, weights=wtsupp, normwt=TRUE))), 
                                 by=.(sex, marr_cohab)]

        
        # Partner earnings, mean and sd, for partnered people
        out$partner_earnings <- imp[marr_cohab=="Married or cohabiting", 
                                    .(mean_pn_earnings=wtd.mean(pn_labern, weights=wtsupp), 
                                         sd_pn_earnings=sqrt(wtd.var(pn_labern, weights=wtsupp, normwt=TRUE))), 
                                     by=sex]
        
        
        # Other income, mean and sd
        out$other_income <- imp[ , .(mean_oth_inc=wtd.mean(other_inc, weights=wtsupp), 
                                     sd_oth_inc=sqrt(wtd.var(other_inc, weights=wtsupp, normwt=TRUE))), 
                                 by=sex]
        
        
        # Share partnered
        out$share_partnered <- imp[ , .(share_partnered=wtd.mean(marr_cohab=="Married or cohabiting", 
                                               weights=wtsupp)), by=sex]
        
        # Share dual earner
        out$share_dual_earner_couples <- imp[marr_cohab=="Married or cohabiting", 
                                             .(share_dual_earner=wtd.mean(num_earners=="Two earners", weights=wtsupp)), by=sex]

        
        # Self-reliance for married persons only
        out$self_reliance_partnered <- imp[marr_cohab=="Married or cohabiting", .(sr=wtd.cors(labern, fam_inc, weight=wtsupp)[1]), by=sex]
        
        # Self-reliance for married persons only
        out$self_reliance_dual_earner <- imp[marr_cohab=="Married or cohabiting" & num_earners=="Two earners", .(sr=wtd.cors(labern, fam_inc, weight=wtsupp)[1]), by=sex]
        
        # Contributions to overall (gender-specific) correlations, by "group" variable:
            # Contribution of group-specific correlation
                # Contribution of association between own and partner earnings
                # Contribution of association between own earnings and other income
                # Contribution of labern polarity relative to family income polarity
        imp[ , labern_grand_mean := wtd.mean(labern, weights=wtsupp), by=sex]
        imp[ , fam_inc_grand_mean := wtd.mean(fam_inc, weights=wtsupp), by=sex]
        imp[is.na(pn_labern), pn_labern := 0L]
        imp[ , pn_labern_grand_mean := wtd.mean(pn_labern, weights=wtsupp), by=sex]
        imp[ , oth_inc_grand_mean := wtd.mean(other_inc, weights=wtsupp), by=sex]
        
        imp[ , SD_labern := sqrt(wtd.var(labern, weights=wtsupp)), by=sex]
        imp[ , SD_fam_inc := sqrt(wtd.var(fam_inc, weights=wtsupp)), by=sex]
        
        imp[ , labern_grand_dev := labern - labern_grand_mean]
        imp[ , fam_inc_grand_dev := fam_inc - fam_inc_grand_mean]
        imp[ , pn_labern_grand_dev := pn_labern - pn_labern_grand_mean]
        imp[ , oth_inc_grand_dev := other_inc - oth_inc_grand_mean]
        
        # Key:
        # EP = expected product
        # RMSD = root mean squared deviations
        # SD = standard deviation
        
        imp[ , fam_structure := marr_cohab:num_earners]
        imp[ , fam_structure := as.factor(as.character(fam_structure))] # get rid of empty category
        
        decomp <- imp[ , .(
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
        
        return(out)
    })
    
    filename <- sprintf("output/qoi_new_imp%s%s%s%s%s.Rdata", yr, fa, ea, et2, etme)
    save(qoi, file=filename)
}
# Get quantities of interest for Deirdre's memo with 
# two new decompositions

# Quantities needed

# 1. Group share of population
# 2. Within-group correlation between labern and faminc
# 3. Within-group standard deviation of labern
# 4. Overall standard deviation of labern
# 5. Within-group standard deviation of faminc
# 6. Overall standard deviation of faminc
# 7. Group mean of labern
# 8. Grand mean of labern
# 9. Group mean of faminc
# 10. Grand mean of faminc
# 11. Within-group covariance of labern and faminc
# 12. Within-group variance of labern
# 13. Within-group covariance of labern and pn_labern
# 14. Within-group covariance of labern and other_inc

library(data.table)
library(Hmisc)
library(weights)

out <- vector(mode="list", length=10)

# Get command line arguments
if(commandArgs()[1] == "RStudio") {
    # Set options manually
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
fa <- ifelse(fam_adj, " fam adj", " no fam adj")
ea <- ifelse(exclude_alloc, " exclude alloc", "")
et2 <- ifelse(exclude_top_2, " exclude top 2 pct", "")
etme <- ifelse(exclude_top_decile_female_earners, " exclude top decile female earners", "")

if(exclude_alloc) {
    # Make flag to exclude cases with missing 
    # own labor earnings or partner labor earnings
    load("main/1_clean_data/cleaned_data_step_5.Rdata")
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
    cat(yr, "")
    imp_file <- sprintf("Data/imputed_datasets_post_tax/imps_%d_analysis_vars.Rdata", yr)
    load(imp_file)
    for(i in 1:10) {
        cat(i, "")
        d <- imps[[i]]
        
        d <- d[age %in% 25:54 & (is.na(pn_age) | pn_age %in% 25:54), ]
        
        # # Remove top X% to deal with topcoding
        # top_x <- .02
        # 
        # cut_above <- d[ , .(fam_inc=fam_inc[1], wtsupp=wtsupp[1]), by=.(year, serial)][ , wtd.quantile(fam_inc, weights=wtsupp, probs=1-top_x), by=year]
        # cut_above <- with(cut_above, structure(V1, names=year))
        # 
        # d <- d[fam_inc < cut_above[as.character(year)], ]
        
        
        d[wtsupp < 0, wtsupp := 0]
        
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
        
        if(exclude_alloc) {
            # Remove cases with allocated own or partner labor earnings
            setkey(d, year, serial, pernum)
            d <- to_exclude[d]
            d <- d[alloc==FALSE, ]
        }
        
        if(exclude_top_2) {
            setkey(d, year, serial, pernum)
            d <- top_2_flag[d]
            d <- d[top_2_pct==FALSE, ]
        }
        
        if(exclude_top_decile_female_earners) {
            source("scripts/exclude_hhs_with_top_decile_female_earner.R")
            d <- exclude_hhs_with_top_decile_female_earner(d)
        }
        
        d[ , decade := signif(year, 3)]
        
        # First, create population-level (sex and year specific) variables
        setkey(d, sex, decade)
        d[ , sigma_x := sqrt(wtd.var(labern, wtsupp)), by=key(d)]
        d[ , sigma_y := sqrt(wtd.var(fam_inc, wtsupp)), by=key(d)]
        d[ , mu_x := wtd.mean(labern, wtsupp), by=key(d)]
        d[ , mu_y := wtd.mean(fam_inc, wtsupp), by=key(d)]
        d[ , mu_other := wtd.mean(other_inc, wtsupp), by=key(d)]
        d[ , mu_pn := wtd.mean(pn_labern, wtsupp), by=key(d)]
        d[ , total_pop := sum(wtsupp), by=key(d)]
        
        # Now consolidate all into group level (sex, year, and fam-structure specific) variables
        d[ , fam_structure := marr_cohab:num_earners]
        d[ , fam_structure := as.factor(as.character(fam_structure))] # get rid of empty category
        
        setkey(d, sex, decade, fam_structure)
        
        qoi <- d[ , .(
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
            
        ), by=key(d)]
        
        rm(d)
        
        qoi[ , cov_xg_partnerg := cor_xg_partnerg*sigma_gx*sigma_gpartner]
        qoi[ , cov_xg_otherg := cor_xg_otherg*sigma_gx*sigma_gother]
        qoi[ , cov_xg_yg := r_g*sigma_gx*sigma_gy]
        qoi[ , c("cor_xg_partnerg", "cor_xg_otherg") := NULL]
        
        out[[i]] <- rbindlist(list(out[[i]], qoi))
    }
    cat("\n")
}

cat("Saving individual component files...\n")

for(i in seq_along(out)) {
    outfile <- sprintf("output/New decomposition components new imputation%s%s%s%s %d.csv", fa, ea, et2, etme, i)
    write.csv(out[[i]], file=outfile, row.names=FALSE)
}

all <- rbindlist(out)

non_key_vars <- setdiff(names(all), c("sex", "decade", "fam_structure"))

cat("Averaging across imputations...\n")

avg <- eval(
    parse(
        text=sprintf("all[ , .(%s), by=.(sex, decade, fam_structure)]", 
                     paste0(non_key_vars, "=mean(", non_key_vars, ", na.rm=TRUE)", 
                            collapse=", "))
    )
)

outfile <- sprintf("output/New decomposition components new imputation%s%s%s%s avg.csv", fa, ea, et2, etme)

write.csv(avg, file=outfile, row.names=FALSE)


make_figures_1_and_2_non_imputed <- function(IN_DIR, OUT_DIR,
                                             fam_adj, 
                                             exclude_alloc, 
                                             exclude_top_2_pct, 
                                             exclude_top_decile_female_earners, 
                                             exclude_top_decile_male_earners) {
    
    require(data.table)
    
    source("functions/build_filename_suffix.R")
    
    suffix <- build_filename_suffix(fam_adj, 
                                    exclude_alloc, 
                                    exclude_top_2_pct, 
                                    exclude_top_decile_female_earners, 
                                    exclude_top_decile_male_earners)
    
    
    
    years <- seq(1970, 2010, 10)
    qoi <- lapply(years, function(yr) {
        qoi_file <- sprintf("qoi_non_imp_%d_%s.Rdata",
                            yr, 
                            suffix)
        
        load(file.path(IN_DIR, qoi_file))
        qoi
    })

    # Figure 3: self-reliance by decade for men and women

    self_reliance_by_decade <- rbindlist(lapply(qoi, `[[`, "self_reliance"))
    years <- years
    self_reliance_by_decade[ , year := rep(years, each=2)]
    
    out_file <- file.path(OUT_DIR, 
                       sprintf("figure_3_%s.csv", suffix)
    )
    write.csv(self_reliance_by_decade, file=out_file, row.names=FALSE)
    
    
    # Figure 1: family income percentile by earnings decile
    
    decile_labels <- c("No earn", as.character(1:10))
    
    faminc_pctile_data <- data.table(earnings_decile=decile_labels, 
                                     men_faminc_pctile=qoi[[1]]$mens_faminc_pctile_by_decile, 
                                     women_faminc_pctile=qoi[[1]]$womens_faminc_pctile_by_decile)
    
    out_file <- file.path(OUT_DIR, 
                       sprintf("figure_1_%s.csv", suffix)
    )
    write.csv(faminc_pctile_data, file=out_file, row.names=FALSE)
    
    
    # Figure 2: Single-panel figure with multiple lines, % of 1970 value, by decade, with lines for: 
    # (a) female-male ratio in self-reliance
    # (b) share of total spousal earnings in married couples contributed by wives' earnings
    # (c) female-male ratio in earnings (perhaps limited to full-time, year-round workers)
    
    setorder(self_reliance_by_decade, sex, year)
    
    female_male_sr_ratio <- self_reliance_by_decade[ , .(self_reliance_ratio=self_reliance[sex=="Female"]/self_reliance[sex=="Male"]), by=year]
    
    female_male_sr_ratio[ , sr_ratio_pct_of_1970 := 100*self_reliance_ratio/self_reliance_ratio[year==1970]]
    
    wives_share <- data.table(year=years, wives_share=unlist(lapply(qoi, function(x) x$wives_share)))
    
    wives_share[ , wives_share_pct_of_1970 := 100*wives_share/wives_share[year==1970]]
    
    fm_earn_ratio <- data.table(year=years, female_male_earn_ratio=sapply(qoi, function(x) x$female_male_earn_ratio))
    
    fm_earn_ratio[ , earn_ratio_pct_of_1970 := 100*female_male_earn_ratio/female_male_earn_ratio[year==1970]]
    
    setkey(female_male_sr_ratio, year)
    setkey(wives_share, year)
    setkey(fm_earn_ratio, year)
    
    out <- female_male_sr_ratio[wives_share[fm_earn_ratio]]
    
    out_file <- file.path(OUT_DIR, 
                          sprintf("figure_2_%s.csv", suffix)
    )
    write.csv(out, file = out_file, row.names=FALSE)

}


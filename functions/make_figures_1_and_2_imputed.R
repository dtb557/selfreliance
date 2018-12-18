make_figures_1_and_2_imputed <- function(fam_adj = TRUE, 
                                         exclude_alloc = FALSE, 
                                         exclude_top_2_pct = TRUE, 
                                         exclude_top_decile_female_earners = FALSE, 
                                         exclude_top_decile_male_earners = FALSE, 
                                         no_cohab = FALSE) {
    
    require(data.table)
    
    source("functions/build_filename_suffix.R")
    
    if (no_cohab) {
        qoi_dir <- "main/8c_perform_decomposition_no_cohab/2_qois_for_tables_and_figs"
        out_dir <- "main/8c_perform_decomposition_no_cohab/3_figures_1_and_2"
    } else {
        qoi_dir <- "main/8a_perform_decomposition_imputed/2_qois_for_tables_and_figs"
        out_dir <- "main/8a_perform_decomposition_imputed/3_figures_1_and_2"
    }
    
    suffix <- build_filename_suffix(fam_adj, 
                                    exclude_alloc, 
                                    exclude_top_2_pct, 
                                    exclude_top_decile_female_earners, 
                                    exclude_top_decile_male_earners)
    
    years <- seq(1970, 2010, 10)
    qoi <- lapply(years, function(yr) {
        qoi_file <- sprintf("qoi_imp_%d_%s.Rdata", 
                            yr, 
                            suffix)
        
        load(file.path(qoi_dir, qoi_file))
        qoi
    })
    
    # Figure 1: Four-panel figure; 
    # top-left graph (Panel A) self-reliance by decade, men; 
    # top-right graph (Panel B) self-reliance by decade, women; 
    # bottom-left graph (Panel C) rank in gender-specific family income dist. by rank in gender-specific earnings dist, men 1970; 
    # bottom-right graph (Panel D) rank in gender-specific family income dist. by rank in gender-specific earnings dist, women 1970
    
    # Figure 1, Panels 1 and 2
    
    self_reliance_by_decade <- lapply(qoi, 
                                      function(x) {
                                          rbindlist(lapply(x, function(y) y$self_reliance))
                                      }
    )
    self_reliance_by_decade <- lapply(self_reliance_by_decade, 
                                      function(x) {
                                          x[ , .(self_reliance=mean(self_reliance)), by=sex]
                                      }
    )
    
    self_reliance_by_decade <- rbindlist(self_reliance_by_decade)
    years <- years
    self_reliance_by_decade[ , year := rep(years, each=2)]
    
    out_file <- file.path(out_dir, 
                          sprintf("figure_1_panels_1_and_2_%s.csv", suffix)
    )
    write.csv(self_reliance_by_decade, file=out_file, row.names=FALSE)
    
    
    # Figure 1, Panels 3 and 4
    
    decile_labels <- c("No earn", as.character(1:10))
    
    mens_faminc_pctile_by_decile <- Reduce(`+`, lapply(qoi[[1]], function(x) x$mens_faminc_pctile_by_decile))/10
    womens_faminc_pctile_by_decile <- Reduce(`+`, lapply(qoi[[1]], function(x) x$womens_faminc_pctile_by_decile))/10
    
    faminc_pctile_data <- data.table(earnings_decile=decile_labels, 
                                     men_faminc_pctile=mens_faminc_pctile_by_decile, 
                                     women_faminc_pctile=womens_faminc_pctile_by_decile)
    
    out_file <- file.path(out_dir, 
                          sprintf("figure_1_panels_3_and_4_%s.csv", suffix)
    )
    write.csv(faminc_pctile_data, file=out_file, row.names=FALSE)
    
    
    # Figure 2: Single-panel figure with multiple lines, % of 1970 value, by decade, with lines for: 
    # (a) female-male ratio in self-reliance
    # (b) share of total spousal earnings in married couples contributed by wives' earnings
    # (c) female-male ratio in earnings (perhaps limited to full-time, year-round workers)
    
    setorder(self_reliance_by_decade, sex, year)
    
    female_male_sr_ratio <- self_reliance_by_decade[ , .(self_reliance_ratio=self_reliance[sex=="Female"]/self_reliance[sex=="Male"]), by=year]
    
    female_male_sr_ratio[ , sr_ratio_pct_of_1970 := 100*self_reliance_ratio/self_reliance_ratio[year==1970]]
    
    wives_share <- data.table(year=years, wives_share=unlist(lapply(qoi, function(x) {
        mean(sapply(x, function(y) y$wives_share))
    })))
    
    wives_share[ , wives_share_pct_of_1970 := 100*wives_share/wives_share[year==1970]]
    
    fm_earn_ratio <- data.table(year=years, female_male_earn_ratio=unlist(lapply(qoi, function(x) {
        mean(sapply(x, function(y) y$female_male_earn_ratio))
    })))
    
    fm_earn_ratio[ , earn_ratio_pct_of_1970 := 100*female_male_earn_ratio/female_male_earn_ratio[year==1970]]
    
    setkey(female_male_sr_ratio, year)
    setkey(wives_share, year)
    setkey(fm_earn_ratio, year)
    
    out <- female_male_sr_ratio[wives_share[fm_earn_ratio]]
    
    out_file <- file.path(out_dir, 
                          sprintf("figure_2_%s.csv", suffix)
    )
    write.csv(out, file = out_file, row.names=FALSE)
    
}


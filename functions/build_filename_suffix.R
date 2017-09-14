build_filename_suffix <- function(fam_adj = TRUE, 
                                  exclude_alloc = FALSE, 
                                  exclude_top_2_pct = TRUE,
                                  exclude_top_decile_female_earners = FALSE, 
                                  exclude_top_decile_male_earners = FALSE) {
    require(stringr)
    suffix <- ""
    if(fam_adj) suffix <- str_c(suffix, "fa_")
    if(exclude_alloc) suffix <- str_c(suffix, "ea_")
    if(exclude_top_2_pct) suffix <- str_c(suffix, "e2_")
    if(exclude_top_decile_female_earners) suffix <- str_c(suffix, "ef_")
    if(exclude_top_decile_male_earners) suffix <- str_c(suffix, "em_")
    str_replace(suffix, "_$", "")
}
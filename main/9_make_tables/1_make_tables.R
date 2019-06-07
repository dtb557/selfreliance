library(data.table)
library(Hmisc)
library(weights)

source("functions/make_tables.R")
source("functions/build_filename_suffix.R")

fam_adj <- TRUE
exclude_alloc <- FALSE
exclude_top_2_pct <- FALSE
exclude_top_decile_female_earners <- FALSE
exclude_top_decile_male_earners <- FALSE

suffix <- build_filename_suffix(fam_adj, exclude_alloc, 
                                exclude_top_2_pct, 
                                exclude_top_decile_female_earners, 
                                exclude_top_decile_male_earners)

IN_DIR <- "main/8a_perform_decomposition_imputed/1_decomp_component_tables"
OUT_DIR <- "main/9_make_tables"
OUT_FILE <- file.path(OUT_DIR, "1_tabs_list.Rdata")

dat.decomp.imp.list <- lapply(
    1:10,
    function(i) {
        read.csv(
            file.path(
                IN_DIR, 
                sprintf("decomp_components_imputed_%s_%d.csv", suffix, i)
            ),
            stringsAsFactors = FALSE
        )
    }
)

dat.decomp.list <- lapply(dat.decomp.imp.list, fun.data.prep)

out.decomp.list <- lapply(dat.decomp.list, fun.sr.decomp.5piece.apply)

tabs.list <- do.call(fun.tabs.list, c(out.decomp.list, dat.decomp.list))

save(tabs.list, file = OUT_FILE)
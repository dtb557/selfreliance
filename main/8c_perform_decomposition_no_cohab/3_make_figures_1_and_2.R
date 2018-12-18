source("functions/make_figures_1_and_2_imputed.R")

make_figures_1_and_2_imputed(no_cohab = TRUE) # defaults to fam_adj and exclude_top_2_pct
make_figures_1_and_2_imputed(exclude_top_2_pct = FALSE, no_cohab = TRUE)
make_figures_1_and_2_imputed(fam_adj = FALSE, no_cohab = TRUE)

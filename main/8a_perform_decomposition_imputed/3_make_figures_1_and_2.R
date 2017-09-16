source("functions/make_figures_1_and_2_imputed.R")

make_figures_1_and_2_imputed() # defaults to fam_adj and exclude_top_2_pct
make_figures_1_and_2_imputed(exclude_top_2_pct = FALSE)
make_figures_1_and_2_imputed(fam_adj = FALSE)

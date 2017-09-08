library(data.table)
library(Hmisc)
library(weights)

load(paste0("Data/non_imputed_post_tax/post_tax_analysis_vars.Rdata"))

# Limit ages
d <- d[age %in% 25:54 & (is.na(pn_age) | pn_age %in% 25:54), ]

# Remove top X% to deal with topcoding
top_x <- .02

cut_above <- d[ , .(fam_inc=fam_inc[1], wtsupp=wtsupp[1]), by=.(year, serial)][ , wtd.quantile(fam_inc, weights=wtsupp, probs=1-top_x), by=year]
cut_above <- with(cut_above, structure(V1, names=year))

d[ , top_2_pct := fam_inc > cut_above[as.character(year)]]

top_2_flag <- d[ , .(top_2_pct, year, serial, pernum)]

save(top_2_flag, file="output/top_2_flag.Rdata")


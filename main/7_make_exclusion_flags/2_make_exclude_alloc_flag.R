# Make flag to exclude cases with allocated own or partner earnings

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

save(to_exclude, file="main/7_make_exclusion_flags/2_exclude_alloc_flag.Rdata")
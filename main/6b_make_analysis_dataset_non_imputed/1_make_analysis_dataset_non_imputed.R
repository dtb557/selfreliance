#############################################################################
# Create analysis variables and save datasets with only necessary variables #
#############################################################################
library(data.table)
library(ipumsr)

source("functions/make_analysis_dataset.R")

dss <- function(text, subs) {
    split = strsplit(text, "%s")[[1]]
    if(grepl("%s$", text)) split <- c(split, "")
    if (length(split) - 1 != length(subs)) {
        stop("Number of wildcard characters does not equal number of variables to substitute.")
    }
    return(paste0(split, c(subs, ""), collapse=""))
}

do_string <- function(x) {
    eval(parse(text=x), envir=parent.frame())
}

load("main/4b_estimate_taxes_and_transfers_non_imputed/6_non_imp_data_post_tax.Rdata")

# Add region
ddi <- read_ipums_ddi("original_data/add_region.xml")
region <- read_ipums_micro(ddi, data_file = "original_data/add_region.dat.gz")
region <- data.table(region)
setnames(region, names(region), c("year", "serial", "region", "pernum"))
setkey(region, year, serial, pernum)

setkey(d, year, serial, pernum)
d <- region[d]
rm(region)

for(n in "filestat") {
    do_string(dss("d[ , %s := NULL]", n))
}

d <- make_analysis_dataset(d)
    

save(d, file="main/6b_make_analysis_dataset_non_imputed/1_non_imputed_analysis_vars.Rdata")

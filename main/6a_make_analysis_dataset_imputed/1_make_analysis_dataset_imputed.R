#############################################################################
# Create analysis variables and save datasets with only necessary variables #
#############################################################################
library(data.table)
library(mice)
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

load("main/1_clean_data/cleaned_data_step_5.Rdata")
d <- d[ , .(pce, pnloc, subfamid, momloc, 
            poploc, wtsupp, relate, nchild), keyby=.(year, serial, pernum)]

# Add region
ddi <- read_ipums_ddi("original_data/add_region.xml")
region <- read_ipums_micro(ddi, data_file = "original_data/add_region.dat.gz")
region <- data.table(region)
setnames(region, names(region), c("year", "serial", "region", "pernum"))
setkey(region, year, serial, pernum)

d <- region[d]
rm(region)

for(yr in seq(1970, 2010, 10)) {
    # if(yr==1970) next
    cat(yr, "")
    load(sprintf("main/4a_estimate_taxes_and_transfers_imputed/6_imp_post_tax_%d.Rdata", yr))
    imps <- lapply(1:10, function(i) {
        # for(n in c("schlcoll", "srcearn", "filestat")) {
        #     do_string(dss("imp[ , %s := NULL]", n))
        # }
        
        tmp <- data.table(complete(imp, i))
        
        make_analysis_dataset(tmp, imputed=TRUE, data_to_merge=d)
    })
    save(imps, file=file.path("main/6a_make_analysis_dataset_imputed", 
                              sprintf("1_imps_%d_analysis_vars.Rdata", yr)))
    rm(imp, imps)
}

#############################################################################
# Create analysis variables and save datasets with only necessary variables #
#############################################################################
library(data.table)
library(mice)
library(purrr)
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
d <- d[ , .(pce, 
            # sploc, 
            # pnloc,
            # subfamid, 
            momloc, 
            poploc, wtsupp, relate, nchild, 
            cohabp1, cohabp2), keyby=.(year, serial, pernum)]

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
    load(sprintf("main/5_make_no_cohab_datasets/1_imp_%d_post_tax_no_cohab.Rdata", yr))
    imps <- map(1:10, function(i) {
        # for(n in c("schlcoll", "srcearn", "filestat")) {
        #     do_string(dss("imp[ , %s := NULL]", n))
        # }
        
        tmp <- data.table(complete(imp, i))
        
        make_analysis_dataset(tmp, imputed=TRUE, data_to_merge=d, no_cohab=TRUE)
    })
    save(imps, file=file.path("main/6c_make_analysis_dataset_no_cohab", 
                              sprintf("1_imps_%d_analysis_vars.Rdata", yr)))
    rm(imp, imps)
}

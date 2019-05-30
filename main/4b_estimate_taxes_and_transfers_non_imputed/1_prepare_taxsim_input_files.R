##############################
# Prepare TAXSIM input files #
##############################
library(data.table)

taxsim_input_dir <- "main/4b_estimate_taxes_and_transfers_non_imputed/1_taxsim_input"

if (!dir.exists(taxsim_input_dir)) dir.create(taxsim_input_dir)

dss <- function(text, subs) {
    split = strsplit(text, "%s")[[1]]
    if(grepl("%s$", text)) split <- c(split, "")
    if (length(split) - 1 != length(subs)) {
        return("Error: Number of wildcard characters does not equal number of variables to substitute.")
    }
    return(paste0(split, c(subs, ""), collapse=""))
}

do_string <- function(x) {
    eval(parse(text=x), envir=parent.frame())
}

source("functions/make_taxsim_dataset.R")

load("main/1_clean_data/cleaned_data_step_5_no_imputation.Rdata")

d[year >= 1989, incwage := oincwage + inclongj*(srcearn=="Wage and salary")]
d[year >= 1989, incbus := oincbus + inclongj*(srcearn=="Self employment")]
d[year >= 1989, incfarm := oincfarm + inclongj*(srcearn=="Farm self employment")]

d[ , incdisab := ifelse(year >=1989, incdisa1 + incdisa2, as.integer(NA))]
d[ , incretir := ifelse(year >= 1989, increti1 + increti2, as.integer(NA))]
d[ , incsurv := ifelse(year >=1989, incsurv1 + incsurv2, as.integer(NA))]

d[year %in% 1979:1981 & !is.na(incwelfr_alloc), incwelfr := incwelfr_alloc]

d[ , labern := apply(d[ , .(incwage, incbus, incfarm)], 1, sum, na.rm=TRUE)]

d[ , posern := factor(ifelse(labern > 0, "Has positive earnings", "Has zero or negative earnings"))]

d[ , inctot := as.integer(sum(incwage, incbus, incfarm, incss, incwelfr, incgov, 
                                 incidr, incaloth, incretir, incssi, incdrt, incint,
                                 incunemp, incwkcom, incvet, incdivid, incrent, 
                                 inceduc, incchild, incalim, incasist, incother, 
                                 incdisab, incsurv, na.rm=TRUE)), 
      by=.(year, serial, pernum)]
out <- make_taxsim_dataset(copy(d))
out <- rbind(c(9, 85, 2, rep(0, 19)), as.matrix(out))
write.table(
    out, 
    file="main/4b_estimate_taxes_and_transfers_non_imputed/1_taxsim_input/sr1970_2010", 
    row.names=FALSE, 
    col.names=FALSE
)

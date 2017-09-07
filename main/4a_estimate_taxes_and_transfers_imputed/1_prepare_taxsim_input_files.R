##############################
# Prepare TAXSIM input files #
##############################
library(data.table)
library(mice)

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

source("taxsim/make_taxsim_dataset.R")

# Load master dataset to fill in missing age values
# I fixed this problem in the latest imputation
# load("Data/ipums_data_full/cps_master.Rdata")
# age <- d[ , .(year, serial, pernum, age)]
# setkey(age, year, serial, pernum)
# 
# age[age=="Under 1 year", age := "0"]
# age[age=="99+", age := "99"]
# age[age=="90 (90+, 1988-2002)", age := "90"]
# age[year >=2009 & age==80, age := "82"]
# age[ , age := as.integer(as.character(age))]

load("Data/cleaned_data/cleaned_data_step_5.Rdata")

vars_to_add <- d[ , .(year, serial, pernum, momloc, poploc, sploc, nchild, relate, 
            pce)] #, schlcoll, srcearn)]

setkey(vars_to_add, year, serial, pernum)

for(yr in c(1970, 1980, 1990, 2000, 2010)) {
    # if(!(yr %in% c(1990, 2000))) next
    cat(paste0(yr, " "))
    imp_file <- sprintf("Data/imp_iterations/with_ppc/imp_%d_10_extreme_values_transposed.Rdata", yr)
    if(!file.exists(imp_file)) next
    load(imp_file)
    for(i in 1:10) {
        cat(paste0(i, " "))
        dimp <- data.table(complete(imp, i))
        
        dimp[ , age_group_by_sex := sex:age_group]
        
        cases_to_add <- d[is.na(inctot) & year %in% (yr-1):(yr+1), intersect(names(dimp), names(d)), with=FALSE]
        
        dimp <- rbindlist(list(dimp, cases_to_add), fill=TRUE)
        rm(cases_to_add)
        
        for(n in c("filestat")) { # ", schlcoll", "srcearn")) { 
            do_string(dss("dimp[ , %s := NULL]", n))
        }
        
        setkey(dimp, year, serial, pernum)
        
        dimp <- vars_to_add[dimp]
        # dimp <- age[dimp]
        
        dimp[year >= 1989, incwage := oincwage + inclongj*(srcearn=="Wage and salary")]
        dimp[year >= 1989, incbus := oincbus + inclongj*(srcearn=="Self employment")]
        dimp[year >= 1989, incfarm := oincfarm + inclongj*(srcearn=="Farm self employment")]
        
        dimp[ , incdisab := ifelse(year >=1989, incdisa1 + incdisa2, as.integer(NA))]
        dimp[ , incretir := ifelse(year >= 1989, increti1 + increti2, as.integer(NA))]
        dimp[ , incsurv := ifelse(year >=1989, incsurv1 + incsurv2, as.integer(NA))]
        
        dimp[year %in% 1979:1981 & !is.na(incwelfr_alloc), incwelfr := incwelfr_alloc]
        
        dimp[ , labern := apply(dimp[ , .(incwage, incbus, incfarm)], 1, sum, na.rm=TRUE)]
        
        dimp[ , posern := factor(ifelse(labern > 0, "Has positive earnings", "Has zero or negative earnings"))]
        
        dimp[ , inctot := as.integer(sum(incwage, incbus, incfarm, incss, incwelfr, incgov, 
                                         incidr, incaloth, incretir, incssi, incdrt, incint,
                                         incunemp, incwkcom, incvet, incdivid, incrent, 
                                         inceduc, incchild, incalim, incasist, incother, 
                                         incdisab, incsurv, na.rm=TRUE)), 
              by=.(year, serial, pernum)]
        out <- make_taxsim_dataset(copy(dimp))
        out <- rbind(c(9, 85, 2, rep(0, 19)), as.matrix(out))
        write.table(out, file=paste0("taxsim/taxsim_input/sr", yr, "_", i), row.names=FALSE, 
                    col.names=FALSE)
    }
    rm(list=c("dimp", paste0("dimp_", yr)))
    cat("\n")
}
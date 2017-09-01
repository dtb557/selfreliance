STEP_1_DATA = "Data/cleaned_data/cleaned_data_step_1.Rdata"

if(!file.exists(STEP_1_DATA)) stop("Cannot perform step 2 of data cleaning without data from step 1.")

library(data.table)

source("functions/dss.R")
source("functions/do_string.R")

load(STEP_1_DATA)

cat(paste0(format(Sys.time(), format="%H:%M:%S"), "    Preparing to recode topcoded and allocated cases as NA...\n"))
# Here are the values indicating no imputation (allocation) (all other values indicate some kind of imputation, of income value or source):
no_alloc_values <- c("NIU/No allocation", "No change / not allocated", "No change or children or armed forces", "No change or children", "No allocation", "Not allocated", "No change")

# Combine qschcol1-3 and remove them
d[ , qschlco := "Not allocated"]
d[!(qschcol1=="No change" & qschcol2=="No change" & qschcol3=="No change"), qschlco := "Allocated"]

d[ , qschcol1 := NULL]
d[ , qschcol2 := NULL]
d[ , qschcol3 := NULL]

# Remove variables that are the sum of two components: incdisab, incsurv, incretir
# But first, assign zero values to components when the sum is zero
# Wait, incretir was its own variable (no components) before 1989, with its own
# topcoding, so treat it differently and don't remove
# Also, change income from second source variables from NA to zeros, because
# these NAs are actually NIU values for these variables
d[incdisab==0, incdisa1 := 0]
d[incdisab==0, incdisa2 := 0]
d[is.na(incdisa2), incdisa2 := 0]
d[incsurv==0, incsurv1 := 0]
d[incsurv==0, incsurv2 := 0]
d[is.na(incsurv2), incsurv2 := 0]
d[incretir==0 & year >= 1989, increti1 := 0]
d[incretir==0 & year >= 1989, increti2 := 0]
d[is.na(increti2), increti2 := 0]
d[incretir > 0 & year >= 1989 & is.na(increti1), increti1 := incretir] # Fix two cases with positive incretir and NA increti1

d[ , incdisab := NULL]
d[ , incsurv := NULL]

# Try removing qmarst and qrelate (so that these variables don't 
# get NA values filled in for allocated cases)
d[ , qmarst := NULL]
d[ , qrelate := NULL]

# Make data quality variable key
q_flags <- names(d)[grep("^q", names(d))]
get_var_name_from_flag_name <- function(x) {
    pattern <- paste0("^", ifelse(grepl("\\d$", x), substr(x, 2, 7), substr(x, 2, nchar(x))))
    names(d)[grep(pattern, names(d))]
}
names(q_flags) <- sapply(q_flags, get_var_name_from_flag_name)

rm(get_var_name_from_flag_name)

# Fix data quality flag names that don't match the pattern
change_name <- function(flag, new_name) names(q_flags)[which(q_flags==flag)] <<- new_name
change_name("qwkswork", "wkswork1")
q_flags["wkswork2"] <- "qwkswork"
change_name("qincassi", "incasist")
change_name("qincdis1", "incdisa1")
change_name("qincdis2", "incdisa2")
change_name("qincret1", "increti1")
change_name("qincret2", "increti2")
change_name("qincreti", "incretir")
change_name("qincss", "incss")
change_name("qincsur1", "incsurv1")
change_name("qincsur2", "incsurv2")
q_flags["incalim"] <- "qincalot"

rm(change_name)


# Actually, don't do this boxed step yet, the problem isn't as bad as I thought
###############################################################################
# Don't convert allocated values to NA for variables having fewer than 1,000
# observed, non-zero cases in a decade, AND more than four times as many 
# cases to impute as observed, non-zero cases
###############################################################################

# Variables to include (not counting all the decade interactions)
# imp_vars <- c("year", "serial", "pernum",
#               "proptax", "stampval", "spmcaphous", "ffngcare", "ffngcaid", 
#               "sex", "race", "marst", "popstat", "hispan", "educ", 
#               "schlcoll", "classwly", "wkswork1", "wkswork2", "hrswork", "uhrswork", 
#               "ftotval", "inctot", "incwage", "incbus", "incfarm", "incss", "incwelfr", 
#               "incgov", "incidr", "incaloth", "incretir", "incssi", "incdrt", "incint", 
#               "incunemp", "incwkcom", "incvet", "incdivid", "incrent", "inceduc", 
#               "incchild", "incalim", "incasist", "incother", "incdisa1", "incdisa2", 
#               "inclongj", "increti1", "increti2", "incsurv1", "incsurv2", "oincbus", 
#               "oincfarm", "oincwage", "srcearn", "adjginc", "capgain", "caploss", "eitcred",
#               "fedtax", "fedtaxac", "filestat", "statetax", "stataxac", "taxinc")
# 
# 
# # Remove everyone for which is.na(inctot)==TRUE (these are all kids under age 15, there aren't any values we really need to fill in for them)
# dimp <- d[!is.na(inctot), ]
# 
# # Create subset with just those variables
# dimp <- dimp[ , c(imp_vars, unique(q_flags)), with=FALSE]
# 
# qois_by_decade <- list(list(), 
#                        list(), 
#                        list(),
#                        list(), 
#                        list())
# 
# decades <- as.character(seq(1970, 2010, 10))
# names(qois_by_decade) <- decades
# 
# dimp[] <- lapply(dimp, function(x) {
#     if(is.factor(x)) return(as.character(x))
#     else return(x)
# })
# 
# for(v in names(q_flags)) {
#     do_string(dss("dimp[!(%s %in% no_alloc_values) & !is.na(%s), %s := NA]", c(q_flags[v], q_flags[v], v)))
# }
# 
# for(v in names(q_flags)) {
#     if(!is.numeric(dimp[[v]])) next
#     for(yr in decades) {
#         var <- do_string(sprintf("dimp[year %%in%% (%s-1):(%s+1), %s]", yr, yr, v))
#         if(all(is.na(var)) | all(!is.na(var))) next
#         else qois_by_decade[[yr]][[v]] <- c(n_non_zeros = sum(var!=0, na.rm=TRUE), 
#                                             pct_zeros = 100*mean(var==0, na.rm=TRUE), 
#                                             n_to_impute = sum(is.na(var)))
#     }
# }
# 
# excluded_years <- vector(mode="list", length=length(q_flags))
# names(excluded_years) <- names(q_flags)
# 
# for(dcd in names(qois_by_decade)) {
#     cat(dcd, "\n")
#     for(v in names(qois_by_decade[[dcd]])) {
#         q <- qois_by_decade[[dcd]][[v]]
#         if(q["n_non_zeros"] < 1000 & q["n_to_impute"] > (4*q["n_non_zeros"])) {
#             cat(sprintf("%s has only %d observed, non-zero values, and %d cases to impute", 
#                         v, q["n_non_zeros"], q["n_to_impute"]), "\n")
#             excluded_years[[v]] <- c(excluded_years[[v]], (as.integer(dcd)-1):(as.integer(dcd)+1))
#         }
#     }
#     cat("\n\n")
# }
# 
# rm(dimp)

# Load topcodes and transform into matrix with rownames as varnames and colnames as years
topcodes <- data.table(read.csv("Data/topcode_values.csv", stringsAsFactors=FALSE))
varnames <- topcodes$var
topcodes[ , var := NULL]
topcodes <- as.matrix(topcodes)
rownames(topcodes) <- tolower(varnames)
colnames(topcodes) <- gsub("^X", "", colnames(topcodes))
rm(varnames)

# Make dummies to flag topcoded cases
cat(paste0(format(Sys.time(), format="%H:%M:%S"), "    Making topcode flag variables...\n"))
for(v in rownames(topcodes)) {
    tc_var <- paste0("tc_", v)
    do_string(dss("d[ , %s := 0L]", tc_var))
    for(y in colnames(topcodes)) {
        if(is.na(topcodes[v, y])) next
        do_string(dss("d[year==%s & %s >= %s, %s := 1L]", c(y, v, topcodes[v, y], tc_var)))
    }
}

rm(topcodes, tc_var)

cat(paste0(format(Sys.time(), format="%H:%M:%S"), "    Converting all factor variables to character...\n"))
# Change all factor variables to character to ease recoding as NA
d[] <- lapply(d, function(x) {
    if(is.factor(x)) return(as.character(x))
    else return(x)
})

cat(paste0(format(Sys.time(), format="%H:%M:%S"), "    Recoding allocated and topcoded cases as NA...\n"))
# Change allocated cases to NA
for(v in names(q_flags)) {
    # excl_yrs <- excluded_years[v]
    do_string(dss("d[!(%s %in% no_alloc_values) & !is.na(%s), %s := NA]", c(q_flags[v], q_flags[v], v)))
}

rm(no_alloc_values, q_flags)

# Change topcoded cases to NA
for(tcv in names(d)[grep("^tc_", names(d))]) {
    v <- sub("^tc_", "", tcv)
    do_string(dss("d[%s==1, %s := NA]", c(tcv, v)))
}

save(d, file="Data/cleaned_data/cleaned_data_step_2.Rdata")
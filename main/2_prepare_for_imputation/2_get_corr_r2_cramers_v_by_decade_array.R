AI <- Sys.getenv("PBS_ARRAY_INDEX")
NAI <- as.numeric(AI)

A_YEAR <- seq(1970, 2010, 10)[NAI]

setwd("../")

# Get full corr matrix and Cramer's V matrix
library(data.table)

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

load("main/1_clean_data/cleaned_data_step_5.Rdata") # returns data as d

# Variables to include (not counting all the decade interactions)
imp_vars <- c("year", "serial", "pernum",
			  "proptax", "stampval", "spmcaphous", "ffngcare", "ffngcaid", 
			  "sex", "race", "marst", "popstat", "hispan", "educ", 
			  "schlcoll", "classwly", "wkswork1", "wkswork2", "hrswork", "uhrswork", 
			  "ftotval", "inctot", "incwage", "incbus", "incfarm", "incss", "incwelfr", 
			  "incgov", "incidr", "incaloth", "incretir", "incssi", "incdrt", "incint", 
			  "incunemp", "incwkcom", "incvet", "incdivid", "incrent", "inceduc", 
			  "incchild", "incalim", "incasist", "incother", "incdisa1", "incdisa2", 
			  "inclongj", "increti1", "increti2", "incsurv1", "incsurv2", "oincbus", 
			  "oincfarm", "oincwage", "srcearn", "adjginc", "capgain", "caploss", "eitcred",
			  "fedtax", "fedtaxac", "filestat", "statetax", "stataxac", "taxinc", "group",
			  "labern", "pn_labern", "subfaminc", "age_group", "age_group_by_sex", 
			  "sqrt_famsize", "marr_cohab", "incwelfr_alloc", "age")



# Create subset with just those variables from a single decade
d <- d[year %in% (A_YEAR-1):(A_YEAR+1), imp_vars, with=FALSE]
rm(imp_vars)

# Make pn_labern equal zero for non-married/cohabiting individuals
d[marr_cohab=="Not married or cohabiting", pn_labern := 0L]

# Create other_inc
d[ , other_inc := subfaminc-labern-pn_labern]


# Remove cases with missing inctot (all kids under 15)
d <- d[!is.na(inctot), ]

# Consolidate srcearn and classwly
d[ , srcearn := as.character(srcearn)]
d[srcearn == "Without pay", srcearn := "niu"]
d[ , srcearn := as.factor(srcearn)]

d[ , classwly := as.character(classwly)]
d[!grepl("^Self-employed|^niu$", classwly), classwly := "Not self-employed"]
d[grepl("^Self-employed", classwly), classwly := "Self-employed"]
d[ , classwly := as.factor(classwly)]

# Load info on which variables should be transformed
tform <- as.matrix(read.csv("output/variable_distributions/variable_transformations.csv", row.names=1))
needs_transform <- tform[ , NAI]
needs_transform["other_inc"] <- "y"

get_adj <- function(x) {
	if(is.numeric(x)) {
		if(min(x, na.rm=T) <= 0) {
			return(-min(x, na.rm=T) + 1)
		} else return(0)
	} else return(0)
}

adjustments <- sapply(d, get_adj)


# Types of variables:

analysis_vars <- c(
			"age_group", 
			"sex", 
			"age_group_by_sex", 
			"race", 
			"marst", 
			"hispan", 
			"educ", 
			"sqrt_famsize", 
			"marr_cohab", 
			"group", 
			"incwage", 
			"incbus", 
			"incfarm", 
			"inclongj", 
			"oincbus", 
			"oincfarm", 
			"oincwage", 
			"labern", 
			"pn_labern", 
			"incssi", 
			"increti1", 
			"increti2", 
			"incss", 
			"ffngcare", 
			"incsurv1", 
			"incsurv2", 
			"incretir", 
			"incgov", 
			"incunemp", 
			"incwkcom", 
			"incvet", 
			"inceduc", 
			"incasist", 
			"incidr", 
			"incaloth", 
			"incdrt", 
			"incint", 
			"incdivid", 
			"incrent", 
			"incchild", 
			"incalim", 
			"incother", 
			"other_inc", 
			"inctot", 
			"ftotval", 
			"subfaminc", 
			"incdisa1", 
			"incdisa2", 
			"incwelfr", 
			"stampval", 
			"spmcaphous", 
			"ffngcaid", 
			"incwelfr_alloc", 
			"proptax", 
			"adjginc", 
			"capgain", 
			"caploss", 
			"eitcred",
			"fedtax", 
			"fedtaxac", 
			"statetax", 
			"stataxac", 
			"taxinc", 
			"wkswork1", 
			"wkswork2", 
			"hrswork", 
			"uhrswork", 
			"schlcoll", 
			"filestat", 
			"popstat", 
			"classwly", 
			"srcearn", 
            "age")

non_analysis_vars <- c(
			"year", 
			"serial", 
			"pernum")

quant_analysis_vars <- names(d)[intersect(which(sapply(d, is.numeric)), which(names(d) %in% analysis_vars))]

factor_analysis_vars <- names(d)[intersect(which(sapply(d, is.factor)), which(names(d) %in% analysis_vars))]

# Get corr matrix, r-squared matrix, and Cramer's V matrix


# Corr list
if(!file.exists(dss("output/corr_list_%s.Rdata", A_YEAR))) {

	corr_list <- vector(mode="list", length=length(quant_analysis_vars))
	names(corr_list) <- quant_analysis_vars
	for(v in quant_analysis_vars) {
		y <- d[[v]]
		if(needs_transform[v] == "y") {
			y <- log(y + adjustments[v])
		}
		corr_list[[v]] <- cor(y, d[ , setdiff(quant_analysis_vars, v), with=FALSE], use="pairwise")
	}
	save(corr_list, file=dss("output/corr_list_%s.Rdata", A_YEAR))
	rm(corr_list)

}



# R-squared for race, educ, hispan, and sex

if(!file.exists(dss("output/new_r_squared_mtrx_%s.Rdata", A_YEAR))) {

	get_r_squared <- function(yname, xname) {
		try(return(do_string(dss("summary(lm(%s ~ %s, data=d))$r.squared", c(yname, xname)))))
		return(0)
	}
	get_r_squared <- Vectorize(get_r_squared)

	r_squared_mtrx <- t(outer(quant_analysis_vars, factor_analysis_vars, 
								  FUN=get_r_squared))
	dimnames(r_squared_mtrx) <- list(factor_analysis_vars, quant_analysis_vars)
	save(r_squared_mtrx, file=dss("output/new_r_squared_mtrx_%s.Rdata", A_YEAR))
	rm(r_squared_mtrx, get_r_squared)

}


# Cramer's V
if(!file.exists(dss("output/cramers_v_%s.Rdata", A_YEAR))) {

	cramers_v <- function(x, y) {
		lx <- length(levels(x))
		ly <- length(levels(y))
		if( lx < 2 | ly < 2 | all(is.na(x)) | all(is.na(y)) ) return(NA)
		smaller_dim <- min(lx, ly)
		suppressWarnings( cv <- sqrt(chisq.test(x, y, correct=FALSE)$statistic / 
										 (length(x) * (smaller_dim-1))) )
		names(cv) <- NULL
		return(cv)
	}
	
	cramers_v_mtrx <- matrix(NA, nrow=length(factor_analysis_vars), ncol=length(factor_analysis_vars), 
							 dimnames=list(factor_analysis_vars, 
										   factor_analysis_vars))
	
	for(j in 2:length(factor_analysis_vars)) {
		for(k in 1:(j-1)) {
			cramers_v_mtrx[j, k] <- cramers_v(d[ , factor_analysis_vars, with=FALSE][[j]], d[ , factor_analysis_vars, with=FALSE][[k]])
		}
	}
	
	save(cramers_v_mtrx, file=dss("output/cramers_v_%s.Rdata", A_YEAR))
	rm(cramers_v_mtrx, cramers_v)

}


# Get matrix of R2 for predicting missingness

if(!file.exists(dss("output/NA_r_squared_mtrx_%s.Rdata", A_YEAR))) {

	r <- !is.na(d)

	get_NA_r_squared <- function(yname, xname) {
		if(yname==xname) return(NA)
		if(is.numeric(d[[xname]])) try(return(cor(r[ , yname], d[[xname]], use="pair")^2))
		try(return(summary(lm(r[ , yname] ~ d[[xname]]))$r.squared))
		return(0)
	}
	get_NA_r_squared <- Vectorize(get_NA_r_squared)

	NA_r_squared_mtrx <- outer(analysis_vars, analysis_vars, 
							   FUN=get_NA_r_squared)
	dimnames(NA_r_squared_mtrx) <- list(analysis_vars, analysis_vars)
	save(NA_r_squared_mtrx, file=dss("output/NA_r_squared_mtrx_%s.Rdata", A_YEAR))
	rm(NA_r_squared_mtrx, get_NA_r_squared, r)

}


# Get matrix of puc

if(!file.exists(dss("output/puc_%s.Rdata", A_YEAR))) {

	library(mice)
	p <- md.pairs(d)
	puc <- p$mr/(p$mr + p$mm)
	save(puc, file=dss("output/puc_%s.Rdata", A_YEAR))
	rm(p, puc)

}

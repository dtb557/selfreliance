# Select predictors based on correlations (variable and missingness), 
# R-squared, Cramer's V, and proportional of usable cases

# To include as predictor for every variable:
# 1. Sex
# 2. Group
# 3. labern
# 4. pn_labern
# 5. subfaminc
# 6. age_group_by_sex
# 7. sqrt_famsize

#setwd("../")

always_include <- c("sex", "group", "labern", "pn_labern", "subfaminc", 
                    "age_group_by_sex", "sqrt_famsize")

library(data.table)
library(nnet)

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

load("main/1_clean_data/cleaned_data_step_5.Rdata")

# Change pn_labern to zero for non-married/cohabiting people
d[marr_cohab=="Not married or cohabiting", pn_labern := 0L]

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


# Remove everyone for which is.na(inctot)==TRUE (these are all kids under age 15, there aren't any values we really need to fill in for them)
d <- d[!is.na(inctot), ]

# Create subset with just those variables plus topcode dummies
d <- d[ , c(imp_vars, names(d)[grep("^tc_", names(d))]), with=FALSE]

# Create other_inc
d[ , other_inc := subfaminc-labern-pn_labern]

# Create group_by_age_group
d[ , group_by_age_group := group:age_group]

# Consolidate srcearn and classwly
d[ , srcearn := as.character(srcearn)]
d[srcearn == "Without pay", srcearn := "niu"]
d[ , srcearn := as.factor(srcearn)]

d[ , classwly := as.character(classwly)]
d[!grepl("^Self-employed|^niu$", classwly) & !is.na(classwly), classwly := "Not self-employed"]
d[grepl("^Self-employed", classwly) & !is.na(classwly), classwly := "Self-employed"]
d[ , classwly := as.factor(classwly)] 

# Load statistics
load_into_list <- function(file, object_name) {
    out <- vector(mode="list", length=5)
    years <- seq(1970, 2010, 10)
    for(i in 1:5) {
        load(gsub(".Rdata", paste0("_", years[i], ".Rdata"),file, fixed=TRUE), envir=sys.frame(1))
        out[[i]] <- get(object_name)
    }
    return(out)
}

stats_dir <- "main/2_prepare_for_imputation"

corr_list_list <- load_into_list(file.path(stats_dir, "corr_list.Rdata"), "corr_list")
cramers_v_list <- load_into_list(file.path(stats_dir, "cramers_v.Rdata"), "cramers_v_mtrx")
r_squared_list <- load_into_list(file.path(stats_dir, "new_r_squared_mtrx.Rdata"), "r_squared_mtrx")
NA_r_squared_list <- load_into_list(file.path(stats_dir, "NA_r_squared_mtrx.Rdata"), "NA_r_squared_mtrx")
puc_list <- load_into_list(file.path(stats_dir, "puc.Rdata"), "puc")

# Symmetrize Cramer's V matrix
cramers_v_list <- lapply(cramers_v_list, function(x) {
    x[is.na(x)] <- 0
    x <- x + t(x)
    return(x)
    })


# Types of variables
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
      "srcearn")

non_analysis_vars <- c(
      "year", 
      "serial", 
      "pernum")

quant_analysis_vars <- names(d)[intersect(which(sapply(d, is.numeric)), which(names(d) %in% analysis_vars))]

factor_analysis_vars <- names(d)[intersect(which(sapply(d, is.factor)), which(names(d) %in% analysis_vars))]



# For each variable, we will use up to 13 more variables, for a total of 20, plus topcode flags.
# There are three separate metrics:
# 1. Strength of relationship between variables among observed cases (corr, r-squared, Cramer's V)
# 2. Strength of relationship between predictor and missingness of target (na r-squared)
# 3. Proportion of missing cases on target for which predictor is observed (puc)



# For quantitative targets, get a vector of correlations and square-rooted r-squareds.
# Exclude the "always_include" variables, then sort in decreasing order
# Take the best 12 variables and switch those on in the target row
# Get a vector of square-rooted r-squareds for predicting missingness, 
# exclude the "always_include" vars, then sort in decreasing order.
# Take the 12 best vars and switch those on in the target row.



# For factor targets, need to find a way to compare Cramer's V to r/r-squared.
# Just multiply Cramer's V by .8, and then throw those in with the correlations.

# Get list of vars with no missing values
no_missing <- names(d)[sapply(d, function(x) !any(is.na(x)))]

all_missing <- lapply(seq(1970, 2010, 10), function(y) names(d)[sapply(d[year %in% (y-1):(y+1), ], function(x) all(is.na(x)))])

pred_matrix_list <- rep(list(matrix(0L, nrow=ncol(d), ncol=ncol(d), dimnames=rep(list(names(d)), 2))), 5)

# Quant vars
for(i in 1:5) {
    for(n in setdiff(quant_analysis_vars, no_missing)) {
        scores <- c(corr_list_list[[i]][[n]][1,], sqrt(r_squared_list[[i]][,n]))
        scores <- scores[!(names(scores) %in% always_include)]
        scores <- scores[!(names(scores) %in% names(puc_list[[i]][n,])[puc_list[[i]][n,] < .25])]
        scores <- abs(scores)
        scores <- scores[scores > .05]
        scores <- scores[!is.na(scores)]
        if(length(scores) > 10) scores <- sort(scores, decreasing=TRUE)[1:10]
	if(!(n %in% rownames(pred_matrix_list[[i]]))) stop(paste0(n , " is not a row in pred_matrix_list."))
	missing_columns <- setdiff(names(scores), colnames(pred_matrix_list[[i]]))
	if(length(missing_columns) > 0) stop(paste0(paste0(missing_columns, collapse=", "), " are not columns in pred_matrix_list."))
        pred_matrix_list[[i]][n, names(scores)] <- 1L
    }
}


# Factor vars
for(i in 1:5) {
    for(n in setdiff(factor_analysis_vars, no_missing)) {
        scores <- c(sqrt(r_squared_list[[i]][n,]), 0.8*cramers_v_list[[i]][n,])
        scores <- scores[!(names(scores) %in% always_include)]
        scores <- scores[!(names(scores) %in% names(puc_list[[i]][n,])[puc_list[[i]][n,] < .25])]
        scores <- scores[scores > .05]
        scores <- scores[!is.na(scores)]
        if(length(scores) > 10) scores <- sort(scores, decreasing=TRUE)[1:10] 
        pred_matrix_list[[i]][n, names(scores)] <- 1L
    }
}


# NA_r_squared
for(i in 1:5) {
    for(n in setdiff(analysis_vars, no_missing)) {
        scores <- sqrt(NA_r_squared_list[[i]][n,])
        scores <- scores[!(names(scores) %in% always_include)]
        scores <- scores[!(names(scores) %in% names(puc_list[[i]][n,])[puc_list[[i]][n,] < .25])]
        scores <- scores[scores > .05]
        scores <- scores[!is.na(scores)]
        if(length(scores) > 3) scores <- sort(scores, decreasing=TRUE)[1:3]
        pred_matrix_list[[i]][n, names(scores)] <- 1L
    }
}


# Include topcode flag as predictor for each topcoded variable
# CORRECTION: DON'T INCLUDE THESE; EVEN THOUGH THEY ARE A GOOD PREDICTOR
# OF MISSINGNESS, THERE IS NO VARIANCE AMONG OBSERVED CASES, BECAUSE ALL
# TOPCODED CASES HAVE BEEN SWITCHED TO MISSING
for(i in 1:5) {
    for(n in grep("^tc_", names(d), value=TRUE)) {
        pred_matrix_list[[i]][gsub("^tc_", "", n), n] <- 0L
    }
}


# Don't use interaction terms to predict their component part variables
# (This is already taken care of by excluding predictors with low puc.)

# Switch on "always_include" variables
pred_matrix_list <- lapply(pred_matrix_list, function(x) { x[ , always_include] <- 1L ; return(x) } )

# Switch off always_include variables for self
for(i in 1:5) {
    for(n in always_include) {
        pred_matrix_list[[i]][n, n] <- 0L
    }
}

# Turn off all values in no_missing rows
pred_matrix_list <- lapply(pred_matrix_list, function(x) { x[no_missing, ] <- 0L ; return(x) } )

# Turn off all values in all_missing rows
for(i in 1:5) {
    pred_matrix_list[[i]][all_missing[[i]], ] <- 0L
}

# Remove sex as predictor (since it is in group and age_group_by_sex)
pred_matrix_list <- lapply(pred_matrix_list, function(x) { x[ , "sex"] <- 0L ; return(x) } )

# Remove age_group as predictor (since it is in age_group_by_sex)
pred_matrix_list <- lapply(pred_matrix_list, function(x) { x[ , "age_group"] <- 0L ; return(x) } )

# Remove marr_cohab as predictor (since it is in group)
pred_matrix_list <- lapply(pred_matrix_list, function(x) { x[ , "marr_cohab"] <- 0L ; return(x) } )

# Remove wkswork2 from pred_matrix_list[[1]]["incbus", ] to see if it solves singularity problem
# pred_matrix_list[[1]]["incbus", "wkswork2"] <- 0L

# Remove other_inc because it is a linear combination of subfaminc, pn_labern, and labern
pred_matrix_list <- lapply(pred_matrix_list, function(x) { x[,"other_inc"] <- 0L ; return(x) })

# Remove age_group, age_group_by_sex, and group as predictors, and insert group_by_age_group instead
for(i in 1:5) {
    pred_matrix_list[[i]][, c("age_group", "age_group_by_sex", "group")] <- 0L
    pred_matrix_list[[i]][!(rownames(pred_matrix_list[[i]]) %in% c(no_missing, all_missing[[i]], "group_by_age_group")), "group_by_age_group"] <- 1L
}


# Remove wkswork2 as predictor when classwly is also a predictor, to avoid singularities, and replace it with wkswork1
pred_matrix_list <- lapply(pred_matrix_list, function(x) {
    x[which(x[,"classwly"]==1 & x[,"wkswork2"]==1), "wkswork1"] <- 1L
    x[which(x[,"classwly"]==1), "wkswork2"] <- 0L
    return(x)
})

# Turn wkswork1 off for 1969-71, during which is it NA
pred_matrix_list[[1]][, "wkswork1"] <- 0L

# Remove classwly as predictor when srcearn is also a predictor
pred_matrix_list <- lapply(pred_matrix_list, function(x) {
    x[x[,"srcearn"]==1, "classwly"] <- 0L
    return(x)
})

# Remove marst as predictor because of possible collinearity problems with group_by_age_group
pred_matrix_list <- lapply(pred_matrix_list, function(x) { x[,"marst"] <- 0L ; return(x) } )

# Remove inclongj as predictor in 2000 and 2010 when labern is also a predictor
for(i in 4:5) {
    pred_matrix_list[[i]][pred_matrix_list[[i]][,"labern"]==1, "inclongj"] <- 0L
}

# Remove labern as predictor in 2000 and 2010 when inctot is also a predictor
for(i in 4:5) {
    pred_matrix_list[[i]][pred_matrix_list[[i]][,"inctot"]==1, "labern"] <- 0L
}

# Remove incwage as predictor in 2000 and 2010 when inctot is also a predictor
for(i in 4:5) {
    pred_matrix_list[[i]][pred_matrix_list[[i]][,"inctot"]==1, "incwage"] <- 0L
}

# Zero out all predictors for incwelfr_alloc, and make sure it is predictor of incwelfr
for(i in 1:5) {
  	pred_matrix_list[[i]]["incwelfr_alloc", ] <- 0L
  	pred_matrix_list[[i]]["incwelfr", "incwelfr_alloc"] <- 1L
}

# For increti1-2 use passive imputation for the 
# composite variable (incretir) starting in 1989, and only use the composite as a predictor
# of either component
for(i in 3:5) {
    m <- pred_matrix_list[[i]]
    m["incretir", ] <- 0L
    m["increti1", "increti2"] <- 0L
    m["increti2", "increti1"] <- 0L

    m["increti1", "incretir"] <- 1L
    m["increti2", "incretir"] <- 1L

    pred_matrix_list[[i]] <- m
}

# Create report with number of missing cases, R-squared, and proportion of usable cases
# (for regression as a whole) for each target variable
# reg_report_list <- vector(mode="list", length=5)
# 
# for(i in 1:5) {
# 	yr <- seq(1970, 2010, 10)[i]
#     rnames <- rownames(pred_matrix_list[[i]])
#     cnames <- colnames(pred_matrix_list[[i]])
#     reg_report <- matrix(rep(0, 4*nrow(pred_matrix_list[[i]])), ncol=4, dimnames=list(rnames, c("n_missing", "rsq", "puc", "n_preds")))
#     for(r in 1:nrow(pred_matrix_list[[i]])) {
#         preds <- cnames[pred_matrix_list[[i]][r, ]==1]
#         if(length(preds) == 0) next
#         fmla <- as.formula(paste0(rnames[r], " ~ ", paste0(preds, collapse=" + ")))
#         n_missing <- sum(is.na(d[[rnames[r]]][d$year %in% (yr-1):(yr+1)]))
#         do_string(dss("nuc <- nrow(d[year %in% (yr-1):(yr+1) & %s, ])", paste0("is.na(", rnames[r], ") & ", paste0("!is.na(", preds, ")", collapse=" & "))))
#         puc <- round(nuc / n_missing, 2)
#         if(rnames[r] %in% factor_analysis_vars) {
#             reg_report[r, ] <- c(n_missing, NA, puc, length(preds))    
#             # reg <- multinom(fmla, data=d)
#             # do_string(dss("reg0 <- multinom(fmla, data=d[%s, ])", paste0("!is.na(", preds, ")", collapse = " | ")))
#             # rsq <- 1 - (logLik(reg)/logLik(reg0))
#             # rm(reg, reg0)
#         }
#         if(rnames[r] %in% quant_analysis_vars) {
#             rsq <- summary(lm(fmla, data=d[year %in% (yr-1):(yr+1), ]))$r.squared
#             reg_report[r, ] <- c(n_missing, round(rsq, 2), puc, length(preds))
#         }
#     }
#     reg_report_list[[i]] <- reg_report
#     rm(reg_report, rnames, cnames, preds, fmla, rsq, n_missing, nuc, puc)
# }

# Save
save(pred_matrix_list, file="main/2_prepare_for_imputation/3_pred_matrix_list.Rdata")
# save(reg_report_list, file="output/reg_report_list.Rdata")

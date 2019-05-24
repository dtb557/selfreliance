AI <- tail(commandArgs(), 1)
NAI <- as.numeric(AI)

error_dir <- "main/3_multiply_impute/1_error_dump"
if (!dir.exists(error_dir)) dir.create(error_dir)

options(error = quote(dump.frames("main/3_multiply_impute/1_error_dump/error_dump", TRUE)))

# NAI <- 1

A_YEAR <- as.integer(seq(1970, 2010, 10)[NAI])

# Run full imputation
library(data.table)
library(mice)
library(stringr)
library(pscl)
library(msm)
library(MASS)

source("main/3_multiply_impute/1_diagnostic_functions/modified_qqPlot.R")
source("main/3_multiply_impute/1_diagnostic_functions/modified_spreadLevelPlot.R")

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

output_dir <- "main/3_multiply_impute/output" 
coefs_dir <- file.path(output_dir, "coefs")
diag_dir <- file.path(output_dir, "imp_diagnostics")
tnorm_dir <- file.path(diag_dir, "from_tnorm")
coefs_yr_dir <- file.path(coefs_dir, A_YEAR)

for (.dir in c(output_dir, coefs_dir, diag_dir, tnorm_dir, coefs_yr_dir)) {
    if (!dir.exists(.dir)) dir.create(.dir)
}

load("main/1_clean_data/cleaned_data_step_5.Rdata") # returns data as d

# Change pn_labern to zero for non-married/cohabiting people
d[marr_cohab=="Not married or cohabiting", pn_labern := 0L]

# Variables to include (not counting all the decade interactions)
imp_vars <- c("year", "serial", "pernum",
              "proptax", "stampval", "spmcaphous", "ffngcare", "ffngcaid", 
              "sex", "num_earners", "race", "marst", "popstat", "hispan", "educ", 
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
d <- d[!is.na(inctot) & year %in% (A_YEAR-1):(A_YEAR+1), ]

# Create subset with just those variables plus topcode dummies
d <- d[ , c(imp_vars, names(d)[grep("^tc_", names(d))]), with=FALSE]

rm(imp_vars)


#################################################################################
#################################################################################
#################################################################################
#################################################################################
# Create subset for testing
# d <- d[union(sample(1:nrow(d), 10000), c(which(is.na(incdisa2)), which(incdisa2 > 0))), ]
#################################################################################
#################################################################################
#################################################################################
#################################################################################


# Create other_inc
d[ , other_inc := subfaminc-labern-pn_labern]

# Get rid of empty levels in age_group_by_sex
d[ , age_group_by_sex := as.factor(as.character(age_group_by_sex))]

age_group_levels <- c(tail(levels(d$age_group), 1), head(levels(d$age_group), -1))
d[ , age_group := ordered(age_group, levels=age_group_levels)]

# Create group_by_age_group
d[ , group_by_age_group := group:age_group]

# Consolidate srcearn and classwly
d[ , srcearn := as.character(srcearn)]
d[srcearn == "Without pay", srcearn := "niu"]
d[ , srcearn := as.factor(srcearn)]

d[ , classwly := as.character(classwly)]
d[!grepl("^Self-employed|^niu$", classwly), classwly := "Not self-employed"]
d[grepl("^Self-employed", classwly), classwly := "Self-employed"]
d[ , classwly := as.factor(classwly)]

# Create imputation method vector
method_picker <- function(x) {
    if(any(is.na(x))) {
        if(is.numeric(x) | is.integer(x)) return("tnorm")
        else if(is.factor(x)) {
            if(length(levels(x)) > 2) return("rev_pmm")
            else return("rev_pmm")
        }
    }
    else return("")
}

imp_methods <- sapply(d, method_picker)

rm(method_picker)

# Change educ, wkswork2, and age_group methods to "polr"
#imp_methods["educ"] <- "polr"
#imp_methods["wkswork2"] <- "polr"
#imp_methods["age_group"] <- "polr"

# Change race, srcearn, and classwly method to polyreg
for(x in c("race", "srcearn", "classwly")) {
    imp_methods[x] <- "rev_polyreg"
}

# Use passive imputation for age_group, group_by_age_group, age_group_by_sex, group
imp_methods["age_group"] <- '~I(cut(age, 
                                    breaks=c(-Inf, 14, 19, 24, 29, 34, 39, 44, 
                                             49, 54, 59, 64, 69, 74, 79, Inf), 
                                    labels=c("Under 15", "15-19", "20-24", "25-29", 
                                             "30-34", "35-39", "40-44", "45-49", 
                                             "50-54", "55-59", "60-64", "65-69", 
                                             "70-74", "75-79", "80 plus")
                                    )
                               )'
imp_methods["group_by_age_group"] <- "~I(group:age_group)"
imp_methods["age_group_by_sex"] <- "~I(sex:age_group)"
imp_methods["group"] <- "~I(sex:marr_cohab:num_earners)"
if(A_YEAR >= 1990) {
    imp_methods["incretir"] <- "~I(increti1 + increti2)"
}



# Load predictor matrix
load("main/2_prepare_for_imputation/3_pred_matrix_list.Rdata")

# Zero out the age_group rows
pred_matrix_list <- lapply(pred_matrix_list, function(x) {
    x["age_group", ] <- 0L
    x
})

# Remove extra "group_by_age_group" row and column at the end of each matrix
# pred_matrix_list <- lapply(pred_matrix_list, function(x) {
#     x <- x[1:(nrow(x)-1), 1:(ncol(x)-1)]
#     return(x)
# })

# Zero out rows for srcearn and classwly so they aren't imputed
# pred_matrix_list <- lapply(pred_matrix_list, function(x) {
#     x["srcearn",] <- 0L
#     x["classwly",] <- 0L
#     return(x)
# })

if(NAI==5) { # This error cropped up in imputing for 2010
    # Remove group_by_age_group as predictor of race, and include group and age_group_by_sex instead, 
    # to reduce the number of dummies and avoid a "too many weights" error from nnet
    pred_matrix_list <- lapply(pred_matrix_list, function(x) {
        x["race", "group_by_age_group"] <- 0L
        x["race", "group"] <- 0L
        x["race", "age_group_by_sex"] <- 0L
        return(x)
    })
}


# Don't use incwelfr_alloc as a predictor except in years 1979-1981
if(NAI != 2) { 
    pred_matrix_list <- lapply(pred_matrix_list, function(x) {
        x[, "incwelfr_alloc"] <- 0L
        return(x)
    })
}

# Add a row and a column for num_earners
pred_matrix_list <- lapply(pred_matrix_list, function(x) {
    num_earners <- rep(0L, nrow(x))
    x <- cbind(x[,1:which(colnames(x)=="sex")], num_earners, x[,(which(colnames(x)=="sex")+1):ncol(x)])
    num_earners <- rep(0L, ncol(x))
    x <- rbind(x[1:which(rownames(x)=="sex"),], num_earners, x[(which(rownames(x)=="sex")+1):nrow(x),])
    return(x)
})

# Make sure variables aren't used as predictors of themselves
pred_matrix_list <- lapply(pred_matrix_list, function(x) {
    for(v in rownames(x)) {
        x[v, v] <- 0L
    }
    x
})


# Load transformations matrix
tform <- as.matrix(read.csv("main/2_prepare_for_imputation/1_variable_distributions/variable_transformations.csv", row.names=1))
tform <- tform[ , NAI]

# Create adjustments matrix to keep track of added constants
get_adj <- function(x) {
    if(is.numeric(x)) {
        if(min(x, na.rm=T) <= 0) {
            return(-min(x, na.rm=T) + 1)
        } else return(0)
    } else return(0)
}

adj_vector <- sapply(d, get_adj)

# Create dummy for excess zeros
has_excess_zeros <- sapply(d, function(x) {
    if(is.numeric(x)) {
        if(any(!is.na(x))) {
            return(mean(x==0, na.rm=T) > .1)
        } else return(FALSE)
    } else return(FALSE)   
})

# Modify remove.lindep method for faster alternative to rankifremoved
remove.lindep.no.log <- function (x, y, ry, eps = 1e-04, maxcor = 0.99, ...) 
{
    if (ncol(x) == 0) 
        return(NULL)
    if (eps <= 0) 
        stop("\n Argument 'eps' must be positive.")
    xobs <- x[ry, , drop = FALSE]
    yobs <- as.numeric(y[ry])
    keep <- unlist(apply(xobs, 2, var) > eps)
    keep[is.na(keep)] <- FALSE
    highcor <- suppressWarnings((unlist(apply(xobs, 2, cor, yobs)) < 
                                     maxcor))
    keep <- keep & highcor
    if (length(keep) == 1) 
        keep[1] <- TRUE
    k <- sum(keep)
    cx <- cor(xobs[, keep, drop = FALSE], use = "all.obs")
    eig <- eigen(cx, symmetric = TRUE)
    ncx <- cx
    while (eig$values[k]/eig$values[1] < eps) {
        j <- (1:k)[order(abs(eig$vectors[, k]), decreasing = TRUE)[1]]
        keep[keep][j] <- FALSE
        ncx <- cx[keep[keep], keep[keep], drop = FALSE]
        k <- k - 1
        eig <- eigen(ncx)
    }
    return(keep)
}

environment(remove.lindep.no.log) <- environment(mice)

logLik_glm.fit <- function(glm.fit.output) {
    p <- glm.fit.output$rank
    val <- p - glm.fit.output$aic/2
    attr(val, "nobs") <- sum(!is.na(glm.fit.output$residuals))
    attr(val, "df") <- p
    class(val) <- "logLik"
    val
}

environment(logLik_glm.fit) <- environment(mice)

mcfaddens_glm <- function(glm_obj, y) {
    logLik1 <- logLik_glm.fit(glm_obj)
    logLik0 <- logLik(glm(y ~ 1, family=binomial(link=logit)))
    return(1 - (logLik1/logLik0))
}

environment(mcfaddens_glm) <- environment(mice)

# Create truncated normal imputation method
mice.impute.tnorm <- function (y, ry, x, tc_matrix=tcm, varname=get("vname", envir=parent.frame()),
                               needs_transform=tform, adjustments=adj_vector, excess_zeros=has_excess_zeros, ...) {
    
    var_start_time <- Sys.time()
    
    if(varname %in% names(tc_matrix)) {
        topcodes <- tc_matrix[[varname]][!ry]
    } else topcodes <- rep(-Inf, length(which(!ry)))
    
    iter <- get("iteration", pos=parent.frame())
    imp_n <- get("i", pos=parent.frame())
    
    if(excess_zeros[varname]) {
        start_time <- Sys.time()
        dummy_y <- factor(ifelse(y == 0, 0L, 1L))
        aug <- augment(dummy_y, ry, x, ...)
        aug$x <- cbind(1, as.matrix(aug$x))
        expr <- expression(glm.fit(aug$x[aug$ry, ], aug$y[aug$ry], family = binomial(link = logit), 
                                   weights = aug$w[aug$ry]))
        fit <- suppressWarnings(eval(expr))
        # save(
        #     fit, 
        #     file = file.path(
        #         coefs_dir,
        #         sprintf("%s_zeros_%d_%d.Rdata", varname, imp_n, iter)
        #     )
        # )
        mcf <- mcfaddens_glm(fit, aug$y[aug$ry])
        cat("\nLogreg pseudo-r-squared =", mcf, ".")
        fit.sum <- summary.glm(fit)
        beta <- coef(fit)
        if(any(is.na(beta))) {
            aug$x <- aug$x[ , -which(is.na(beta))]
            beta <- beta[!is.na(beta)]
        }
        rv <- t(chol(sym(fit.sum$cov.unscaled)))
	    for_ppc <- list(beta=beta, fit.sum_cov.unscaled=fit.sum$cov.unscaled)
	    save(
	        for_ppc, 
	        file = file.path(
	            coefs_yr_dir,
	            sprintf("%s_zeros_%d_%d.Rdata", varname, imp_n, iter)
            )
	    )
        beta.star <- beta + rv %*% rnorm(ncol(rv))
        p <- 1/(1 + exp(-(aug$x[!aug$ry, ] %*% beta.star)))
        mis_non_zero <- (runif(nrow(p)) <= p)[,1]
        mis_non_zero[topcodes > 0] <- TRUE
        obs_non_zero <- y != 0
        logreg_time <- Sys.time() - start_time
        cat("\nLogreg took", round(logreg_time, 1), units(logreg_time), ".")
        rm(start_time)
    } else {
        mis_non_zero <- rep(TRUE, sum(!ry))
        obs_non_zero <- rep(TRUE, length(y))
    }

    x <- cbind(1, as.matrix(x))
    if(needs_transform[varname] == "y") {
        y <- log(y + adjustments[varname])
        topcodes[!is.infinite(topcodes)] <- log(topcodes[!is.infinite(topcodes)])
    }

    keep <- remove.lindep.no.log(x[obs_non_zero, , drop=FALSE], y[obs_non_zero], ry[obs_non_zero])
    x <- x[ , keep]
    cat("\nRemoved", sum(!keep), "columns from predictor matrix.")
    while(TRUE) {
        parm <- .norm.draw(y[obs_non_zero], ry[obs_non_zero], x[obs_non_zero, ])
	mn <- x[!ry, ] %*% parm$beta
        non_zero_imputed <- numeric(length(mn))
	for(i in seq_along(mn)) {
		non_zero_imputed[i] <- msm::rtnorm(mean=mn[i], lower=topcodes[i], n=1, sd=parm$sigma)
	}
        if(needs_transform[varname] == "y") non_zero_imputed <- exp(non_zero_imputed) - adjustments[varname]
        non_zero_imputed[!mis_non_zero] <- 0
        if(any(is.infinite(non_zero_imputed) | is.na(non_zero_imputed))) {
            cat(paste0("mice.impute.tnorm generated Inf/NaN/NA values for ", varname,
                       "; attempting re-imputation."))
        } else {
            break
        }
    }
    
    save(
        parm, 
        file = file.path(
            coefs_yr_dir,
            sprintf("%s_%d_%d.Rdata", varname, imp_n, iter)
        )
    )

    
    if(iter==10) {

        start_time <- Sys.time()
        lm_obj <- lm(data.table(y[ry & obs_non_zero], x[ry & obs_non_zero, -1]))
        cat("\nLinear regression r-squared =", summary(lm_obj)$r.squared, ".")
        png(
            filename = file.path(
                tnorm_dir,
                paste(varname, A_YEAR, iter, imp_n, "qqplot.png", sep="_")
            )
        )
        try(modified_qqPlot(lm_obj, main=varname))
        dev.off()
        png(
            filename = file.path(
                tnorm_dir, 
                paste(varname, A_YEAR, iter, imp_n, "resid_plot.png", sep="_")
            )
        )
        try(modified_spreadLevelPlot(lm_obj, main=varname))
        dev.off()
        plot_time <- Sys.time() - start_time
        cat("\nIt took", round(plot_time, 1), units(plot_time), "to make plots.")
        rm(start_time, lm_obj)
        
    }

    var_time <- Sys.time() - var_start_time
    cat("\nIt took", round(var_time, 1), units(var_time), "to impute", varname, ".\n\n")
    return(non_zero_imputed)
}

environment(mice.impute.tnorm) <- environment(mice)

# Create revised pmm imputation method
mice.impute.rev_pmm <- function(y, ry, x, donors = 5, type = 1, ridge = 1e-05, version = "", 
                                varname=get("vname", envir=parent.frame()), ...) {
    x <- cbind(1, as.matrix(x))
    ynum <- y
    if (is.factor(y)) 
        ynum <- as.integer(y)
    parm <- NULL
    while(is.null(parm)) {
        try(parm <- .norm.draw(ynum, ry, x, ridge = ridge, ...), silent=TRUE)
        if(is.null(parm)) {
            require(car)
            mvif <- vif(lm(data.table(ynum[ry], x[ry,2:ncol(x)])))
            if(is.matrix(mvif)) worst_var <- rownames(mvif)[mvif[,3]==max(mvif[,3])]
            else worst_var <- names(mvif)[mvif==max(mvif)]
            cat(paste0("Removing ", worst_var, "..."))
            x <- x[ , colnames(x) != worst_var]
        }
    }
    iter <- get("iteration", pos=parent.frame())
    imp_n <- get("i", pos=parent.frame())
    save(
        parm, 
        file = file.path(
            coefs_yr_dir,
            sprintf("%s_%d_%d.Rdata", varname, imp_n, iter)
        )
    )
    yhatobs <- x[ry, ] %*% parm$coef
    yhatmis <- x[!ry, ] %*% parm$beta
    if (version == "2.21") 
        return(apply(as.array(yhatmis), 1, .pmm.match, yhat = yhatobs, 
                     y = y[ry], donors = donors, ...))
    idx <- matcher(yhatobs, yhatmis, k = donors)
    return(y[ry][idx])
}

environment(mice.impute.rev_pmm) <- environment(mice.impute.pmm)

# Create revised polyreg

mice.impute.rev_polyreg <- function (y, ry, x, nnet.maxit = 100, nnet.trace = FALSE, nnet.maxNWts = 1500, 
                                     varname=get("vname", envir=parent.frame()), ...) {
    x <- as.matrix(x)
    aug <- augment(y, ry, x, ...)
    x <- aug$x
    y <- aug$y
    ry <- aug$ry
    w <- aug$w
    fy <- as.factor(y)
    nc <- length(levels(fy))
    un <- rep(runif(sum(!ry)), each = nc)
    xy <- cbind.data.frame(y = y, x = x)
    if (ncol(x) == 0L) 
        xy <- data.frame(xy, int = 1)
    fit <- multinom(formula(xy), data = xy[ry, , drop = FALSE], 
                    weights = w[ry], maxit = nnet.maxit, trace = nnet.trace, 
                    maxNWts = nnet.maxNWts, ...)
    iter <- get("iteration", pos=parent.frame())
    imp_n <- get("i", pos=parent.frame())
    save(
        fit, 
        file = file.path(
            coefs_yr_dir,
            sprintf("%s_%d_%d.Rdata", varname, imp_n, iter)
        )
    )
    post <- predict(fit, xy[!ry, ], type = "probs")
    if (sum(!ry) == 1) 
        post <- matrix(post, nrow = 1, ncol = length(post))
    if (is.vector(post)) 
        post <- matrix(c(1 - post, post), ncol = 2)
    draws <- un > apply(post, 1, cumsum)
    idx <- 1 + apply(draws, 2, sum)
    return(levels(fy)[idx])
}

environment(mice.impute.rev_polyreg) <- environment(mice.impute.polyreg)

# Load topcodes and transform into matrix with rownames as varnames and colnames as years
topcodes <- data.table(read.csv("original_data/topcode_values.csv", stringsAsFactors=FALSE))
varnames <- topcodes$var
topcodes[ , var := NULL]
topcodes <- as.matrix(topcodes)
rownames(topcodes) <- tolower(varnames)
colnames(topcodes) <- gsub("^X", "", colnames(topcodes))
rm(varnames)

# Adjust for inflation
pce <- c("1969"=21.326, "1970"=22.325, "1971"=23.274, "1979"=39.714, "1980"=43.978, 
         "1981"=47.908, "1989"=64.641, "1990"=67.440, "1991"=69.652, "1999"=81.110, 
         "2000"=83.131, "2001"=84.736, "2009"=100.000, "2010"=101.653, "2011"=104.149)

for(yr in names(pce)) {
    topcodes[ , yr] <- topcodes[ , yr]*100/pce[yr]
}

topcodes <- round(topcodes)


for(varname in rownames(topcodes)) {
    do_string(dss("d[ , tcm_%s := topcodes[\"%s\", as.character(year)]*tc_%s]", c(varname, varname, varname)))
    do_string(dss("d[tcm_%s==0 | is.na(tcm_%s), tcm_%s := -Inf]", c(varname, varname, varname)))
}
        
tcm <- d[ , grep("^tcm_", names(d), value=TRUE), with=FALSE]
setnames(tcm, names(tcm), gsub("^tcm_", "", names(tcm)))

for(n in grep("^tcm_", names(d), value=TRUE)) {
    do_string(dss("d[ , %s := NULL]", n))
}

# Make a "form" vector to specify polynomial predictors
poly_if_numeric <- function(varnames, degree=2) {
    out <- sapply(varnames, function(x) {
        if(is.numeric(d[[x]])) return(paste0("poly(", x, ", degree=3)"))
        else return(x)
    }, USE.NAMES=FALSE)
    return(out)
}

imp_formulas <- apply(pred_matrix_list[[NAI]], 1, function(x) {
    preds <- colnames(pred_matrix_list[[NAI]])[x==1]
    if(length(preds)==0) return("")
    else return(paste0("~ ", paste0(poly_if_numeric(preds), collapse="+")))
})

# Add replicate data for posterior predictive checking
# replicate <- as.data.table(lapply(d, function(x) {
#         if(any(is.na(x))) {
#             v <- rep(NA, length(x))
#             if(is.factor(x)) {
#                 if(is.ordered(x)) {
#                     new_x <- ordered(v, levels=levels(x))
#                 } else new_x <- factor(v, levels=levels(x))
#             } else {
#                 new_x <- v
#                 class(new_x) <- class(x)
#             }
#             return(new_x)
#         }
#         else return(x)
#     }))
# for(i in 1:499) {
#     cat(i, " ")
#     d <- rbindlist(list(d, replicate))
# }
# rm(replicate)
# orig <- tcm
# for(i in 1:499) {
#     tcm <- rbindlist(list(tcm, orig))
# }
# rm(orig)

#################################################################################
#################################################################################
#################################################################################
#################################################################################
# Specify a visitSequence, probably for testing purposes
# number_missing <- sapply(d, function(x) sum(is.na(x)))
# increasing_order_of_missingness <- order(number_missing)
# zero_missing <- which(number_missing==0)
# incdrt_index <- which(names(d)=="incdrt")
# incdisa2_index <- which(names(d)=="incdisa2")
# increasing_order_of_missingness <- increasing_order_of_missingness[
#    !(increasing_order_of_missingness %in% c(zero_missing, incdisa2_index))
# ]
# VS <- c(incdisa2_index, increasing_order_of_missingness)
#################################################################################
#################################################################################
#################################################################################
#################################################################################

ITER_DIR = "main/3_multiply_impute/1_imp_iterations/"

if (!dir.exists(ITER_DIR)) dir.create(ITER_DIR)

saved_iterations <- list.files(path=ITER_DIR, pattern=dss("imp_%s_\\d\\.Rdata", c(A_YEAR)))
if(length(saved_iterations)==0) {
    print("Didn't find any saved iterations, starting from scratch.")
    imp <- mice(
        d, 
        m = 10, 
        maxit = 1, 
        method = imp_methods, 
        predictorMatrix = pred_matrix_list[[NAI]], 
        visitSequence = "monotone", 
        form = imp_formulas, 
        MaxNWts=2500
    )
    rm(d, imp_formulas, pred_matrix_list)
    save(imp, file=dss("%s/imp_%s_1.Rdata", c(ITER_DIR, A_YEAR)))
    for(i in 2:10) {
        imp <- mice.mids(imp, maxit=1, MaxNWts=2500)
        save(imp, file=dss("%s/imp_%s_%s.Rdata", c(ITER_DIR, A_YEAR, i)))
    }
} else {
    rm(d, imp_formulas, pred_matrix_list)
    iteration_number <- max(as.numeric(str_match(saved_iterations, dss("imp_%s_(\\d)\\.Rdata", c(A_YEAR)))[,2]))
    print(paste0("Found saved imputation, iteration number ", iteration_number, "."))
    load(dss("%s/imp_%s_%s.Rdata", c(ITER_DIR, A_YEAR, iteration_number)))
    for(i in (iteration_number+1):10) {
        imp <- mice.mids(imp, maxit=1, MaxNWts=2500)
        save(imp, file=dss("%s/imp_%s_%s.Rdata", c(ITER_DIR, A_YEAR, i)))
    }
}

# Set upper threshold for imputed values, then transpose values
# above the topcode or max to the range between the topcode/max 
# and that upper threshold

library(data.table)

yrs <- seq(1970, 2010, 10)

income_vars <- c("eitcred", "fedtax", "adjginc", "taxinc", 
                 "fedtaxac", "capgain", "caploss", "statetax", "stataxac", 
                 "incdivid", "incint", "incrent", "incother", "incalim", "incasist", 
                 "incss", "incwelfr", "incwkcom", "incvet", "incchild", "incunemp", 
                 "inceduc", "incssi", "incwage", "incbus", "incfarm", "incsurv1", 
                 "incsurv2", "incdisa1", "incdisa2", "increti1", "increti2", 
                 "proptax", "incdrt", "incgov", "incidr", "incaloth", "oincwage", 
                 "oincbus", "oincfarm", "inclongj")


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

extreme_values <- lapply(c(yrs, 2011), function(x) {
    as.matrix(read.csv(sprintf("output/extreme_values_%d.csv", x), 
                    row.names=1, check.names=FALSE))
})

thresholds <- numeric(length(income_vars))
names(thresholds) <- income_vars

# First, get thresholds
for(v in income_vars) {
    is_imputed <- any(sapply(extreme_values, function(x) v %in% rownames(x)))
    if(!is_imputed) {
        thresholds[v] <- NA
        next
    }
    is_topcoded <- v %in% rownames(topcodes)
    if(is_topcoded) {
        max_topcode <- max(topcodes[v,], na.rm=TRUE)
        thresholds[v] <- 4*max_topcode
    } else {
        max_observed <- max(sapply(extreme_values, function(x) {
            if(v %in% rownames(x)) x[v, "Obs Max"] else NA
            }
        ), na.rm=TRUE)
        thresholds[v] <- 4*max_observed
    }
}

transpose <- function(x, bottom, old_top, new_top) {
    above_min <- x-bottom
    ppn_above_min <- above_min/(old_top-bottom)
    return(bottom + ppn_above_min*(new_top-bottom))
}

for(yr in yrs) {
    cat(yr, "\n")
    imp_file <- sprintf("Data/imp_iterations/with_ppc/imp_%d_10.Rdata", yr)
    load(imp_file)
    imp$transposed <- sapply(income_vars, function(x) NULL)
    imp$thresholds <- sapply(income_vars, function(x) NULL)
    
    report <- matrix(0L, nrow=length(income_vars), ncol=4, 
                     dimnames=list(income_vars, c("Topcode/Max Observed", 
                                                  "Max Imputed", 
                                                  "Upper Threshold", 
                                                  "Avg n Transposed")))
    
    for(v in income_vars) {
        if(imp$nmis[v] %in% c(0, nrow(imp$data))) {
            report <- report[-which(rownames(report)==v), ]
            next
        }
        else {
            if(v %in% rownames(topcodes)) {
                bottom <- max(topcodes[v, ], na.rm=TRUE)
            } else bottom <- max(imp$data[[v]], na.rm=TRUE)
            old_top <- max(imp$imp[[v]])
            if(old_top > thresholds[v]) {
                imp$transposed[[v]] <- imp$imp[[v]] > bottom
                imp$imp[[v]][imp$transposed[[v]]] <- 
                    transpose(imp$imp[[v]][imp$transposed[[v]]], 
                              bottom, old_top, thresholds[v])
                report[v, "Avg n Transposed"] <- mean(apply(imp$transposed[[v]], 2, sum))
            } else {
                report[v, "Avg n Transposed"] <- 0
            }
            imp$thresholds[[v]] <- c("bottom"=bottom, 
                                     "old_top"=old_top, 
                                     "new_top"=thresholds[v])
            report[v, "Topcode/Max Observed"] <- bottom
            report[v, "Max Imputed"] <- old_top
            report[v, "Upper Threshold"] <- thresholds[v]
        }
    }
    write.csv(report, file=sprintf("output/extreme_values_transposition_report_%d.csv", yr))
    save(imp, file=sprintf("Data/imp_iterations/with_ppc/imp_%d_10_extreme_values_transposed.Rdata", yr))
}

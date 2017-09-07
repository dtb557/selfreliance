# Make tables (one for each decade probably; 2011 is the only year for which the topcodes differ within a decade trio of years, so might have to do separate table for 2011) including (for each imputed income variable) mean observed value, SD, max observed value, topcode, n to be imputed, 1.3/1.4/1.5 times topcode and n cases above each, 3/4/5 SDs above topcode and n cases above each.

library(data.table)

yrs <- c(seq(1970, 2010, 10), 2011)

income_vars <- c("eitcred", "fedtax", "adjginc", "taxinc", 
                         "fedtaxac", "capgain", "caploss", "statetax", "stataxac", 
                         "incdivid", "incint", "incrent", "incother", "incalim", "incasist", 
                         "incss", "incwelfr", "incwkcom", "incvet", "incchild", "incunemp", 
                         "inceduc", "incssi", "incwage", "incbus", "incfarm", "incsurv1", 
                         "incsurv2", "incdisa1", "incdisa2", "increti1", "increti2", 
                         "proptax", "incdrt", "incgov", "incidr", "incaloth", "oincwage", 
                         "oincbus", "oincfarm", "inclongj")

qois <- c("Obs Mean", "Obs SD", "Obs Max", "Topcode", "n Missing", "n Topcoded",
                                  "1.5 x Topcode", "% > 1.5 x Topcode", "2.5 x Topcode", 
                                  "% > 2.5 x Topcode", "4 x Topcode", "% > 4 x Topcode", 
                                  "1.5 x Max", "% > 1.5 x Max", "2.5 x Max", 
                                  "% > 2.5 x Max", "4 x Max", "% > 4 x Max") # "Mean + 3 SD", "n > Mean + 3 SD", 
                                  #"Mean + 4 SD", "n > Mean + 4 SD", "Mean + 5 SD", 
                                  #"n > Mean + 5 SD")

topcodes <- data.table(read.csv("Data/topcode_values.csv", stringsAsFactors=FALSE))
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

for(yr in yrs) {
    yr_label <- if(yr==2011) 2010 else yr
    imp_file <- sprintf("Data/imp_iterations/with_ppc/imp_%d_10.Rdata", yr_label)
    load(imp_file)
    out <- matrix(0.0, nrow=length(income_vars), ncol=length(qois), 
                  dimnames=list(income_vars, qois))
    if(yr==2010) {
        yr_select <- imp$data$year %in% 2009:2010    
    } else if(yr==2011) {
        yr_select <- imp$data$year==2011
    } else yr_select <- rep(TRUE, nrow(imp$data))
    for(v in income_vars) {
        x <- imp$data[,v]
        if(yr > 2001) {
            imp$imp[[v]] <- imp$imp[[v]][yr_select[is.na(x)], ]
            x <- x[yr_select]
        }
        nmis <- sum(is.na(x))
        if(nmis==nrow(imp$data[yr_select,]) | nmis==0) {
            out <- out[-which(rownames(out)==v),]
            next
        }
        ntc <- if(v %in% rownames(topcodes)) {
            sum(imp$data[yr_select, paste0("tc_", v)])
        } else NA
        mn <- mean(x, na.rm=TRUE)
        .sd <- sd(x, na.rm=TRUE)
        .max <- max(x, na.rm=TRUE)
        tc <- if(v %in% rownames(topcodes)) {
            if(yr != 2011) topcodes[v, as.character(yr-1)] else topcodes[v, "2011"]
        } else NA
        # x1.3 <- 1.3*tc
        # x1.4 <- 1.4*tc
        x1.5 <- 1.5*tc
        x2.5 <- 2.5*tc
        x4 <- 4*tc
        max1.5 <- 1.5*.max
        max2.5 <- 2.5*.max
        max4 <- 4*.max
        # sd3 <- mn + 3*.sd
        # sd4 <- mn + 4*.sd
        # sd5 <- mn + 5*.sd
        out[v, "Obs Mean"] <- round(mn)
        out[v, "Obs SD"] <- round(.sd)
        out[v, "Obs Max"] <- round(.max)
        out[v, "Topcode"] <- round(tc)
        out[v, "n Missing"] <- nmis
        out[v, "n Topcoded"] <- ntc
        # out[v, "1.3 x Topcode"] <- x1.3
        # out[v, "n > 1.3 x Topcode"] <- mean(apply(imp$imp[[v]] > x1.3, 2, sum))
        # out[v, "1.4 x Topcode"] <- x1.4
        # out[v, "n > 1.4 x Topcode"] <- mean(apply(imp$imp[[v]] > x1.4, 2, sum))
        out[v, "1.5 x Topcode"] <- round(x1.5)
        out[v, "% > 1.5 x Topcode"] <- 100*mean(apply(imp$imp[[v]] > x1.5, 2, sum))/nmis
        out[v, "2.5 x Topcode"] <- round(x2.5)
        out[v, "% > 2.5 x Topcode"] <- 100*mean(apply(imp$imp[[v]] > x2.5, 2, sum))/nmis
        out[v, "4 x Topcode"] <- round(x4)
        out[v, "% > 4 x Topcode"] <- 100*mean(apply(imp$imp[[v]] > x4, 2, sum))/nmis
        # out[v, "Mean + 3 SD"] <- sd3
        # out[v, "n > Mean + 3 SD"] <- mean(apply(imp$imp[[v]] > sd3, 2, sum))
        # out[v, "Mean + 4 SD"] <- sd4
        # out[v, "n > Mean + 4 SD"] <- mean(apply(imp$imp[[v]] > sd4, 2, sum))
        # out[v, "Mean + 5 SD"] <- sd5
        # out[v, "n > Mean + 5 SD"] <- mean(apply(imp$imp[[v]] > sd5, 2, sum))
        out[v, "1.5 x Max"] <- round(max1.5)
        out[v, "% > 1.5 x Max"] <- 100*mean(apply(imp$imp[[v]] > max1.5, 2, sum))/nmis
        out[v, "2.5 x Max"] <- round(max2.5)
        out[v, "% > 2.5 x Max"] <- 100*mean(apply(imp$imp[[v]] > max2.5, 2, sum))/nmis
        out[v, "4 x Max"] <- round(max4)
        out[v, "% > 4 x Max"] <- 100*mean(apply(imp$imp[[v]] > max4, 2, sum))/nmis
    }
    write.csv(out, file=sprintf("output/extreme_values_%d.csv", yr))
    
}

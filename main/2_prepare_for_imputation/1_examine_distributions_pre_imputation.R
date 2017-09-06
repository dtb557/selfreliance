library(data.table)

#setwd("../../")

norm_approx <- function(x) {
    m <- mean(x, na.rm=TRUE)
    s <- sd(x, na.rm=TRUE)
    .x <- seq(min(x, na.rm=TRUE), max(x, na.rm=TRUE), length.out=300)
    return(cbind(.x, dnorm(.x, m, s)))
}

# Load data
load("main/1_clean_data/cleaned_data_step_5.Rdata")

is_numeric_not_topcode_not_id_var <- function(x) {
    x[sapply(x, function(y) is.numeric(d[[y]]) & !grepl("^tc_", y) & !(y %in% c("serial", "pernum")), USE.NAMES=FALSE)]
}

d <- d[ , is_numeric_not_topcode_not_id_var(names(d)), with=FALSE]

d <- d[!is.na(inctot), ]

# Make histograms with Normal density lines
dist_dir <- "main/2_prepare_for_imputation/variable_distributions"
if(!dir.exists(dist_dir)) dir.create(dist_dir)
for(yr in seq(1970, 2010, 10)) {
    for(v in names(d)) {
        vv <- d[[v]][d[["year"]] %in% (yr-1):(yr+1)]
        if(sum(!is.na(vv)) == 0) next
        png(file.path(dist_dir, paste0(v, "_", yr, ".png")))
        hist(vv, main=v, freq=FALSE)
        lines(norm_approx(vv), col="red")
        dev.off()
    }
}
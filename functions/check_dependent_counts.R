# Unfortunately I couldn't find notes on why I created the script
# on which this function is based, but my best guess is that it's 
# just checking to make sure the total number of dependent exemptions 
# (column 5) is greater than the number of dependents under age 17 
# (column 19)
check_dependent_counts <- function(input_dir) {
    sr_files <- list.files(input_dir, full.names=TRUE)
    dep_counts <- lapply(sr_files, function(x) {
        tmp <- read.table(paste0("taxsim/taxsim_input/", x), quote="")
        return(c(n_dependents=sum(tmp[ , 5]), 
                    n_dependents_under_17=sum(tmp[ , 19])))
    })
    avg_n_dependents <- mean(sapply(dep_counts, `[`, "n_dependents"))
    avg_n_dependents_under_17 <- mean(sapply(dep_counts, `[`, "n_dependents_under_17"))
    if(avg_n_dependents > avg_n_dependents_under_17) {
        cat("Success: average number of dependents is greater than average number of dependents under 17.")
    } else {
        warning("Average number of dependents under 17 is greater than or equal to the average number of total dependents; something is likely wrong with the TAXSIM input files.")
    }
}

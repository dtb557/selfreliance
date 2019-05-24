check_dependent_counts <- function(input_dir) {
    sr_files <- list.files(input_dir, full.names=TRUE)
    dep_counts <- lapply(sr_files, function(x) {
        tmp <- read.table(x, quote="")
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

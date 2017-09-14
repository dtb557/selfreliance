fixed_wtd.rank <- function (x, weights = NULL, normwt = FALSE, na.rm = TRUE) {
    require(Hmisc)
    if (!length(weights)) 
        return(rank(x, na.last = if (na.rm) NA else TRUE))
    tab <- wtd.table(x, weights, normwt = normwt, na.rm = na.rm)
    freqs <- tab$sum.of.weights
    r <- cumsum(freqs) - 0.5 * (freqs - 1)
    approx(tab$x, r, xout = x, rule=2)$y # The fix is to add rule=2
}
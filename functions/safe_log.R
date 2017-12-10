safe_log <- function(x, adj=1) {
    if (min(x, na.rm=TRUE) <= 0) {
        return(log(x - min(x, na.rm=TRUE) + adj))
    }
    else {
        return(log(x))
    }
}
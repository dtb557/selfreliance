dss <- function(text, subs) {
    split = strsplit(text, "%s")[[1]]
    if(grepl("%s$", text)) split <- c(split, "")
    if (length(split) - 1 != length(subs)) {
        return("Error: Number of wildcard characters does not equal number of variables to substitute.")
    }
    return(paste0(split, c(subs, ""), collapse=""))
}
exclude_hhs_with_top_decile_male_earner <- function(data) {
    cut_above <- data[sex=="Male", wtd.quantile(labern, weights=wtsupp, probs=0.9), by=year]
    cut_above <- with(cut_above, structure(V1, names=year))
    data[ , top_decile_male_earner := sex=="Male" & labern > cut_above[as.character(year)]]
    data[ , has_top_male_earner := any(top_decile_male_earner), by=.(year, serial)]
    return(data[has_top_male_earner==FALSE, 
             -c("top_decile_male_earner","has_top_male_earner"), 
             with=FALSE])
}
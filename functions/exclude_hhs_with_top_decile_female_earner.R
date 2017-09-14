exclude_hhs_with_top_decile_female_earner <- function(data) {
    require(Hmisc)
    cut_above <- data[sex=="Female", wtd.quantile(labern, weights=wtsupp, probs=0.9), by=year]
    cut_above <- with(cut_above, structure(V1, names=year))
    data[ , top_decile_female_earner := sex=="Female" & labern > cut_above[as.character(year)]]
    data[ , has_top_female_earner := any(top_decile_female_earner), by=.(year, serial)]
    return(data[has_top_female_earner==FALSE, 
             -c("top_decile_female_earner","has_top_female_earner"), 
             with=FALSE])
}
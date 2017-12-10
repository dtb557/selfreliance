make_decile_groups <- function(variable, wt) {
    require(Hmisc)
    deciles <- Hmisc::wtd.quantile(
        variable, 
        weights = wt, 
        probs=seq(.1, .9, .1)
    )
    decile_groups <- cut(
        variable, 
        breaks = c(0, deciles, Inf), 
        labels = as.character(1:10),
        include.lowest = TRUE, 
        right = FALSE, 
        ordered_result = TRUE
    )
    decile_groups
}
limit_age_range <- function(data, age_min = 25, age_max = 54) {
    data[age %in% age_min:age_max & (is.na(pn_age) | pn_age %in% age_min:age_max), ]
}
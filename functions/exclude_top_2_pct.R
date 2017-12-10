exclude_top_2_pct <- function(data, flag_version = c("non-imputed", "imputed")) {
    flag_version <- match.arg(flag_version)
    if(flag_version == "non-imputed") {
        load("main/7_make_exclusion_flags/1_top_2_pct_flag_non_imp.Rdata")
        flag <- top_2_flag_non_imp
    } else {
        load("main/7_make_exclusion_flags/1_top_2_pct_flag_imp.Rdata")
        flag <- top_2_flag_imp
    }
    setkey(flag, year, serial, pernum)
    setkey(data, year, serial, pernum)
    flag[data][top_2_pct == FALSE, ]
}

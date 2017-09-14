exclude_alloc <- function(data) {
    load("main/7_make_exclusion_flags/2_exclude_alloc_flag.Rdata")
    setkey(data, year, serial, pernum)
    to_exclude[data][alloc == FALSE, ]
}
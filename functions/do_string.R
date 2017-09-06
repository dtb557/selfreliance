do_string <- function(x) {
    eval(parse(text=x), envir=parent.frame())
}
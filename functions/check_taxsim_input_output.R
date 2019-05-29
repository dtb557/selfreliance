# Make sure TAXSIM results files have the same number of lines as TAXSIM 
# input files; if they don't, TAXSIM probably hit an error
check_taxsim_input_output <- function(input_dir, output_dir) {
    in_files <- list.files(input_dir)
    for(f in in_files) {
        cat(f, "\n")
        in_f <- read.table(file.path(input_dir, f), quote="", skip=1)
        out_f <- read.table(file.path(output_dir, paste0(f, ".taxsim")), 
                            quote = "", skip = 1) 
        if(nrow(in_f)==nrow(out_f)) next
        else stop(paste0("Length of input and output do not match for ", f))
    }
    cat("All input and output files have the same number of observations.")
}
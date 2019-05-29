upload_taxsim_input_files <- function(input_dir) {
    taxsim_input_files <- list.files(input_dir, full.names = FALSE)
    p <- dplyr::progress_estimated(length(taxsim_input_files))
    for (f in taxsim_input_files) {
        RCurl::ftpUpload(
            file.path(input_dir, f),
            paste0("ftp://taxsim:02138@taxsimftp.nber.org/tmp/", f)
        )
        p$tick()$print()
    }
}
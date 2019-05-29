download_taxsim_output_files <- function(input_dir, output_dir) {
    taxsim_input_files <- list.files(input_dir, full.names=FALSE)
    taxsim_output_files <- file.path(
        output_dir, 
        paste0(taxsim_input_files, ".taxsim")
    )
    # Don't download output files that already exist
    taxsim_input_files <- taxsim_input_files[!file.exists(taxsim_output_files)]
    p <- dplyr::progress_estimated(length(taxsim_input_files))
    for (f in taxsim_input_files) {
        out <- RCurl::getURLContent(
            url = paste0("ftp://taxsimftp.nber.org/tmp/", f, ".taxsim"),
            userpwd = "taxsim:02138",
            ftp.use.epsv = FALSE,
            dirlistonly = FALSE
        )
        write_taxsim(out, file.path(output_dir, paste0(f, ".taxsim")))
        p$tick()$print()
    }
}

write_taxsim <- function(data, path) {
    fout <- file(path, open = "wt")
    on.exit(close(fout))
    if (is.raw(data)) {
        write(rawToChar(data), file = fout)
    } else {
        write(data, file = fout)
    }
}
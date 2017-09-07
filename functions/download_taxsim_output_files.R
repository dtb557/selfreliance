download_taxsim_output_files <- function(input_dir, output_dir) {
    taxsim_input_files <- list.files(input_dir, full.names=FALSE)
    downloader_file <- "tmp_download.txt"
    output_files <- paste0(taxsim_input_files, ".taxsim")
    get_cmds <- paste0("get ", output_files, " ", file.path(output_dir, output_files))
    cmds <- c("open taxsimftp.nber.org", 
              "taxsim", 
              "02138", 
              "cd tmp", 
              "prompt", 
              get_cmds)
    cat(paste0(cmds, collapse="\n"), file=downloader_file)
    system(paste0("ftp -s:", downloader_file))
    file.remove(downloader_file)
}
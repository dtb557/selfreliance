upload_taxsim_input_files <- function(input_dir) {
    taxsim_input_files <- list.files(input_dir, full.names=TRUE)
    uploader_file <- "tmp_upload.txt"
    mput_cmd <- paste0("mput ", paste0(taxsim_input_files, collapse=" "))
    cmds <- c("open taxsimftp.nber.org", 
              "taxsim", 
              "02138", 
              "cd tmp", 
              "prompt", 
              mput_cmd)
    cat(paste0(cmds, collapse="\n"), file=uploader_file)
    system(paste0("ftp -s:", uploader_file))
    file.remove(uploader_file)
}
# Make table of sample size by year and gender
library(data.table)

IN_DIR <- "main/6a_make_analysis_dataset_imputed"
OUT_DIR <- "main/8a_perform_decomposition_imputed"

out_list <- lapply(
    seq(1970, 2010, 10), 
    function(yr) {
        imp_file <- sprintf("1_imps_%d_analysis_vars.Rdata", yr)
        load(file.path(IN_DIR, imp_file))
        imp <- imps[[1]]
        imp[ , decade := signif(year, 3)]
        imp[ , .(n = .N), keyby = .(Year = decade, Sex = sex)]
    }
)

out <- rbindlist(out_list)

write.csv(out, file = file.path(OUT_DIR, "4_sample_size_table.csv"), row.names = FALSE)
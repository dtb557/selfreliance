
Economic Self-Reliance and Gender Inequality
============================================

This is a replication repository for Deirdre Bloome, Derek Burk, and Leslie McCall, "Economic Self-Reliance and Gender Inequality between U.S. Men and Women, 1970–2010," American Journal of Sociology 124, no. 5 (March 2019): 1413- 1467.

Overview of directory structure
-------------------------------

**main**: This directory contains all the scripts that can be used to reproduce our main analysis.

**functions**: This directory contains scripts which define functions used in the analysis scripts in **main** and **auxiliary**.

**original\_data**: This directory contains all data files necessary to reproduce our analyses. All data are derived from IPUMS CPS extracts, downloaded from the [IPUMS CPS website](https://cps.ipums.org/cps/). These data are posted here with the permission of IPUMS.

Sarah Flood, Miriam King, Steven Ruggles, and J. Robert Warren. Integrated Public Use Microdata Series, Current Population Survey: Version 5.0 \[dataset\]. Minneapolis, MN: University of Minnesota, 2017. <https://doi.org/10.18128/D030.V5.0>

The file "cps\_master.Rdata" was created by loading an IPUMS extract downloaded as a .dta file into R using the **foreign** package, and saving it to an .Rdata file without modification. All other data files were downloaded directly from IPUMS CPS. The file "topcode\_values.csv" is derived from the income component topcode values provided [here](https://cps.ipums.org/cps/topcodes_tables.shtml).

**auxiliary**: This directory contains scripts used for sensitivity analyses.

Required R packages
-------------------

The following packages were used in our analysis:

-   data.table
-   stringr
-   foreign
-   mice, version 2.25
-   nnet
-   car
-   MASS
-   pscl
-   RCurl
-   dplyr
-   magrittr
-   ipumsr
-   Hmisc
-   weights

Unfortunately, we did not rigorously track the package versions used in the analysis, with the exception of the mice package. However, we were able to replicate our results with the following setup:

``` r
pkgs <- c("data.table", "stringr", "foreign", "mice", "nnet", "car", "MASS", 
          "pscl", "RCurl", "dplyr", "magrittr", "ipumsr", "Hmisc", "weights")
for (p in pkgs) {
    suppressPackageStartupMessages(library(p, character.only = TRUE))
}
sessionInfo()
```

    ## R version 3.5.1 (2018-07-02)
    ## Platform: x86_64-w64-mingw32/x64 (64-bit)
    ## Running under: Windows 10 x64 (build 17134)
    ## 
    ## Matrix products: default
    ## 
    ## locale:
    ## [1] LC_COLLATE=English_United States.1252 
    ## [2] LC_CTYPE=English_United States.1252   
    ## [3] LC_MONETARY=English_United States.1252
    ## [4] LC_NUMERIC=C                          
    ## [5] LC_TIME=English_United States.1252    
    ## 
    ## attached base packages:
    ## [1] stats     graphics  grDevices utils     datasets  methods   base     
    ## 
    ## other attached packages:
    ##  [1] weights_1.0       gdata_2.18.0      Hmisc_4.2-0      
    ##  [4] ggplot2_3.1.1     Formula_1.2-3     survival_2.42-3  
    ##  [7] lattice_0.20-35   ipumsr_0.3.0      magrittr_1.5     
    ## [10] dplyr_0.8.0.1     RCurl_1.95-4.12   bitops_1.0-6     
    ## [13] pscl_1.5.2        MASS_7.3-50       car_3.0-2        
    ## [16] carData_3.0-2     nnet_7.3-12       mice_2.25        
    ## [19] Rcpp_1.0.1        foreign_0.8-70    stringr_1.4.0    
    ## [22] data.table_1.11.8
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] gtools_3.8.1        assertthat_0.2.1    zeallot_0.1.0      
    ##  [4] rprojroot_1.3-2     digest_0.6.18       R6_2.4.0           
    ##  [7] cellranger_1.1.0    plyr_1.8.4          backports_1.1.4    
    ## [10] acepack_1.4.1       evaluate_0.13       pillar_1.3.1       
    ## [13] rlang_0.3.4         lazyeval_0.2.2      curl_3.2           
    ## [16] readxl_1.1.0        rstudioapi_0.10     rpart_4.1-13       
    ## [19] Matrix_1.2-14       checkmate_1.9.1     rmarkdown_1.10     
    ## [22] splines_3.5.1       htmlwidgets_1.3     munsell_0.5.0      
    ## [25] compiler_3.5.1      xfun_0.6            pkgconfig_2.0.2    
    ## [28] base64enc_0.1-3     htmltools_0.3.6     tidyselect_0.2.5   
    ## [31] tibble_2.1.1        gridExtra_2.3       htmlTable_1.13.1   
    ## [34] rio_0.5.16          crayon_1.3.4        withr_2.1.2        
    ## [37] grid_3.5.1          gtable_0.3.0        scales_1.0.0       
    ## [40] zip_2.0.2           stringi_1.4.3       latticeExtra_0.6-28
    ## [43] openxlsx_4.1.0      RColorBrewer_1.1-2  tools_3.5.1        
    ## [46] forcats_0.3.0       glue_1.3.1          purrr_0.3.2        
    ## [49] hms_0.4.2           abind_1.4-5         yaml_2.2.0         
    ## [52] colorspace_1.4-1    cluster_2.0.7-1     knitr_1.22         
    ## [55] haven_1.1.2

To install mice version 2.25, you can use the following code:

``` r
# install.packages("remotes")
remotes::install_version("mice", version = "2.25")
```

Replication guide
-----------------

All scripts necessary to reproduce our main analysis can be found in the **main** directory. The subdirectories of **main** are numbered according to the order in which scripts should be run, and the files within each subdirectory are similarly numbered. We list these scripts below, in order, and include notes describing any parameters that need to be set and the outputs created by each script. All scripts create output in the same directory as the script itself, or in a subdirectory of that directory.

### main/1\_clean\_data

-   **clean\_data\_step\_1.R**: Creates output "cleaned\_data\_step\_1.Rdata".
-   **clean\_data\_step\_2.R**: Creates output "cleaned\_data\_step\_2.Rdata".
-   **clean\_data\_step\_2\_no\_imputation**: Creates output "cleaned\_data\_step\_2\_no\_imputation.Rdata".
-   **clean\_data\_step\_3.R**: This script requires the analyst to set the value of the `imputation` parameter at the top of the script to `TRUE` or `FALSE`. When `imputation == TRUE`, the script creates output "cleaned\_data\_step\_3.Rdata". When `imputation == FALSE`, the script creates output "cleaned\_data\_step\_3\_no\_imputation.Rdata".
-   **clean\_data\_step\_4.R**: This script requires the analyst to set the value of the `imputation` parameter at the top of the script to `TRUE` or `FALSE`. When `imputation == TRUE`, the script creates output "cleaned\_data\_step\_4.Rdata". When `imputation == FALSE`, the script creates output "cleaned\_data\_step\_4\_no\_imputation.Rdata".
-   **clean\_data\_step\_5.R**: This script requires the analyst to set the value of the `imputation` parameter at the top of the script to `TRUE` or `FALSE`. When `imputation == TRUE`, the script creates output "cleaned\_data\_step\_5.Rdata". When `imputation == FALSE`, the script creates output "cleaned\_data\_step\_5\_no\_imputation.Rdata".

### main/2\_prepare\_for\_imputation

In selecting variables to include in the imputation, we followed recommendations from:

van Buuren, Stef Flexible Imputation of Missing Data. CRC Press, 2012. (particularly Chapter 5)

van Buuren, S., and Karin Groothuis-Oudshoorn. "mice: Multivariate imputation by chained equations in R." Journal of statistical software (2010): 1-68.

-   **1\_examine\_distributions\_pre\_imputation.R**: This script creates histograms of all variables to assess the need to transform variables included in the multiple imputation. The histograms are saved as .png files in the "1\_variable\_distributions" subdirectory. We visually examined the histograms to decide which variables to transform, and recorded our decisions in the file "1\_variable\_distributions/variable\_transformations.csv", which is called upon in the next script.
-   **2\_get\_corr.sh**: This script cannot be run as-is, but is included for documentary purposes. It was used to submit the script "2\_get\_corr\_r2\_cramers\_v\_by\_decade\_array.R" to a cluster job runner in order to run it separately on each decade of our data.
-   **2\_get\_corr\_r2\_cramers\_v\_by\_decade\_array.R**: This script was written to be run as part of a cluster job that would operate on each decade of data in parallel, but can be run locally by setting the `NAI` parameter at the top of the file to a value between 1 and 5, corresponding to the five decades between 1970 and 2010, or by supplying the value as a trailing argument if running the script from the command line. Also note that this script may print error messages, but as long as execution does not stop, these errors have been handled (with the `try` function) and can be safely ignored.
-   **3\_create\_pred\_matrix\_by\_decade.R**: Creates output "3\_pred\_matrix\_list.Rdata".

### main/3\_multiply\_impute

-   **1\_full\_imputation\_by\_year\_array\_with\_ppc\_and\_inf\_check.R**: This script was written to be run as part of a cluster job that would operate on each decade of data in parallel, but can be run locally by setting the `NAI` parameter at the top of the file to a value between 1 and 5, corresponding to the five decades between 1970 and 2010, or by supplying the value as a trailing argument if running the script from the command line. For each decade, this script creates 10 imputed datasets, and saves a file containing each of 10 iterations along the way so that not all progress is lost if the script is interrupted, in the subdirectory "1\_imp\_iterations". If the script is interrupted, you can simply run it again and it will pick up where it left off. The final imputation results will all be contained in "imp\_YEAR\_10.Rdata", the file for the 10th iteration. The script also creates diagnostic output in the "output" subdirectory, and in the case of an execution error, saves degugging information in the "1\_error\_dump" subdirectory. This is a memory- and computationally-intensive step in the analysis. Creating the 10 imputed datasets for 1970, which has the fewest observations and the least amount of missingness, required about 6 GB of RAM and took around 25 hours. Creating the imputed datasets for 2010 required about 14 GB of RAM and took over two weeks. This long run time is mostly due to our use of a two-stage imputation process for variables with a high rate of zero values (which includes many of our income variables), where the first stage is a computationally-intensive logistic regression.
-   **2\_assess\_extreme\_imputed\_values.R**: Creates five files with filenames of the form "2\_extreme\_values\_YEAR.csv", which contain information on extreme values imputed for topcoded values of income component variables. This output was used to help set an upper threshold for imputed values, which is implemented in the next script.
-   **3\_enforce\_upper\_thresholds\_on\_imputed\_values.R**: This script compresses the distribution of imputed, topcoded income component values for variables with extreme imputed values so that all such values fall below a defined threshold. It produces five files with filenames of the form "3\_imp\_YEAR\_10\_extreme\_values\_transposed.Rdata", as well as diagnostic files of the form "3\_extreme\_values\_transposition\_report\_YEAR.csv".

### main/4a\_estimate\_taxes\_and\_transfers\_imputed

-   **1\_prepare\_taxsim\_input\_files.R**: Creates subdirectory "1\_taxsim\_input" and files in that subdirectory for each imputed dataset for each decade, with filenames of the form "srYEAR\_IMPNUM" (e.g., "sr1970\_1").
-   **2\_check\_dependent\_counts.R**: This script checks for a particular problem we encountered on early TAXSIM runs to make sure that our fix worked. It produces no output. Instead, it prints a message to the console indicating whether the problem is fixed.
-   **3\_upload\_taxsim\_input.R**: This script uploads the files created in step 1 to the TAXSIM ftp server at taxsimftp.nber.org. If the script isn't working for unclear reasons, make sure your firewall settings allow ftp see <http://users.nber.org/~taxsim/ftp-problems.html>. Also, for more info on the TAXSIM ftp service, see <https://users.nber.org/~taxsim/taxsim9/taxsim-ftp.html>.
-   **4\_download\_taxsim\_output.R**: This script downloads the TAXSIM output, saving the output files in the "4\_taxsim\_output" directory. The TAXSIM calculations are performed during file download, so this script can be run immediately after your input files are uploaded. Note that the TAXSIM ftp server deletes old files daily, so this script must be run within one day of uploading a given file.
-   **5\_check\_taxsim\_input\_output.r**: This script simply checks that all the TAXSIM output files have the same number of lines as the corresponding input files.
-   **6\_label\_taxsim\_output\_and\_merge\_imputed.r**: This script creates five output files, one for each decade, with filenames of the form "6\_imp\_post\_tax\_YEAR.Rdata".

### main/4b\_estimate\_taxes\_and\_transfers\_non\_imputed

-   **1\_prepare\_taxsim\_input\_files.R**: Creates subdirectory "1\_taxsim\_input" and one output file ("sr1970\_2010") containing all the TAXSIM input information.
-   **2\_check\_dependent\_counts.R**: This script checks for a particular problem we encountered on early TAXSIM runs to make sure that our fix worked. It produces no output. Instead, it prints a message to the console indicating whether the problem is fixed.
-   **3\_upload\_taxsim\_input.R**: This script uploads the file created in step 1 to the TAXSIM ftp server at taxsimftp.nber.org. If the script isn't working for unclear reasons, make sure your firewall settings allow ftp see <http://users.nber.org/~taxsim/ftp-problems.html>. Also, for more info on the TAXSIM ftp service, see <https://users.nber.org/~taxsim/taxsim9/taxsim-ftp.html>.
-   **4\_download\_taxsim\_output.R**: This script downloads the TAXSIM output, saving the output file in the "4\_taxsim\_output" directory. The TAXSIM calculations are performed during file download, so this script can be run immediately after your input files are uploaded. Note that the TAXSIM ftp server deletes old files daily, so this script must be run within one day of uploading a given file.
-   **5\_check\_taxsim\_input\_output.r**: This script simply checks that all the TAXSIM output files have the same number of lines as the corresponding input files.
-   **6\_label\_taxsim\_output\_and\_merge\_non\_imputed.r**: This script creates the file "6\_non\_imp\_data\_post\_tax.Rdata".

### main/5\_make\_no\_cohab\_datasets

-   **1\_make\_no\_cohab\_datasets.R**: This script creates five output files, one for each decade of imputed data, with filenames of the form "1\_imp\_YEAR\_post\_tax\_no\_cohab.Rdata".

### main/6a\_make\_analysis\_dataset\_imputed

-   **1\_make\_analysis\_dataset\_imputed.R**: This script creates five output files, one for each decade of imputed data, with filenames of the form "1\_imps\_1970\_analysis\_vars.Rdata".

### main/6b\_make\_analysis\_dataset\_non\_imputed

-   **1\_make\_analysis\_dataset\_non\_imputed.R**: This script creates one output file named "1\_non\_imputed\_analysis\_vars.Rdata".

### main/6c\_make\_analysis\_dataset\_no\_cohab

-   **1\_make\_analysis\_dataset\_no\_cohab.R**: This script creates five output files, one for each decade of imputed data, with filenames of the form "1\_imps\_1970\_analysis\_vars.Rdata".

### main/7\_make\_exclusion\_flags

-   **1\_make\_top\_two\_pct\_flags.R**: This script creates two output files named
    "1\_top\_2\_pct\_flag\_non\_imp.Rdata" and "1\_top\_2\_pct\_flag\_imp.Rdata".

-   **2\_make\_exclude\_alloc\_flag.R**: This script creates one output file named "2\_exclude\_alloc\_flag.Rdata".

### main/8a\_perform\_decomposition\_imputed

-   **1\_make\_decomp\_component\_tables.R**: This script allows you to adjust some options by setting the following variables near the top of the file:
    -   `fam_adj`: Should family income be adjusted for family size?
    -   `exclude_alloc`: Should all cases where the focal person's or their spouse's labor earnings were allocated or imputed be excluded from the analysis?
    -   `exclude_top_2_pct`: Should all families in the top 2% of family income be excluded from the analysis?
    -   `exclude_top_decile_female_earners`: Should all families containing a top decile female earner be excluded?
    -   `exclude_top_decile_male_earners`: Should all families containing a top decile male earner be excluded? In our main analysis results, only `fam_adj` is set to `TRUE`. All other options were only used for sensitivity analyses. This script produces ten output files, one for each imputed dataset, in the subdirectory "1\_decomp\_component\_tables", with filenames of the form "decomp\_components\_imputed\_SUFFIX\_IMPNUM.csv", where SUFFIX indicates the options that were set to `TRUE`.
-   **2\_make\_qois\_for\_tables\_and\_figs.R**: This script allows you to adjust the same options described above, and produces five output files in the "2\_qois\_for\_tables\_and\_figs" subdirectory, with filenames of the form "qoi\_imp\_YEAR\_SUFFIX.Rdata".
-   **3\_make\_figures\_1\_2\_and\_3.R**: This script allows you to adjust the same options described above, and produces three output files in the "3\_figures\_1\_2\_and\_3" directory, with filenames "figure\_1\_SUFFIX.csv", "figure\_2\_SUFFIX.csv", and "figure\_3\_SUFFIX.csv".

### main/8b\_perform\_decomposition\_non\_imputed

-   **1\_make\_decomp\_component\_tables.R**: This script allows you to adjust the options described above, and produces one output file, in the subdirectory "1\_decomp\_component\_tables", with filename "decomp\_components\_non\_imputed\_SUFFIX.csv", where SUFFIX indicates the options that were set to `TRUE`.
-   **2\_make\_qois\_for\_tables\_and\_figs.R**: This script allows you to adjust the same options described above, and produces five output files in the "2\_qois\_for\_tables\_and\_figs" subdirectory, with filenames of the form "qoi\_non\_imp\_YEAR\_SUFFIX.Rdata".
-   **3\_make\_figures\_1\_2\_and\_3.R**: This script allows you to adjust the same options described above, and produces three output files in the "3\_figures\_1\_2\_and\_3" directory, with filenames "figure\_1\_SUFFIX.csv", "figure\_2\_SUFFIX.csv", and "figure\_3\_SUFFIX.csv".

### main/8c\_perform\_decomposition\_no\_cohab

-   **1\_make\_decomp\_component\_tables.R**: This script allows you to adjust the options described above, and produces ten output files, one for each imputed dataset, in the subdirectory "1\_decomp\_component\_tables", with filenames of the form "decomp\_components\_imputed\_SUFFIX\_IMPNUM.csv", where SUFFIX indicates the options that were set to `TRUE`.
-   **2\_make\_qois\_for\_tables\_and\_figs.R**: This script allows you to adjust the same options described above, and produces five output files in the "2\_qois\_for\_tables\_and\_figs" subdirectory, with filenames of the form "qoi\_imp\_YEAR\_SUFFIX.Rdata".
-   **3\_make\_figures\_1\_2\_and\_3.R**: This script allows you to adjust the same options described above, and produces three output files in the "3\_figures\_1\_2\_and\_3" directory, with filenames "figure\_1\_SUFFIX.csv", "figure\_2\_SUFFIX.csv", and "figure\_3\_SUFFIX.csv".

### main/9\_make\_tables

-   **1\_make\_tables.R**: This script allows you to adjust the same options described above, and produces one output file, "1\_tabs\_list.Rdata", which contains the figures contained in all tables in the paper.

-   **2\_bootstrap\_loop\_tables.R**: This script allows you to adjust the same options described above, and produces two output files, "2\_all\_bootstrap\_decomp\_tables.Rdata" and "2\_tabs\_list.Rdata". The file "2\_all\_bootstrap\_decomp\_tables.Rdata" just contains intermediate output in case the script hits an error or is interrupted. The file "2\_tabs\_list.Rdata" contains the figures needed to compute bootstrapped uncertainty intervals for all tables in the paper

# Code for tables
# (taking 10 imputations and averaging over them)
# (based on file called "prelim_analysis_july2017.R")
# Goal = loop over bootstrap samples, 
#        create set of tables from each sample,
#        and append them into list of tables
#        so that this list can be used to extract measures of 
#        uncertainty around key point estimates
# September 2017

###
# R packages
###

library(data.table)
library(Hmisc)
library(weights)

fam_adj <- TRUE
exclude_alloc <- FALSE
exclude_top_2_pct <- FALSE
exclude_top_decile_female_earners <- FALSE
exclude_top_decile_male_earners <- FALSE


###
# Directories and files
###

DATA_DIR <- "main/6a_make_analysis_dataset_imputed"
DATA_FILE_PATTERN <- "1_imps_%d_analysis_vars.Rdata" # "%d" is placeholder for year
OUT_DIR <- "main/9_make_tables"
INTERMEDIATE_OUT_FILE <- "2_all_bootstrap_decomp_tables.Rdata"
OUT_FILE <- "2_bootstrap_tabs_list.Rdata"


###
# Parameters
###

NBOOT <- 1000 # set number of bootstrap samples


### 
# Functions
# (run outside the bootstrap loop)
### 


## Derek's function to create decomp components
source("functions/make_decomp_component_table.R")







###
# Read in data, create list of tables,
# and save this list
# (requires looping over bootstrap samples)
###

## Load list of decomp components if it already exists
decomp_component_file <- file.path(OUT_DIR, INTERMEDIATE_OUT_FILE)
if (file.exists(decomp_component_file)) {
    load(decomp_component_file)
} else {
    ## Create list of decomp components
    # Data is saved by decade, but we will combine results across
    # decades by imputation index (1-10) and bootstrap index (1-NBOOT)
    # to yield NBOOT sets of decomp components
    
    decomp_component_list <- vector(mode = "list", length = 10)
    
    for(i in 1:10) {
        decomp_component_list[[i]] <- vector(mode = "list", length = NBOOT)
    }
    
    for(yr in c(1970, 1980, 1990, 2000, 2010)) {
        data_file <- sprintf(DATA_FILE_PATTERN, yr)
        load( # loads list of imputed data.frames named "imps"
            file.path(
                DATA_DIR, 
                data_file
            )
        )
        
        cat(paste0(yr, "\n####\n\n"))
        
        for(imp_i in 1:10) {
            
            cat(paste0("Imputation ", imp_i, ", bootstrap: "))
            
            for(sample_i in 1:NBOOT) {
                cat(sample_i, "")
                bsamp <- sample(
                    1:nrow(imps[[imp_i]]), 
                    nrow(imps[[imp_i]]), 
                    replace = TRUE
                )
                tbl <- make_decomp_component_table(
                    imps[[imp_i]][bsamp, ], 
                    fam_adj = fam_adj, 
                    exclude_alloc = exclude_alloc, 
                    exclude_top_2_pct = exclude_top_2_pct, 
                    exclude_top_decile_female_earners = exclude_top_decile_female_earners, 
                    exclude_top_decile_male_earners = exclude_top_decile_male_earners
                )
                decomp_component_list[[imp_i]][[sample_i]] <- 
                    # Row-bind results across decades by imputation index (imp_i)
                    # and bootstrap index (sample_i)
                    rbindlist(
                        list(
                            decomp_component_list[[imp_i]][[sample_i]],
                            tbl
                        )
                    )
            }
            
            cat("\n\n")
            
        }
        
    }
    
    # Save decomp_component_list in case we need it and don't want to regenerate it
    save(
        decomp_component_list,
        file = decomp_component_file
    )
}




## Initiate lists of tables 
## and run loop to populate the list
#
# (initiate list of tables)
tabs.list <- vector(mode = "list", length = NBOOT)
#
# (run loop)

cat("Making tables...\n################\n")

for(i in 1:NBOOT) {
	
    cat(i, "")
    
    ## Data 
    # (tables are in "decomp_component_list" created above) 
    # 
    
    dat.decomp.imp.list <- lapply(
        decomp_component_list,
        function(x) {
            x[[i]]
        }
    )
    
    # (prep the data using function that replaces some NAs with zeros and separates men/women)
    dat.decomp.list <- lapply(
        dat.decomp.imp.list, 
        fun.data.prep
    )

    # (further prep the data by applying function that 
    #  decomposes change in self-relience to get decomposition output for each imputation)
    out.decomp.list <- lapply(
        dat.decomp.list, 
        fun.sr.decomp.5piece.apply
    )

    
	## Tables
	#
	# (apply function to get all the tables for a given bootstrap sample)
    tabs.boot <- do.call(
        fun.tabs.list, 
        c(
            out.decomp.list,
            dat.decomp.list
        ) 
    )

	# (append all tables to list of tables from all bootstrap samples)
	tabs.list[[i]] <- tabs.boot
	
}

         
## Save list of tables
save(tabs.list,file=file.path(OUT_DIR, OUT_FILE))




                            

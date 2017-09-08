# Steps to produce output from non imputed data as of 08/04/2017

setwd("C:/Users/Derek/OneDrive/Work/@Leslie McCall/Self_Reliance_Project")

# Options: family adjust or not, include cases with allocated labor earnings or not, exclude top 2 pct or not, 
# exclude top decile of male earners or not

# fam_adj exclude_alloc exclude_top_2

scripts <- c("get quantities of interest for deirdre's two new decompositions non imputed data.R", 
	"get_numbers_for_preliminary_tables_and_figs_no_imputation.R", 
	"figure_1_no_imputation_w_allocated.R", 
	"figure_2_no_imputation_w_allocated_fam_adj_exclude_top_decile_female_earners.R") #, 
	#"copy_output_to_box.R")

for(s in scripts) {
    for(fa in c(TRUE)) {
    	for(ea in c(FALSE)) {
    		for(et2 in c(FALSE)) { #, TRUE)) {
    		    for(etfe in c(TRUE)) { #, FALSE)) {
    		        rgs <- ""
    		        if(fa) rgs <- paste0(rgs, "fam_adj")
    		        if(ea) rgs <- paste0(rgs, " exclude_alloc")
    		        if(et2) rgs <- paste0(rgs, " exclude_top_2")
    		        if(etfe) rgs <- paste0(rgs, " exclude_top_decile_female_earners")
    		        cmd <- sprintf('Rscript "scripts/%s" %s', s, rgs)
    		        system(cmd)
    		    }
    		}
    	}
    }
}
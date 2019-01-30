# Fix sqrt_famsize
library(magrittr)
library(dplyr)
library(data.table)

dss <- function(text, subs) {
    split = strsplit(text, "%s")[[1]]
    if(grepl("%s$", text)) split <- c(split, "")
    if (length(split) - 1 != length(subs)) {
        stop("Number of wildcard characters does not equal number of variables to substitute.")
    }
    return(paste0(split, c(subs, ""), collapse=""))
}

do_string <- function(x) {
    eval(parse(text=x), envir=parent.frame())
}

load("main/1_clean_data/cleaned_data_step_5.Rdata")

# Split up cohabitors
split_cohab <- function(data) {
    
    # Set marr_cohab to "Not married or cohabiting" for cohabitors
    data[cohabp1 | cohabp2, marr_cohab := "Not married or cohabiting"]
    
    # Create pnloc as copy of sploc, so cohabs will have 0
    data[ , pnloc := sploc]
    
    ###########################################################################
    # Repeat assignment of subfamid from clean_data_step_3.R ##################
    ###########################################################################
    
    # 1. First, flag single parents (not married or cohabiting but nchild > 0).
    data[ , single_parent := marr_cohab=="Not married or cohabiting" & nchild != "0 children present"]
    
    
    # 2. Create child flags for anyone with (momloc > 0 | poploc > 0) & 
    #    marr_cohab=="Not married or cohabiting" & nchild=="0 children present"
    data[ , child_w_mom := momloc > 0 & poploc==0 & marr_cohab=="Not married or cohabiting" & nchild=="0 children present"]
    data[ , child_w_pop := momloc==0 & poploc > 0 & marr_cohab=="Not married or cohabiting" & nchild=="0 children present"]
    data[ , child_w_both := momloc > 0 & poploc > 0 & marr_cohab=="Not married or cohabiting" & nchild=="0 children present"]
    
    # 3. Create a subfamid variable that is:
    #    - the smaller of pnloc and pernum for marr_cohab people;
    data[ , subfamid := NULL]
    data[marr_cohab=="Married or cohabiting", subfamid := mapply(min, pnloc, pernum)]
    
    #    - pernum for single parents;
    data[single_parent==TRUE, subfamid := pernum]
    
    #    - pernum for unmarried head/householders with no children present;
    data[relate=="Head/householder" & marr_cohab=="Not married or cohabiting" & nchild=="0 children present", subfamid := pernum]
    
    #    - subfamid of mom for children with only mom present, subfamid of pop for children with only pop present, and same subfamid of both parents if both are present;
    get_subfamid <- function(subset, pernums) {
        do_string(dss("xx <- data[%s, list(year, serial, %s)]", c(subset, pernums)))
        setnames(xx, pernums, "pernum")
        setkey(xx, year, serial, pernum)
        setkey(data, year, serial, pernum)
        data[xx][ , subfamid]
    }
    
    data[child_w_mom==TRUE, subfamid := get_subfamid("child_w_mom==TRUE", "momloc")]
    data[child_w_pop==TRUE, subfamid := get_subfamid("child_w_pop==TRUE", "poploc")]
    
    data[child_w_both==TRUE, mom_subfamid := get_subfamid("child_w_both==TRUE", "momloc")]
    data[child_w_both==TRUE, pop_subfamid := get_subfamid("child_w_both==TRUE", "poploc")]
    # table(data[child_w_both==TRUE, mom_subfamid==pop_subfamid])
    # The preceding table shows that the mom and dad of all children with both present
    # have the same subfamid value; so we can just assign that of mom.
    data[child_w_both==TRUE, subfamid := mom_subfamid]
    data[ , mom_subfamid := NULL]
    data[ , pop_subfamid := NULL]
    
    rm(get_subfamid)
    
    #    - subfamid of the head for relatives of the head who aren't marr_cohab, single parents, or children;
    data[ , relative_or_foster_child_of_head := relate %in% c("Head/householder", "Spouse", "Child", 
                                                             "Stepchild", "Parent", "Sibling", 
                                                             "Grandchild", "Other relatives, n.s",
                                                             "Foster children")]
    setkey(data, year, serial)
    data[ , subfamid_of_head := subfamid[which(relate=="Head/householder")], by=key(data)]
    data[relative_or_foster_child_of_head==TRUE & marr_cohab=="Not married or cohabiting" & 
            single_parent==FALSE & momloc==0 & poploc==0, subfamid := subfamid_of_head]
    
    #    - and own pernum for those who aren't marr_cohab, single parents, children, or relatives of the head.
    data[relative_or_foster_child_of_head==FALSE & marr_cohab=="Not married or cohabiting" &
            single_parent==FALSE & momloc==0 & poploc==0, subfamid := pernum]
    
    # Count families and fam_heads
    # table(data[ , pernum==subfamid])
    # setkey(data, year, serial, subfamid)
    # nrow(data[ , .(npersons = .N), by = key(data)])
    
    # Fix sqrt_famsize again
    # data[ , sqrt_famsize := NULL]
    # setkey(data, year, serial, subfamid)
    # data[ , sqrt_famsize := sqrt(.N), by = key(data)]
    
    data
}

fixed_sqrt_famsize <- d %>%
    split_cohab() %>%
    group_by(year, serial, subfamid) %>%
    mutate(sqrt_famsize = sqrt(n())) %>%
    ungroup() %>%
    select(year, serial, pernum, marr_cohab, pnloc, subfamid, sqrt_famsize)
rm(d)

for(yr in seq(1970, 2010, by=10)) {
    cat(yr, " ")
    load(file.path("main/4a_estimate_taxes_and_transfers_imputed", 
                   sprintf("6_imp_post_tax_%d.Rdata", yr)))
    orig_names <- names(imp$data)
    orig_names[orig_names == "sqrt_famsize"] <- "sqrt_hh_size"
    imp$data <- imp$data %>%
        rename(sqrt_hh_size = sqrt_famsize) %>%
        select(-marr_cohab) %>%
        left_join(fixed_sqrt_famsize, by=c("year", "serial", "pernum")) %>%
        select_at(c(orig_names, "sqrt_famsize", "pnloc", "subfamid"))
    names(imp$imp)[names(imp$imp) == "sqrt_famsize"] <- "sqrt_hh_size"
    imp$imp <- c(imp$imp, list(sqrt_famsize = NULL, pnloc = NULL, 
                               subfamid = NULL))
    save(imp, file=sprintf("main/5_fix_pre_imputation_problems/2_imp_%d_post_tax_fixed_no_cohab.Rdata", yr))
}

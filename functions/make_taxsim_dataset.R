make_taxsim_dataset <- function(data) {

    pexemp <- c("1968"=600, "1969"=600, "1970"=625, "1978"=750, "1979"=1000, "1980"=1000, 
                "1988"=1950, "1989"=2000, "1990"=2050, "1998"=2700, 
                "1999"=2750, "2000"=2800, "2008"=3500, "2009"=3650, 
                "2010"=3750)
    
    # Create subfamid, ignoring cohabitors

    # 1. First, flag anyone who is married and flag
    #    single parents (not married but nchild > 0).
    data[ , married := marst=="Married, spouse present"]
    data[ , single_parent := !married & nchild != "0 children present"]

    # 2. Create child flags for anyone with (momloc > 0 | poploc > 0) & 
    #    married=="Not married" & nchild=="0 children present"
    data[ , child_w_mom := momloc > 0 & poploc==0 & !married & nchild=="0 children present"]
    data[ , child_w_pop := momloc==0 & poploc > 0 & !married & nchild=="0 children present"]
    data[ , child_w_both := momloc > 0 & poploc > 0 & !married & nchild=="0 children present"]

    # 3. Create a pnloc variable that equals sploc for married people and 
    #    equals the pernum of cohabp2 for cohabp1 and vice versa.
    # data[ , pnloc := ifelse(marst=="Married, spouse present", sploc, 0L)]
    # setkey(data, year, serial)
    # data[ , cohabp1_pernum := pernum[cohabp1], by=key(data)]
    # data[ , cohabp2_pernum := pernum[cohabp2], by=key(data)]
    # data[cohabp1==TRUE, pnloc := cohabp2_pernum]
    # data[cohabp2==TRUE, pnloc := cohabp1_pernum]
    # data[ , cohabp1_pernum := NULL]
    # data[ , cohabp2_pernum := NULL]
    # # table(data[married=="Married", pnloc])

    # 4. Create a subfamid variable that is:
    #    - the smaller of sploc and pernum for married people;
    data[married==TRUE, subfamid := mapply(min, sploc, pernum)]

    #    - pernum for single parents;
    data[single_parent==TRUE, subfamid := pernum]

    #    - pernum for unmarried head/householders with no children present;
    data[relate=="Head/householder" & married==FALSE & nchild=="0 children present", subfamid := pernum]

    #    - subfamid of mom for children with only mom present, subfamid of pop for children with only pop present, and same subfamid of both parents if both are present;
	child_w_mom <- data[child_w_mom==TRUE, list(pernum=momloc, year, serial)]
	setkey(child_w_mom, year, serial, pernum)
	setkey(data, year, serial, pernum)
	child_w_mom_subfamid <- data[child_w_mom][ , subfamid]
	data[child_w_mom==TRUE, subfamid := child_w_mom_subfamid]
	rm(child_w_mom, child_w_mom_subfamid)

	child_w_pop <- data[child_w_pop==TRUE, list(pernum=poploc, year, serial)]
	setkey(child_w_pop, year, serial, pernum)
	setkey(data, year, serial, pernum)
	child_w_pop_subfamid <- data[child_w_pop][ , subfamid]
	data[child_w_pop==TRUE, subfamid := child_w_pop_subfamid]
	rm(child_w_pop, child_w_pop_subfamid)

	child_w_both <- data[child_w_both==TRUE, list(pernum=momloc, year, serial)]
	setkey(child_w_both, year, serial, pernum)
	setkey(data, year, serial, pernum)
	child_w_both_subfamid <- data[child_w_both][ , subfamid]
	data[child_w_both==TRUE, mom_subfamid := child_w_both_subfamid]
	rm(child_w_both, child_w_both_subfamid)

	child_w_both <- data[child_w_both==TRUE, list(pernum=poploc, year, serial)]
	setkey(child_w_both, year, serial, pernum)
	setkey(data, year, serial, pernum)
	child_w_both_subfamid <- data[child_w_both][ , subfamid]
	data[child_w_both==TRUE, pop_subfamid := child_w_both_subfamid]
	rm(child_w_both, child_w_both_subfamid)

    # table(data[child_w_both==TRUE, mom_subfamid==pop_subfamid])
    # The preceding table shows that the mom and dad of all children with both present
    # have the same subfamid value; so we can just assign that of mom.
    data[child_w_both==TRUE, subfamid := mom_subfamid]
    data[ , mom_subfamid := NULL]
    data[ , pop_subfamid := NULL]

    #    - subfamid of the head for relatives of the head who aren't married, single parents, or children;
    data[ , relative_or_foster_child_of_head := relate %in% c("Head/householder", "Spouse", "Child", 
                                                           "Stepchild", "Parent", "Sibling", 
                                                           "Grandchild", "Other relatives, n.s",
                                                           "Foster children")]
    setkey(data, year, serial)
    data[ , subfamid_of_head := subfamid[relate=="Head/householder"], by=key(data)]
    data[relative_or_foster_child_of_head==TRUE & married==FALSE & 
          single_parent==FALSE & momloc==0 & poploc==0, subfamid := subfamid_of_head]

    #    - and own pernum for those who aren't married, single parents, children, or relatives of the head.
    data[relative_or_foster_child_of_head==FALSE & married==FALSE &
          single_parent==FALSE & momloc==0 & poploc==0, subfamid := pernum]


    # Un-adjust income vars for inflation
    income_vars <- c("eitcred", "fedtax", "adjginc", "taxinc", 
                     "fedtaxac", "capgain", "caploss", "statetax", "stataxac", 
                     "incdivid", "incint", "incrent", "incother", "incalim", "incasist", 
                     "incss", "incwelfr", "incwkcom", "incvet", "incchild", "incunemp", 
                     "inceduc", "incssi", "incwage", "incbus", "incfarm", "incsurv", 
                     "incdisab", "incretir", "proptax", "incdrt", "incgov", "incidr", 
                     "incaloth")
    
    for(n in income_vars) {
        do_string(dss("data[ , %s := as.integer(round(pce*%s/100))]", c(n, n)))
    }
    
    
    # Set negative imputed values to zero for items that don't allow
    # negative values (I judged this based on whether there were any negative
    # values in the observed data).
    no_negatives <- c("capgain", "caploss", "incdivid", "incint", "incother",
                      "incalim", "incasist", "incss", "incwelfr", "incwkcom",
                      "incvet", "incchild", "incunemp", "inceduc", "incssi", 
                      "incwage", "incsurv", "incdisab", "incretir", "incdrt", 
                      "incgov", "incaloth")
    
    for(n in no_negatives) {
        do_string(dss("data[%s < 0, %s := 0L]", c(n, n)))
    }
    
    
    # Now, identify tax units and dependents

    # Adjust filing thresholds for inflation

    pce1992 <- 71.494

    se <- 400*100/pce1992
    wage <- 1000*100/pce1992
    unearned <- 1000*100/pce1992

    data[ , se_th := se*pce/100]
    data[ , wage_th := wage*pce/100]
    data[ , unearned_th := unearned*pce/100]

    data[ , tax_se := incbus + incfarm]
    data[ , tax_wage := incwage]
    data[ , tax_unearned := as.integer(NA)]
    data[year %in% 1969:1971, tax_unearned := incidr + incaloth]
    data[year %in% 1979:1981, tax_unearned := incretir + incdrt + incint + incaloth]
    data[year %in% 1989:2011, tax_unearned := incint + incdivid + incrent + incalim + 
          incasist + incother]

    spouse <- data[sploc > 0, .(pernum=sploc, year, serial, sp_tax_se=tax_se, 
                             sp_tax_wage=tax_wage, sp_tax_unearned=tax_unearned)]
    setkey(spouse, year, serial, pernum)
    setkey(data, year, serial, pernum)

    data <- spouse[data]
    rm(spouse)

    data[ , total_tax_se := sum(tax_se, sp_tax_se, na.rm=TRUE), by=key(data)]
    data[ , total_tax_wage := sum(tax_wage, sp_tax_wage, na.rm=TRUE), by=key(data)]
    data[ , total_tax_unearned := sum(tax_unearned, sp_tax_unearned, na.rm=TRUE), by=key(data)]

    data[ , filer := total_tax_se > se_th | total_tax_wage > wage_th | 
           total_tax_unearned > unearned_th]

    # A qualifying child is any child or relative under age 19, or under 24 and a 
    # full-time student, who is not a member of a related subfamily that contains
    # a tax unit itself (because then that tax unit would claim them). If a person 
    # is married, they are only a qualifying child if they do not file taxes
    # (filer == FALSE).

    data[ , subfam_contains_tax_unit := any(filer), by=.(year, serial, subfamid)]
    data[ , qual_child := (age < 19 | (age < 24 & schlcoll %in% 
                                       c("College or university full time", 
                                         "High school full time"))) & 
           (!married | !filer) & 
           !(relate %in% c("Head/householder", "Spouse"))]

    # Who can claim the qualifying child? Its own subfam head, or the household
    # head if there are no tax filers in its subfam and it is related to household
    # head.
    data[ , qual_child_pointer := ifelse(qual_child, subfamid, 0L)]
    data[qual_child==TRUE & subfam_contains_tax_unit==FALSE & 
          relative_or_foster_child_of_head==TRUE, 
      qual_child_pointer := subfamid_of_head]

    # A dependent is someone under 15 or with "taxable income" less than $1,000,
    # The problem is that I don't know what they mean by "taxable income", because
    # the amount of income that is taxable depends on dependency status. I think 
    # it must just mean a total of less than $1,000 in income from taxable sources,
    # because if they meant $1,000 more than the personal exemption plus the 
    # standard deduction, that threshold would be much higher than the tax 
    # filer threshold they use. Oh, and anyone under 24 who is a full-time 
    # student is automatically a dependent according to the algorithm.
    # Also, a married person can only be dependent if they do not file
    # taxes (filer == FALSE).

    data[ , dependent := !(relate %in% c("Head/householder", "Spouse")) & 
           (age %in% 1:14 | 
                 
                 ((!married & (tax_se + tax_wage + tax_unearned) < wage_th) | 
                      !filer) |
                 
                (age < 24 & schlcoll %in% c("College or university full time", 
                                            "High school full time")))]

    data[ , dependent_pointer := ifelse(dependent, subfamid, 0L)]

    data[dependent==TRUE & subfam_contains_tax_unit==FALSE & 
          relative_or_foster_child_of_head==TRUE, 
      dependent_pointer := subfamid_of_head]

    data[ , subfam_head_is_filer := filer[pernum==subfamid], by=.(year, serial, subfamid)]
    data[ , hh_head_is_filer := filer[pernum==subfamid_of_head], by=.(year, serial)]

    # Okay, so just disqualify dependents and qualifying children who can only
    # be claimed by non-filers
    data[ , qual_child := qual_child & (subfam_head_is_filer | 
                                         (hh_head_is_filer & relative_or_foster_child_of_head))]
    data[ , qual_child_pointer := ifelse(qual_child, qual_child_pointer, 0L)]

    data[ , dependent := dependent & (subfam_head_is_filer | 
                                       (hh_head_is_filer & relative_or_foster_child_of_head))]
    data[ , dependent_pointer := ifelse(dependent, dependent_pointer, 0L)]

    # Also, you can't claim a dependent if you yourself are dependent, 
    # and you can't claim a qualifying child for EITC if you are a qualifying
    # child. The household head and spouse have already been disqualified
    # from being a dependent or qual_child, so just need to worry about 
    # subfam heads.

    data[ , subfam_head_is_dependent := dependent[pernum==subfamid], by=.(year, serial, subfamid)]
    data[ , subfam_head_is_qual_child := qual_child[pernum==subfamid], by=.(year, serial, subfamid)]

    data[ , qual_child := qual_child & !subfam_head_is_qual_child]
    data[ , dependent := dependent & !subfam_head_is_dependent]

    data[ , qual_child_pointer := ifelse(qual_child, qual_child_pointer, 0L)]
    data[ , dependent_pointer := ifelse(dependent, dependent_pointer, 0L)]

    # Also, a qualifying child must be younger than the person claiming them
    # and that person's spouse, if married
    sp_age <- data[sploc > 0, .(sp_age=age, pernum=sploc, year, serial)]
    setkey(sp_age, year, serial, pernum)
    setkey(data, year, serial, pernum)
    data <- sp_age[data]
    rm(sp_age)

    data[!is.na(sp_age), max_sp_age := max(age, sp_age, na.rm=TRUE), by=.(year, serial, pernum)]
    data[is.na(sp_age) & (pernum==subfamid | pernum==subfamid_of_head), max_sp_age := age]
    data[ , max_sp_age_subfam := max_sp_age[pernum==subfamid], by=.(year, serial, subfamid)]
    data[ , max_sp_age_hh := max_sp_age[pernum==subfamid_of_head], by=.(year, serial)]

    data[ , qual_child := qual_child & ((qual_child_pointer==subfamid & age < max_sp_age_subfam) | 
      (qual_child_pointer==subfamid_of_head & age < max_sp_age_hh))]
    
    # You can't be a dependent or qualifying child of your spouse
    # (I found this for two cases where the spouse was age 14)
    data[ , qual_child := qual_child & (qual_child_pointer != sploc)]
    data[ , dependent := dependent & (dependent_pointer != sploc)]
    
    data[ , qual_child_pointer := ifelse(qual_child, qual_child_pointer, 0L)]
    data[ , dependent_pointer := ifelse(dependent, dependent_pointer, 0L)]

    # Now, create a variable for number of dependents and number of qual_child
    depx <- data[dependent==TRUE, .(pernum=dependent_pointer, year, serial)]
    depx <- depx[ , .(ndep=.N), by=.(year, serial, pernum)]
    setkey(depx, year, serial, pernum)
    setkey(data, year, serial, pernum)
    data <- depx[data]
    rm(depx)

    qc <- data[qual_child==TRUE, .(pernum=qual_child_pointer, year, serial)]
    qc <- qc[ , .(nqual_child=.N), by=.(year, serial, pernum)]
    setkey(qc, year, serial, pernum)
    data <- qc[data]
    rm(qc)

    # Also need vars for number of dependents under age 17
    dep17 <- data[dependent==TRUE & age < 17, .(pernum=dependent_pointer, year, serial)]
    dep17 <- dep17[ , .(ndep17=.N), by=.(year, serial, pernum)]
    setkey(dep17, year, serial, pernum)
    data <- dep17[data]
    rm(dep17)

	# Set all missing income components to 0
    
	for(n in income_vars) {
		do_string(dss("data[is.na(%s), %s := 0L]", c(n, n)))
	}
    
    
    
	# Aggregate all income vars across spouses
	sp_inc <- data[sploc > 0, c(income_vars, "year", "serial", "sploc"), with=FALSE]
	setnames(sp_inc, income_vars, paste0("sp_", income_vars))
	setnames(sp_inc, "sploc", "pernum")
	setkey(sp_inc, year, serial, pernum)
	setkey(data, year, serial, pernum)
	data <- sp_inc[data]
	rm(sp_inc)
	for(n in income_vars) {
		do_string(dss("data[is.na(%s), %s := 0]", rep(paste0("sp_", n), 2)))
		do_string(dss("data[ , %s := sum(%s, %s), by=.(year, serial, pernum)]", 
			c(paste0("tu_", n), n, paste0("sp_", n))))
	}

	####################
	# TAXSIM variables #
	####################

	# x1 : case ID
	data[ , x1 := 0]

	# x2 : tax year
	data[ , x2 := year - 1]

	# x3 : state SOI code (0 if you don't want to calculate state taxes)
	data[ , x3 := 0]

	# x4 : marital status (1=single, 2=joint, 3=head of household, 8=dependent taxpayer)
	data[ , x4 := ifelse(married, 2L, 1L)]
	data[married==FALSE & dependent==TRUE, x4 := 8L]
	data[married==FALSE & ndep > 0, x4 := 3L]

	# x5 : number of dependent exemptions
	data[is.na(nqual_child), nqual_child := 0L]
	data[is.na(ndep), ndep := 0L]
	data[ , x5 := nqual_child]
	data[x5 > 15, x5 := 15L]

	# x6 : number of taxpayers over 65 years of age (in tax year)
	data[filer & !married, x6 := ifelse(age >= 65, 1L, 0L)]
	data[filer & married & age >= 65, x6 := ifelse(sp_age >= 65, 2L, 1L)]
	data[filer & married & age < 65, x6 := ifelse(sp_age >= 65, 1L, 0L)]

	# x7 : wage and salary income of taxpayer
	data[ , x7 := max(incwage + incbus + incfarm, 0), by=.(year, serial, pernum)]

	# x8 : wage and salary income of spouse
	data[ , x8 := max(sp_incwage + sp_incbus + sp_incfarm, 0), by=.(year, serial, pernum)]

	# x9 : dividend income
	data[ , x9 := max(tu_incdivid, 0), by=.(year, serial, pernum)]

	# x10 : other property income
	data[ , x10 := max(tu_incint + tu_incrent + tu_incother + tu_incalim + 
	               tu_incaloth + tu_incdrt + tu_incidr + tu_incasist, 0), by=.(year, serial, pernum)]

	# x11 : taxable pensions
	data[ , x11 := max(tu_incretir, 0), by=.(year, serial, pernum)]

	# x12 : gross social security benefits
	data[ , x12 := max(tu_incss, 0), by=.(year, serial, pernum)]

	# x13 : other non-taxable transfer income (welfare, workers comp, vet benefits, child support)
	data[ , x13 := tu_incwelfr + tu_incwkcom + tu_incvet + tu_incsurv + tu_incdisab + 
		           tu_incchild + tu_inceduc + tu_incgov + tu_incssi]

	# x14 : rent paid
	data[ , x14 := 0]

	# x15 : real estate taxes paid
	data[ , x15 := 0]

	# x16 : other itemized deductions
	data[ , x16 := max(c(0, tu_adjginc-sum(pexemp[as.character(x2)], 
		                 tu_proptax, tu_statetax, tu_taxinc))), 
					by=.(year, serial, pernum)]

	# x17 : child care expenses
	data[ , x17 := 0]

	# x18 : unemployment compensation received
	data[ , x18 := max(tu_incunemp, 0), by=.(year, serial, pernum)]

	# x19 : number of dependents under age 17
	data[is.na(ndep17), ndep17 := 0L]
	data[ , x19 := ndep17]
	data[x19 > 15, x19 := 15L]

	# x20 : deductions not included in item 16
	data[ , x20 := 0]

	# x21 : short term capital gains or losses
	data[ , x21 := 0]

	# x22 : long term capital gains or losses
	data[ , x22 := tu_capgain - tu_caploss]

	# Drop spouses and non-filers
	data <- data[filer==TRUE, ]
	data <- data[x4==1 | (x4==2 & pernum==subfamid), ] # keeps single filers

	# Assign tax unit ID
	data[ , x1 := 100*serial + pernum]

	# Drop other variables
	data <- data[ , paste0("x", 1:22), with=FALSE]

	return(data)
}
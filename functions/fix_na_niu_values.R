fix_na_niu_values <- function(data) {
    five_nines <- 99999
    six_nines <- 999999
    seven_nines <- 9999999
    eight_nines <- 99999999
    
    all_yrs <- c(1969:1971, 1979:1981, 1989:1991, 1999:2001, 2009:2011)
    all_five_nines <- list(years=all_yrs, values=five_nines)
    all_six_nines <- list(years=all_yrs, values=six_nines)
    
    na_niu_values <- list(
        ftotval=list(years=1969:1971, values=six_nines), 
        inctot=list(years=all_yrs, values=c(eight_nines, eight_nines-1)), 
        incwage=list(years=all_yrs, values=c(seven_nines, seven_nines-1)),
        incbus=list(years=all_yrs, values=c(seven_nines, seven_nines-1)),
        incfarm=list(years=all_yrs, values=c(seven_nines, seven_nines-1)),
        incss=all_five_nines,
        incwelfr=all_five_nines,
        incgov=all_five_nines,
        incidr=all_five_nines,
        incaloth=all_five_nines,
        incretir=all_six_nines,
        incssi=all_five_nines,
        incdrt=all_five_nines,
        incint=all_five_nines,
        incunemp=all_five_nines,
        incwkcom=all_five_nines,
        incvet=all_five_nines,
        incsurv=all_six_nines,
        incdisab=all_six_nines,
        incdivid=all_six_nines,
        incrent=all_five_nines,
        inceduc=all_five_nines,
        incchild=all_five_nines,
        incalim=all_five_nines,
        incasist=all_five_nines,
        incother=all_five_nines, 
        INCDISA1=all_five_nines,
        INCDISA2=all_five_nines,
        inclongj=all_six_nines,
        INCRETI1=all_five_nines,
        INCRETI2=all_five_nines,
        INCSURV1=all_five_nines,
        INCSURV2=all_five_nines,
        oincbus=all_six_nines,
        oincfarm=all_six_nines,
        oincwage=list(years=all_yrs, values=seven_nines),
        proptax=list(years=all_yrs, values=99997),
        adjginc=list(years=all_yrs, values=seven_nines),
        capgain=all_five_nines,
        caploss=all_five_nines,
        eitcred=list(years=all_yrs, values=9999),
        fedtax=all_six_nines,
        fedtaxac=all_six_nines,
        statetax=all_six_nines,
        stataxac=all_six_nines,
        taxinc=list(years=all_yrs, values=seven_nines),
        INCWAGE_SP=list(years=all_yrs, values=c(seven_nines, seven_nines-1))
    )

    for(varname in intersect(names(na_niu_values), names(data))) {
        nas <- na_niu_values[[varname]]
        attach(data)
        to_replace <- which(year %in% nas$years & data[[varname]] %in% nas$values)
        detach(data)
        data[[varname]][to_replace] <- rep(NA, length(to_replace))
    }
    return(data)
}
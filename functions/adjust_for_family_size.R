adjust_for_family_size <- function(data) {
    data[ , fam_inc := fam_inc / sqrt_famsize]
    data[ , labern := labern / sqrt_famsize]
    data[ , pn_labern := pn_labern / sqrt_famsize]
    data[ , other_inc := other_inc / sqrt_famsize]
    data[ , fam_unearned_non_gov := fam_unearned_non_gov / sqrt_famsize]
    data[ , fam_unearned_gov := fam_unearned_gov / sqrt_famsize]
    data[ , fam_tax := fam_tax / sqrt_famsize]
    data[ , oth_labern := oth_labern / sqrt_famsize]
    data
}
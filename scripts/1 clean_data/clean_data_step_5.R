# CLEANING TO PREPARE FOR MULTIPLE IMPUTATION OR NOT?

# imputation <- FALSE
imputation <- TRUE

if(imputation) {
    STEP_4_DATA = "Data/cleaned_data/cleaned_data_step_4.Rdata"
} else {
    STEP_4_DATA = "Data/cleaned_data/cleaned_data_step_4_no_imputation.Rdata"
}

if(!file.exists(STEP_4_DATA)) stop("Cannot perform step 5 of data cleaning without data from step 4.")

library(data.table)

load(STEP_4_DATA)

cat(paste0(format(Sys.time(), format="%H:%M:%S"), "    Changing character variables back to factors...\n"))
# Change character variables to factors
d[] <- lapply(d, function(x) { 
    if(is.character(x)) as.factor(x)
    else x
})

# Fix levels of sex
d[ , sex := as.factor(as.character(sex))]
d[ , pn_sex := as.factor(as.character(pn_sex))]

# Recode race
race_recodes <- c("American Indian-Asian" = "Other/mixed race",
                  "American Indian/Aleut/Eskimo" = "American Indian", 
                  "Asian-Hawaiian/Pacific Islander" = "Other/mixed race", 
                  "Asian only" = "Asian",
                  "Asian or Pacific Islander" = "Asian",
                  "Black-American Indian" = "Other/mixed race",
                  "Black-Asian" = "Other/mixed race", 
                  "Black-Hawaiian/Pacific Islander" = "Other/mixed race", 
                  "Black/Negro" = "African-American", 
                  "Four or five races, unspecified" = "Other/mixed race", 
                  "Hawaiian/Pacific Islander only" = "Native Hawaiian/Pacific Islander", 
                  "Other (single) race, n.e.c" = "Other/mixed race", 
                  "Two or three races, unspecified" = "Other/mixed race", 
                  "White" = "White", 
                  "White-American Indian" = "Other/mixed race", 
                  "White-American Indian-Asian" = "Other/mixed race", 
                  "White-Asian" = "Other/mixed race", 
                  "White-Asian-Hawaiian/Pacific Islander" = "Other/mixed race", 
                  "White-Black" = "Other/mixed race", 
                  "White-Black-American Indian" = "Other/mixed race", 
                  "White-Black-American Indian-Asian" = "Other/mixed race", 
                  "White-Black-Asian" = "Other/mixed race", 
                  "White-Hawaiian/Pacific Islander" = "Other/mixed race")

d[ , race := factor(race_recodes[as.character(race)])]

rm(race_recodes)

educ_recode <- c("1 year of college" = "Some college", 
                 "12th grade, diploma unclear" = "HS diploma", 
                 "12th grade, no diploma" = "Less than HS", 
                 "2 years of college" = "Two-year degree", 
                 "3 years of college" = "Some college", 
                 "4 years of college" = "Four-year degree", 
                 "5 years of college" = "Four-year degree", 
                 "6+ years of college" = "Graduate or professional degree", 
                 "Associate's degree, academic program" = "Two-year degree", 
                 "Associate's degree, occupational/vocational program" = "Two-year degree", 
                 "Bachelor's degree" = "Four-year degree", 
                 "Doctorate degree" = "Graduate or professional degree", 
                 "Grade 1" = "Less than HS", 
                 "Grade 10" = "Less than HS", 
                 "Grade 11" = "Less than HS", 
                 "Grade 2" = "Less than HS", 
                 "Grade 3" = "Less than HS", 
                 "Grade 4" = "Less than HS", 
                 "Grade 5" = "Less than HS", 
                 "Grade 6" = "Less than HS", 
                 "Grade 7" = "Less than HS", 
                 "Grade 8" = "Less than HS", 
                 "Grade 9" = "Less than HS", 
                 "Grades 1, 2, 3, or 4" = "Less than HS", 
                 "Grades 5 or 6" = "Less than HS", 
                 "Grades 7 or 8" = "Less than HS", 
                 "High school diploma or equivalent" = "HS diploma", 
                 "Master's degree" = "Graduate or professional degree", 
                 "Missing/Unknown" = NA, 
                 "NIU or blank" = "NIU", 
                 "None or preschool" = "Less than HS", 
                 "Professional school degree" = "Graduate or professional degree", 
                 "Some college but no degree" = "Some college")

d[ , educ := ordered(educ_recode[as.character(educ)], 
                     levels=c("Less than HS", "HS diploma", "Some college", "Two-year degree", 
                              "Four-year degree", "Graduate or professional degree"))]

d[ , wkswork2 := ordered(wkswork2, levels=c("niu", "1-13 weeks", "14-26 weeks", "27-39 weeks", "40-47 weeks", "48-49 weeks", "50-52 weeks"))]

rm(educ_recode)

hispan_recode <- c("Central/South American" = "Hispanic", 
                   "Chicano/Chicana" = "Hispanic", 
                   "Cuban" = "Hispanic", 
                   "Do not know" = "Hispanic", 
                   "Dominican" = "Hispanic", 
                   "Mexican" = "Hispanic", 
                   "Mexican (Mexicano)" = "Hispanic", 
                   "Mexican American" = "Hispanic", 
                   "Mexicano/Chicano" = "Hispanic", 
                   "Mexicano/Mexicana" = "Hispanic", 
                   "N/A (and no response 1985-87)" = NA, 
                   "Not Hispanic" = "Not Hispanic", 
                   "Puerto Rican" = "Hispanic")

d[ , hispan := factor(hispan_recode[as.character(hispan)])]

rm(hispan_recode)

# Add back incwelfr with allocated values to use as a predictor in the imputation
attach("Data/ipums_data_full/cps_master.Rdata")
master <- get("d", pos=2)[ , .(incwelfr), keyby=.(year, serial, pernum)]
source("functions/fix_na_niu_values.R")
master <- fix_na_niu_values(master)
setnames(master, old="incwelfr", new="incwelfr_alloc")
setkey(d, year, serial, pernum)
d[ , incwelfr_alloc := master$incwelfr_alloc]
rm(master)
d[ , incwelfr_alloc := 100*incwelfr_alloc/pce]


if(imputation) {
    save(d, file="Data/cleaned_data/cleaned_data_step_5.Rdata")
} else {
    save(d, file="Data/cleaned_data/cleaned_data_step_5_no_imputation.Rdata")
}


## modeling function, will help in future use

library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(forcats)
library(data.table)
library(feather)
library(caret)
library(rms)
library(pROC)
library(DescTools)
library(e1071)
library(quantreg)

# library(leaps)

library(biglm)



# identify the right indicator for coefficient extraction (row number) --> --> 35
  # dg <- results_df$feature[3]
  # form <- formula(paste(base_form_string, "+", dg))
  # fit <- glm(form, family = binomial(), data = (base_and_vars %>% sample_n(10000)))
  # coef(summary(fit))[,1][35]


development_set <- fread("/data/projects/project_pvartiai/rsv/predictors/development_set.csv") %>% 
  as_tibble



#### F U N C T I O N S ###

univar.baseadjusted.modeling <- function(base = development_set, vars, name_of_vars) {

# test some conditions
base_vars <- names(base)
base_features <- setdiff(base_vars, c("TNRO", "outcome"))
vars_features <- setdiff(names(vars), "TNRO")

if (sum(base_features %in% vars_features) < 0) {
  stop("base vars and features overlap")
}


if (!all(base$TNRO %in% vars$TNRO)) {
  stop("dataframes have different n:o of cases")
}

# counts and save them to results
setwd("/data/projects/project_pvartiai/rsv/results/featureselect")

vars %>%
  pivot_longer(cols = -TNRO) %>%
  filter(value == 1) %>%
  group_by(name) %>%
  summarise(n = n()) %>%
  write.csv(., paste0(name_of_vars, "_count.csv"))


###### names and formula object definition ######

# names and model object string

# continuous vars as a string vector, for creating a model object
base_cont_vars <- c("gest_days", 
					"dist", 
					"weight_sd", 
					"mother_age", 
					"father_age")

# categorical vars
base_cat_vars <- c("sib_0_4",
					"sib_4_7", 
					"sib_over7")

non_predicting_vars <- c("TNRO", "outcome", "outcome_3days", "height_sd", "outcome_duration", "birth_month", "LAPSEN_SYNTYMAPVM")

# binary vars
base_bin_vars <- setdiff(base_vars, c(base_cont_vars, base_cat_vars, non_predicting_vars))


# parts of the model object as strings
start_string <- "outcome ~"

# collapse variable name vectors to a single character string object 
# combined with "+"
base_cont_string <- paste0('rcs(', base_cont_vars, ', 4)+') %>% paste(., collapse = "")
base_cat_string <- paste(base_cat_vars, collapse = "+")
base_bin_string <- paste(base_bin_vars, collapse = "+")
base_other_string <- paste(c(base_cat_string, base_bin_string), collapse = "+")
base_allpred_string <- paste(c(base_cont_string, base_other_string), collapse = "")
base_form_string <- paste0(start_string, base_allpred_string, collapse = "")

#actual formula object
base_form <- formula(base_form_string)


# create a joined dataset with base vars and tested features
base_and_vars <- left_join(base, vars, by = "TNRO")

features <- setdiff(names(base_and_vars), base_vars)

if("TNRO" %in% features|"outcome" %in% features) {
  stop("problem in feature names")
}


###### modeling ######


results_df <- data.frame(feature = as.character(features), coef = NA, se = NA, p = NA)

# # identify the right indicator for coefficient extraction (row number [indicator]) 
  dg <- results_df$feature[3]
  form <- formula(paste(base_form_string, "+", dg))
  fit <- glm(form, family = binomial(), data = (base_and_vars %>% sample_n(10000)))
  indicator <- coef(summary(fit))[,1] %>% length() # 27
  # test
  coef(summary(fit))[,1][indicator]
  
  
for (i in 1:nrow(results_df)) {
    
    dg <- results_df$feature[i]
    
    #create formula for model
    form <- formula(paste(base_form_string, "+", dg))
    
    # the model
    fit <- glm(form, family = binomial(), data = base_and_vars)
    
    # dig the  coefficient, standard error, p-value
    # note that if the number of base predictors (or their categories) change, 
    # we need to change the indicator numbrer
    coef <- coef(summary(fit))[,1][indicator]
    se <- summary(fit)$coefficients[,2][indicator]
    p <- coef(summary(fit))[,4][indicator]
    
    results_df[i,2:4] <- c(coef, se, p)
    
    print(i)
    
  }
setwd("/data/projects/project_pvartiai/rsv/results/featureselect")
write.csv(results_df, paste0(name_of_vars, "_univar.csv"))

}





multivar.modeling <- function(base = development_set, vars, name_of_vars) {

###### names and formula object definition ######

# names and model object string
base_vars <- names(base)

# continuous vars as a string vector, for creating a model object
base_cont_vars <- c("gest_days", 
          "dist", 
          "weight_sd", 
          "mother_age", 
          "father_age")

# categorical vars
base_cat_vars <- c("sib_0_4",
          "sib_4_7", 
          "sib_over7")

non_predicting_vars <- c("TNRO", "outcome", "outcome_3days", "height_sd", "outcome_duration", "birth_month", "LAPSEN_SYNTYMAPVM")

# binary vars
base_bin_vars <- setdiff(base_vars, c(base_cont_vars, base_cat_vars, non_predicting_vars))

# parts of the model object as strings
start_string <- "outcome ~"

# collapse variable name vectors to a single character string object 
# combined with "+"
base_cont_string <- paste0('rcs(', base_cont_vars, ', 4)+') %>% paste(., collapse = "")
base_cat_string <- paste(base_cat_vars, collapse = "+")
base_bin_string <- paste(base_bin_vars, collapse = "+")
base_other_string <- paste(c(base_cat_string, base_bin_string), collapse = "+")
base_allpred_string <- paste(c(base_cont_string, base_other_string), collapse = "")
base_form_string <- paste0(start_string, base_allpred_string, collapse = "")

#actual formula object
base_form <- formula(base_form_string)


# create a joined dataset with base vars and tested features
base_and_vars <- left_join(base, vars, by = "TNRO")



features <- setdiff(names(base_and_vars), base_vars)

if("TNRO" %in% features|"outcome" %in% features) {
  stop("problem in feature names")
}

###### modeling, multivariate GLM ###############

feature_string <- paste(features, collapse = "+")
feature_multivar_string <- paste(base_form_string, "+", feature_string) 
feature_multivar_form <- formula(feature_multivar_string)

mbr_struct_fit <- glm(data = (base_and_vars),
          family = binomial(),
          formula = feature_multivar_form)

multivar_results <- data.frame(feature = rownames(coef(summary(mbr_struct_fit))), 
                              coef = NA, 
                              se = NA, 
                              p = NA)
multivar_results["coef"] <- (coef(summary(mbr_struct_fit))[,1] %>% as_tibble)
multivar_results["se"] <- coef(summary(mbr_struct_fit))[,2]
multivar_results["p"] <- coef(summary(mbr_struct_fit))[,4]


setwd("/data/projects/project_pvartiai/rsv/results/featureselect")
write.csv(multivar_results, paste0(name_of_vars, "_multivar.csv"))

print("yee!")

}



model.aic <- function(base = development_set, vars, name_of_vars) {

# names

###### names and formula object definition ######

# names and model object string
base_vars <- names(base)

# continuous vars as a string vector, for creating a model object
base_cont_vars <- c("gest_days", 
          "dist", 
          "weight_sd", 
          "mother_age", 
          "father_age")


# categorical vars
base_cat_vars <- c("sib_0_4",
          "sib_4_7", 
          "sib_over7")

non_predicting_vars <- c("TNRO", "outcome", "outcome_3days", "height_sd", "outcome_duration", "birth_month", "LAPSEN_SYNTYMAPVM")

# binary vars
base_bin_vars <- setdiff(base_vars, c(base_cont_vars, base_cat_vars, non_predicting_vars))


# parts of the model object as strings
start_string <- "outcome ~"

# collapse variable name vectors to a single character string object 
# combined with "+"
base_cont_string <- paste0('rcs(', base_cont_vars, ', 4)+') %>% paste(., collapse = "")
base_cat_string <- paste(base_cat_vars, collapse = "+")
base_bin_string <- paste(base_bin_vars, collapse = "+")
base_other_string <- paste(c(base_cat_string, base_bin_string), collapse = "+")
base_allpred_string <- paste(c(base_cont_string, base_other_string), collapse = "")
base_form_string <- paste0(start_string, base_allpred_string, collapse = "")

#actual formula object
base_form <- formula(base_form_string)


# create a joined dataset with base vars and tested features
base_and_vars <- left_join(base, vars, by = "TNRO")

features <- setdiff(names(base_and_vars), base_vars)
feature_string <- paste(features, collapse = "+")
feature_multivar_string <- paste(base_form_string, "+", feature_string) 
feature_multivar_form <- formula(feature_multivar_string)


if("TNRO" %in% features|"outcome" %in% features) {
  stop("problem in feature names")
}


###### modeling, AIC-based exclusion  ###############


# then, the actual model


multivar_fit2 <- Glm(data = (base_and_vars),
          family = binomial(),
          formula = feature_multivar_form)


weird_na_coefs <- coefficients(multivar_fit2)[is.na(coefficients(multivar_fit2))] %>% 
  names

if (length(weird_na_coefs > 0)) {

# new data without weird variables
base_and_vars2 <- base_and_vars %>%
  select(-all_of(weird_na_coefs))

# new form...

features2 <- setdiff(names(base_and_vars2), base_vars)
feature2_string <- paste(features2, collapse = "+")
feature2_multivar_string <- paste(base_form_string, "+", feature2_string) 
feature2_multivar_form <- formula(feature2_multivar_string)

# new fit without weird variables

multivar_fit2 <- Glm(data = (base_and_vars2),
          family = binomial(),
          formula = feature2_multivar_form)

  }

# fastbw!
aic_multivar <- fastbw(multivar_fit2)

# select retained feature names as vector,
# and create formula strings again
dropped_vars <- setdiff(names(base_and_vars), 
  c(aic_multivar$names.kept, "TNRO", "outcome"))

step2_cont_vars <- setdiff(base_cont_vars, dropped_vars)
step2_other_vars <- setdiff(aic_multivar$names.kept, step2_cont_vars)

if(length(step2_other_vars) == 0) {
  step2_cont_string <- paste0('rcs(', step2_cont_vars, ', 4)') %>% paste(., collapse = "+")
} else {
  step2_cont_string <- paste0('rcs(', step2_cont_vars, ', 4)+') %>% paste(., collapse = "")
}

step2_other_string <- paste(step2_other_vars, collapse = "+")

# create formula object
step2_form <- paste(start_string, step2_cont_string, step2_other_string) %>%
 formula


step2_fit <-  glm(data = (base_and_vars),
          family = binomial(),
          formula = step2_form)

# extract coefficients
aic_results <- data.frame(feature = rownames(coef(summary(step2_fit))), coef = NA, se = NA, p = NA)
aic_results["coef"] <- coef(summary(step2_fit))[,1]
aic_results["se"] <- coef(summary(step2_fit))[,2]
aic_results["p"] <- coef(summary(step2_fit))[,4]

setwd("/data/projects/project_pvartiai/rsv/results/featureselect/")
write.csv(aic_results, paste0(name_of_vars, "_aic_multivar.csv"))
print("yee")


}




#
#
#
#
#
#
#
#
#
#
#




## test the functions
# a <- development_set %>% sample_n(10000)
# b <- left_join(a, meds_sib_wide, by = "TNRO") 
# b <- b %>% select(-setdiff(colnames(a), "TNRO"))

# univar.baseadjusted.modeling(base = a, vars = b, name_of_vars = "test_ignore")
# multivar.modeling(base = a, vars = b, name_of_vars = "test_ignore")
# model.aic(base = a, vars = b, name_of_vars = "test_ignore")








# the first function tests that datasets are equal
# mbr_struct

# mbr_struct
mbr_structural <- fread("/data/projects/project_pvartiai/rsv/predictors/mbr_structural_vars.csv") %>%
  as_tibble() 

univar.baseadjusted.modeling(base = development_set, vars = mbr_structural, name_of_vars = "mbr_structural")
multivar.modeling(base = development_set, vars = mbr_structural, name_of_vars = "mbr_structural")
model.aic(base = development_set, vars = mbr_structural, name_of_vars = "mbr_structural")
rm(mbr_structural)
gc()
Sys.time()
#

# load all data

neonatal_dgs_pre <- fread("/data/projects/project_pvartiai/rsv/predictors/neonatal_diagnoses_feb_22.csv") %>% 
  as_tibble

mbr_structural <- fread("/data/projects/project_pvartiai/rsv/predictors/mbr_structural_vars.csv") %>%
  as_tibble() 

neonatal_dgs <- left_join(neonatal_dgs_pre, mbr_structural, by = "TNRO")

# fa_ep
fa_ep <- fread("/data/projects/project_pvartiai/rsv/predictors/father_endpoints_wide_15022022.csv") %>% 
  as_tibble %>% 
  # be sure to exclude this because of very small n
  select(-any_of(c("KRA_PSY_ANYMENTAL_SUICID_PREG_NERV_EXMORE")))

csums <- colSums(fa_ep[2:length(fa_ep)])
fa_ep <- fa_ep %>%
  select(-c(names(csums[csums<1500])))


# sib ep
sib_ep <- fread("/data/projects/project_pvartiai/rsv/predictors/sibling_endpoints_wide_15022022.csv") %>% 
  as_tibble
csums <- colSums(sib_ep[2:length(sib_ep)])
sib_ep <- sib_ep %>%
  select(-c(names(csums[csums<1500])))

# mo_ep
mo_ep <- fread("/data/projects/project_pvartiai/rsv/predictors/mother_endpoints_wide_15022022.csv") %>% 
  as_tibble

csums <- colSums(mo_ep[2:length(mo_ep)])

mo_ep <- mo_ep %>%
  select(-c(names(csums[csums<1500])))



### meds

meds_mother_beforepreg_wide <- fread("/data/projects/project_pvartiai/rsv/predictors/meds_mother_beforepreg_wide.csv") %>%
  as_tibble

meds_mother_pregnancy_wide <- fread("/data/projects/project_pvartiai/rsv/predictors/meds_pregnancy_wide.csv") %>%
  as_tibble

meds_father_beforepreg_wide <- fread("/data/projects/project_pvartiai/rsv/predictors/meds_father_beforepreg_wide.csv") %>%
  as_tibble

meds_sib_wide <- fread("/data/projects/project_pvartiai/rsv/predictors/meds_sib_wide.csv") %>%
   as_tibble



univar.baseadjusted.modeling(base = development_set, vars = neonatal_dgs, name_of_vars = "neodg")
multivar.modeling(base = development_set, vars = neonatal_dgs, name_of_vars = "neodg")
model.aic(base = development_set, vars = neonatal_dgs, name_of_vars = "neodg")
rm(neonatal_dgs)
rm(neonatal_dgs_pre)
gc()
Sys.time()
#


univar.baseadjusted.modeling(base = development_set, vars = sib_ep, name_of_vars = "sib_ep")
multivar.modeling(base = development_set, vars = sib_ep, name_of_vars = "sib_ep")
model.aic(base = development_set, vars = sib_ep, name_of_vars = "sib_ep")

rm(sib_ep)
gc()
Sys.time()
#


univar.baseadjusted.modeling(base = development_set, vars = mo_ep, name_of_vars = "mother_ep")
multivar.modeling(base = development_set, vars = mo_ep, name_of_vars = "mother_ep")
model.aic(base = development_set, vars = mo_ep, name_of_vars = "mother_ep")
rm(mo_ep)
gc()
Sys.time()
#

univar.baseadjusted.modeling(base = development_set, vars = fa_ep, name_of_vars = "father_ep")
multivar.modeling(base = development_set, vars = fa_ep, name_of_vars = "father_ep")
model.aic(base = development_set, vars = fa_ep, name_of_vars = "father_ep")
rm(fa_ep)
gc()

# meds_mother_beforepreg_wide

univar.baseadjusted.modeling(base = development_set, vars = meds_mother_beforepreg_wide, name_of_vars = "meds_mother_beforepreg")
multivar.modeling(base = development_set, vars = meds_mother_beforepreg_wide, name_of_vars = "meds_mother_beforepreg")
model.aic(base = development_set, vars = meds_mother_beforepreg_wide, name_of_vars = "meds_mother_beforepreg")

# meds_mother_pregnancy_wide

univar.baseadjusted.modeling(base = development_set, vars = meds_mother_pregnancy_wide, name_of_vars = "meds_mother_preg")
multivar.modeling(base = development_set, vars = meds_mother_pregnancy_wide, name_of_vars = "meds_mother_preg")
model.aic(base = development_set, vars = meds_mother_pregnancy_wide, name_of_vars = "meds_mother_preg")


# meds_father_beforepreg_wide

univar.baseadjusted.modeling(base = development_set, vars = meds_father_beforepreg_wide, name_of_vars = "meds_father_beforepreg")
multivar.modeling(base = development_set, vars = meds_father_beforepreg_wide, name_of_vars = "meds_father_beforepreg")
model.aic(base = development_set, vars = meds_father_beforepreg_wide, name_of_vars = "meds_father_beforepreg")


# meds_sib

univar.baseadjusted.modeling(base = development_set, vars = meds_sib_wide, name_of_vars = "meds_sib")
multivar.modeling(base = development_set, vars = meds_sib_wide, name_of_vars = "meds_sib")
model.aic(base = development_set, vars = meds_sib_wide, name_of_vars = "meds_sib")



# # # # # # # # # # # # #



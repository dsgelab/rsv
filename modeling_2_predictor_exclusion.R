
# load the libraries

library(tidyr)
library(dplyr)
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
library(knitr)
# splines2 package and function 'ns' will (hopefully) 
# work better in predicting fitted values when using spline functions
# There was a problem in predicting using 'rcs' in rms package. See https://docs.google.com/document/d/1QTnRRcKFMXZ7mM8L60B5kSme4M87xZ4tLIn2iV316XM/edit#heading=h.jnbga7daq4jd
library(splines2)
library(tidymodels)
library(lmtest)
library(glmnet)



# MODELING SCRIPT 1

# we work with the composite data which combines variables from all different source registries.
# First, composite data includes all candidate predictors
# then we'll do the AIC-based backwards stepwise exclusion. Then we'll model some epidemic changes.

# to clarify, composite data is only build from the development set.


# composite data location
comp_path <- "/data/projects/project_pvartiai/rsv/predictors/"
composite_data_name <- "composite_data_all_candidate_predictors.csv"
# all_possible_composite_data_name <- "composite_data_all_interesting_variables.csv"



### Read the data combined from different registries
# load the data with liberally selected variables for the AIC-exclusion
# composite_data <- fread(paste0(comp_path, all_possible_composite_data_name)) %>% as_tibble

## if need to read more conservatively selected variables, use
composite_data <- fread(paste0(comp_path, composite_data_name)) %>% 
  as_tibble %>%
  select(-any_of("ASTHMA_ACUTE_RESPIRATORY_INFECTIONS"))


# ids and birth dates
all_bds <- fread("/data/projects/project_pvartiai/rsv/wrangle/preprocess_3_mbr_hilmo_outcomes.csv", 
                 select = c("TNRO", "LAPSEN_SYNTYMAPVM")) %>% as_tibble




## build the first formula object

# remove non-predictor variables
all_features <- setdiff(names(composite_data), c("TNRO", 
                                                 "outcome", 
                                                 "LAPSEN_SYNTYMAPVM", 
                                                 "birth_year", 
                                                 "epidemic_year",
                                                 "height_sd",
                                                 "outcome_3days", 
                                                 "outcome_duration",
                                                 "birth_month")) # replace this with birth month if modeling with distance and vice versa


cont_vars <- c("gest_days", "dist", "weight_sd", "mother_age", "father_age") 
cont_string <- paste0('rcs(', cont_vars, ', 4)+') %>% paste(., collapse = "")

other_vars <- setdiff(all_features, cont_vars)
other_string <- paste(other_vars, collapse = "+")

allvars_string <- paste(c(cont_string, other_string), collapse = "")

start_string <- "outcome ~ "
# start_longhosp_string <- "outcome_3days ~"

formula_string <- paste(start_string, allvars_string)
# formula_longhosp_string <- paste(start_longhosp_string, allvars_string)

# formula objects
all_form <- formula(formula_string)
# all_longhosp_form <- formula(formula_longhosp_string)






#  C O M P O S I T E    D A T A    N #

count <- composite_data %>%
  select(any_of(other_vars)) %>%
  summarise(across(everything(), ~ sum(., na.rm = T))) %>%
  pivot_longer(cols = everything()) %>%
  rename(feature = name,
         count = value)

setwd("/data/projects/project_pvartiai/rsv/results/composite_model/")
write.csv(count,  "first_composite_count.csv",
          row.names = FALSE)







# B U I L D    T H E   F I R S T    M O D E L    
# with all preselected variables (we'll do the AIC exclusion based on this variable list)

first_fit <- glm(data = (composite_data),
                 family = binomial(),
                 formula = all_form)

# data dist for rcs fit
ddist <- datadist(composite_data)
options("datadist" = ddist)

first_fit_rcs <- lrm(data = composite_data,
                     x = TRUE, y = TRUE,
                     formula = all_form)

summary(first_fit)

multivar_results <- data.frame(feature = rownames(coef(summary(first_fit))), 
                               coef = NA, 
                               se = NA, 
                               p = NA)

multivar_results["coef"] <- (coef(summary(first_fit))[,1] %>% as_tibble)
multivar_results["se"] <- coef(summary(first_fit))[,2]
multivar_results["p"] <- coef(summary(first_fit))[,4]


setwd("/data/projects/project_pvartiai/rsv/results/composite_model/")
write.csv(multivar_results, "all_composite_model_coefs_2_edited_features.csv")


### long code to get the anova table
anova_table <- anova(first_fit, test = "LRT")



write.csv(anova_table, "anova_results_pre_aic_model.csv")



# # # # #    L A S S O    F O R    F I R S T    F I T 

composite_data


# data
lasso_data <- composite_data %>%
  select(-any_of(c("TNRO",
                   "outcome_3days",
                   "outcome_duration",
                   "LAPSEN_SYNTYMAPVM",
                   "birth_year",
                   "birth_month",
                   "epidemic_year",
                   "even_epidemic_year"))) %>%
  na.omit()


# model matrices
xvar <- model.matrix(~ ., select(lasso_data, -outcome))
yvar <- lasso_data %>% select(outcome) %>% data.matrix


# lasso model 
lasso <- cv.glmnet(xvar, 
                   yvar, 
                   alpha = 1, 
                   nlambda = 15, # number of lambdas to test
                   nfolds = 10, # nubmer of cv folds
                   family = "binomial",
                   type.measure = "auc")


# view the model
plot(lasso)

# best fitting model head - tarviiko tätä?
best_lambda <- lasso$lambda.1se # lambda value of .1se is chosen, to get the most simple model where we are within 1 se
# best_lambda <- lasso$lambda.min # this selects lambda with minimum RSE

# best auc: 0.7462
lasso_auc <- 0.7462

best_fit <- lasso$glmnet.fit
head(best_fit)
coef(best_fit)
best_lambda_index <- which(lasso$lambda == best_lambda)
best_lambda_betas <- lasso$glmnet.fit$beta[, (best_lambda_index)]
only_important_betas <- best_lambda_betas[best_lambda_betas != 0]
as.data.frame(only_important_betas)
length(only_important_betas) # 64
lasso_vars <- names(only_important_betas)


lasso_coefs <- as.data.frame(best_lambda_betas) %>%
  rownames_to_column(var = "feature")


lasso_coefs


# save
setwd("/data/projects/project_pvartiai/rsv/results/composite_model/")
write.csv(lasso_coefs, file = "lasso_first_composite_model.csv",
          row.names = FALSE)


# which lasso coefs are important?
lasso_coefs






#  M O D E L  F O R    A I C    S E L E C T I O N 


# rcs-compatible fit, done in the previous step. Retained the old name final_fit2 to keep the script intact
final_fit2 <- first_fit_rcs

# final_longhosp_fit <- lrm(data = (composite_data),
#                           formula = interaction_longhosp_form,
#                           x = TRUE, y = TRUE)


weird_na_coefs <- coefficients(final_fit2)[is.na(coefficients(final_fit2))] %>% 
  names # no variables

aic_multivar <- fastbw(final_fit2)

# select retained feature names as vector,
# and create formula strings again
dropped_vars <- setdiff(names(composite_data), 
                        c(aic_multivar$names.kept, "TNRO", "outcome", "LAPSEN_SYNTYMAPVM", 
                          "mother_asthma", "sib_asthma"))

# 4 continuous variables in 3/2022
step2_cont_vars <- setdiff(cont_vars, dropped_vars) 
# 43 other variables
step2_other_vars <- setdiff(c(aic_multivar$names.kept, 
                              "mother_asthma", "sib_asthma", "father_asthma"), 
                            c(step2_cont_vars, "dist"))

if(length(step2_other_vars) == 0) {
  step2_cont_string <- paste0('rcs(', step2_cont_vars, ', 4)') %>% paste(., collapse = "+")
} else {
  step2_cont_string <- paste0('rcs(', step2_cont_vars, ', 4)+') %>% paste(., collapse = "")
}

step2_other_string <- paste(step2_other_vars, collapse = "+")

# create formula object
step2_string <- paste(start_string, step2_cont_string, step2_other_string) 
step2_form <- formula(step2_string)



# # # # fit model that includes all predictors from AIC exclusion

# fit of post_aic variables
postaic_fit <- glm(data = (composite_data),
                   family = binomial(),
                   formula = step2_form)

# rcs fit for anova
postaic_fit_rcs <- lrm(data = composite_data,
                       x = TRUE, y = TRUE,
                       formula = step2_form)

#
#
#
#

postaic_results <- data.frame(feature = rownames(coef(summary(postaic_fit))), 
                              coef = NA, 
                              se = NA, 
                              p = NA)

postaic_results["coef"] <- (coef(summary(postaic_fit))[,1] %>% as_tibble)
postaic_results["se"] <- coef(summary(postaic_fit))[,2]
postaic_results["p"] <- coef(summary(postaic_fit))[,4]


setwd("/data/projects/project_pvartiai/rsv/results/composite_model/")
write.csv(postaic_results, "post_aic_all_coefficients.csv")


## long code to get the anova table

postaic_anova_table <- anova(postaic_fit_rcs)
# names for anova table
postaic_anova_names <- postaic_anova_table %>% row.names
postaic_anova_table <- data.frame(postaic_anova_table, row.names = NULL)
postaic_anova_table$feature <- postaic_anova_names

postaic_anova_table_refined <- postaic_anova_table %>% as_tibble %>%
  rename(chisq = Chi.Square,
         df = d.f.,
         p = P) %>%
  select(feature, chisq, p, df) %>%
  mutate(nonlinear = ifelse(feature == " Nonlinear", "nonlinear", "linear"),
         feature = ifelse(feature == " Nonlinear", lag(feature), feature)) %>%
  pivot_wider(names_from = nonlinear,
              values_from = c(chisq, p)) %>%
  pivot_longer(cols = c(chisq_linear, chisq_nonlinear, p_linear, p_nonlinear)) %>%
  filter(!is.na(value)) %>%
  select(-df) %>%
  separate(name, into = c("stat", "linear"), sep = "_") %>%
  pivot_wider(names_from = stat,
              values_from = value) %>%
  pivot_wider(names_from = linear,
              values_from = c(chisq, p)) %>%
  select(feature, chisq_linear, p_linear, chisq_nonlinear, p_nonlinear)

write.csv(postaic_anova_table_refined, "anova_post_aic_model_rcs.csv")


postaic_anova_table_vanilla <- anova(postaic_fit, test = "LRT")
write.csv(postaic_anova_table_vanilla, "anova_post_aic_model_vanilla.csv")


postaic_anova_table_refined %>%
  arrange(desc(chisq_linear)) %>%
  print(n = 30)





# # # # # # # # # # C R E A T E     P R E D I C T     D A  T A     # # # 

keep_these <- c("sib_asthma", "father_asthma", "sib_asthma")
not_these <- c()

# Create 'predict_data', the final data to be used in the next script
predict_data <- composite_data %>%
  select(c(TNRO, 
           outcome, 
           outcome_3days, 
           outcome_duration,
           step2_cont_vars, 
           step2_other_vars, 
           keep_these)) %>%
  left_join(., all_bds, by = "TNRO") %>%
  mutate(birth_year = as.numeric(year(LAPSEN_SYNTYMAPVM))) %>%
  mutate(birth_month = as.numeric(month(LAPSEN_SYNTYMAPVM))) %>%
  mutate(epidemic_year = ifelse(birth_month <= 5, birth_year, (birth_year+1)))


names(predict_data)

# confirm the esophagus atresia variable
esophagus <- fread("/data/projects/project_pvartiai/rsv/predictors/esophagus_malformations.csv") %>%
  as_tibble

predict_data <- predict_data %>%
  select(-Q39_neodg) %>%
  left_join(., esophagus)

# save
setwd("/data/projects/project_pvartiai/rsv/modeling")
write.csv(predict_data, "development_set_final_variables.csv",
          row.names = FALSE)


# exclude all possibly vague or less elevant variables
vague_vars <- c(
  "father_age",
  
  "ASTHMA_ACUTE_RESPIRATORY_INFECTIONS_sib_ep",
  "ppi_mother_drug_pregnancy",
  "opioids_mother_drug_pregnancy",
  "other_j21_endpoint_sib_ep", # 
  "N02BE_mother_drug_pregnancy", # paracetamol
  "po_cortison_mother_drug_pregnancy",
  "benz_mother_drug_pregnancy",
  
  "F5_KELAMENT_mother_ep",
  "K11_REFLUX_mother_ep",
  "O15_BREAST_LACT_OTHER_DIS_mother_ep",
  "N14_HYPERTROPHYBREAST_mother_ep",
  "O15_PREG_ABORT_mother_ep",
  "N14_FEMALEGENINF_mother_ep",
  "L12_CELLULITIS_mother_ep",
  "M13_FIBROBLASTIC_mother_ep",
  
  
  # antibiotics
  "kefexin_mother_drugs",
  
  # pregnancy and neonatal diagnoses
  
  "O209_neodg",
  "O342_neodg",
  "O990_neodg",
  "O40_neodg",
  "P58_neodg",
  "O9980_neodg",
  "O2682_neodg",
  "other_j21_endpoint_sib_ep",
  "Q67_68_neodg",
  "P94_neodg",
  
  "inhaled_opening_drugs_father_drugs",
  
  "montelucast_sib_drugs",
  "valproate_mother_drug_pregnancy",
  "inhaled_opening_drugs_mother_drug_pregnancy",
  "both_inhalation_meds_sib",
  "common_uti_ab_amorion_sib_drugs",
  "AB1_INTESTINAL_INFECTIONS_sib_ep",
  "CHILDHOOD_ALLERGY_sib_ep",
  
  "macrolide_mother_drugs",
  "ppi_father_drugs",
  "E4_CONGEIOD_mother_ep",
  "macrolide_sib_drugs",
  
  
  "sib_over7",
  "chd_oper_later"
)



# more compact data with limited predictors
#   - filter out vague variables
#   - combine those where possible
compact_data <- predict_data %>%
  select(-any_of(vague_vars)) %>%
  
  # create any family member asthma variable
  mutate(any_family_asthma = ifelse(
    sib_asthma == 1 | 
      father_asthma == 1|
      mother_asthma == 1, 1, 0)) %>%
  
  # create any severe chd (needing operation during 1st year of life)
  mutate(all_chd_oper_1y = ifelse(
    chd_oper_1mo == 1 | chd_oper_1year == 1, 1, 0
  )) %>%
  
  # recode siblings into binary
  mutate(sib_0_4 = ifelse(sib_0_4 == "0", 0, 1),
         sib_4_7 = ifelse(sib_4_7 == "0", 0, 1)) %>%
  
  # combine asd and vsd
  mutate(asd_or_vsd_only = ifelse(only_common_asd_mbr_structural == 1|
                                    only_vsd_mbr_structural == 1, 1, 0)) %>%

  
  # remove variables used in the creation of the composites
  select(-any_of(c(
    "sib_asthma",
    "father_asthma",
    "mother_asthma",
    "chd_oper_1mo",
    "chd_oper_1year",
    "only_common_asd_mbr_structural",
    "only_vsd_mbr_structural",
    
    "common_uti_ab_amorion_mother_drug_pregnancy",
    "macrolide_mother_drug_pregnancy",
    "kefexin_mother_drug_pregnancy",
    "neonate_substance"
  ))) %>%
  # rename
  rename(any_severe_chd = all_chd_oper_1y,
         sib_resp_hosp = sib_resp_hosp_sib_ep)



compact_data

compact_data %>% names


setwd("/data/projects/project_pvartiai/rsv/modeling")
write.csv(compact_data, "development_set_only_feasible_variables.csv",
          row.names = FALSE)




### EDIT VALIDATION SET FOR THE FINAL MORE COMPACT FORM

val_dir <- "/data/projects/project_pvartiai/rsv/validation_data/"
comp_val_path <- paste0(val_dir, "validation_set_all_candidates.csv")
# load esophagus - confirm the esophagus atresia variable
esophagus <- fread("/data/projects/project_pvartiai/rsv/predictors/esophagus_malformations.csv") %>%
  as_tibble

# load composite validation data
comp_val <- fread(comp_val_path) %>%
  as_tibble %>%
  select(c(TNRO, 
           outcome, 
           outcome_3days, 
           outcome_duration,
           step2_cont_vars, 
           step2_other_vars, 
           keep_these)) %>%
  left_join(., all_bds, by = "TNRO") %>%
  mutate(birth_year = as.numeric(year(LAPSEN_SYNTYMAPVM))) %>%
  mutate(birth_month = as.numeric(month(LAPSEN_SYNTYMAPVM))) %>%
  mutate(epidemic_year = ifelse(birth_month <= 5, birth_year, (birth_year+1))) %>%
  select(-Q39_neodg) %>%
  left_join(., esophagus)


final_val <- comp_val %>%
  select(-any_of(vague_vars)) %>%
  
  # create any family member asthma variable
  mutate(any_family_asthma = ifelse(
    sib_asthma == 1 | 
      father_asthma == 1|
      mother_asthma == 1, 1, 0)) %>%
  
  # create any severe chd (needing operation during 1st year of life)
  mutate(all_chd_oper_1y = ifelse(
    chd_oper_1mo == 1 | chd_oper_1year == 1, 1, 0
  )) %>%
  
  # recode siblings into binary
  mutate(sib_0_4 = ifelse(sib_0_4 == "0", 0, 1),
         sib_4_7 = ifelse(sib_4_7 == "0", 0, 1)) %>%
  
  # combine asd and vsd
  mutate(asd_or_vsd_only = ifelse(only_common_asd_mbr_structural == 1|
                                    only_vsd_mbr_structural == 1, 1, 0)) %>%
  
  # remove variables used in the creation of the composites
  select(-any_of(c(
    "sib_asthma",
    "father_asthma",
    "mother_asthma",
    "chd_oper_1mo",
    "chd_oper_1year",
    "only_common_asd_mbr_structural",
    "only_vsd_mbr_structural",
    
    "common_uti_ab_amorion_mother_drug_pregnancy",
    "macrolide_mother_drug_pregnancy",
    "kefexin_mother_drug_pregnancy"
  ))) %>%
  # rename
  rename(any_severe_chd = all_chd_oper_1y,
         sib_resp_hosp = sib_resp_hosp_sib_ep)


setwd("/data/projects/project_pvartiai/rsv/validation_data")
final_val

write.csv(final_val, "validation_set_regression.csv",
          row.names = FALSE)


# composite model table


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







comp_dir <- "/data/projects/project_pvartiai/rsv/results/composite_model/"
pre_name <- "all_composite_model_coefs_2_edited_features.csv"
pre_anova_name <- "anova_results_pre_aic_model.csv"
all_n_name <- "first_composite_count.csv"
lasso_name <- "lasso_first_composite_model.csv"
postaic_name <- "post_aic_all_coefficients.csv"


# n of all variables
all_n <- fread(paste0(comp_dir, all_n_name)) %>%
  select(-any_of("V1"))


# coefficients of the first model
pre_coef <- fread(paste0(comp_dir, pre_name)) %>%
  mutate(pre_feature = feature) %>%
  mutate(feature =  case_when(
    grepl("gest_days", pre_feature) ~ "gest_days",
    grepl("dist", pre_feature) ~ "dist",
    grepl("mother_age", pre_feature) ~ "mother_age",
    grepl("weight_sd", pre_feature) ~ "weight_sd",
    grepl("father_age", pre_feature) ~ "father_age",
    TRUE ~ pre_feature
  )) %>%
  select(-any_of("V1")) %>%
  select(-se)



pre_results <- left_join(pre_coef, all_n, by = "feature")

# read lasso
lasso <- fread(paste0(comp_dir, lasso_name))


#join lasso to results 
pre_res_lasso <- left_join(pre_results, lasso, by = "feature")


View(pre_res_lasso)

# load post_aic results 

post_coef <- fread(paste0(comp_dir, "post_aic_all_coefficients.csv")) %>% 
  mutate(pre_feature = feature) %>%
  select(-any_of("V1")) %>%
  select(-se, -feature) %>%
  rename(post_aic_coef = coef,
         post_aic_p = p)


prepost_res <- left_join(pre_res_lasso, post_coef, by = "pre_feature")

View(prepost_res)

# anova of the first model
pre_anova <- fread(paste0(comp_dir, pre_anova_name)) 

## long code to get the anova table

# names for anova table
pre_anova_names <- pre_anova$V1
pre_anova_table <- data.frame(pre_anova, row.names = NULL)
pre_anova_table$feature <- pre_anova_names

pre_anova_table_refined <- pre_anova_table %>% as_tibble %>%
  rename(chisq_pre_aic = "Deviance",
         p_chi_pre_aic = 'Pr..Chi.') %>%
  select(feature, chisq_pre_aic, p_chi_pre_aic)
# # %>%
#   mutate(nonlinear = ifelse(feature == " Nonlinear", "nonlinear", "linear"),
#          feature = ifelse(feature == " Nonlinear", lag(feature), feature)) %>%
#   pivot_wider(names_from = nonlinear,
#               values_from = c(chisq, p)) %>%
#   pivot_longer(cols = c(chisq_linear, chisq_nonlinear, p_linear, p_nonlinear)) %>%
#   filter(!is.na(value)) %>%
#   select(-df) %>%
#   separate(name, into = c("stat", "linear"), sep = "_") %>%
#   pivot_wider(names_from = stat,
#               values_from = value) %>%
#   pivot_wider(names_from = linear,
#               values_from = c(chisq, p)) %>%
#   select(feature, chisq_linear, p_linear, chisq_nonlinear, p_nonlinear)


pre_anova <- pre_anova_table_refined





# anova of the post_aic model
post_anova <- fread(paste0(comp_dir, "anova_post_aic_model_vanilla.csv")) %>%
  as_tibble() %>%
  rename(chisq_post_aic = "Deviance",
         p_chi_post_anova = 'Pr(>Chi)',
         feature = "V1") %>%
  select(feature, chisq_post_aic, p_chi_post_anova)


all_anovas <- left_join(pre_anova, post_anova, by = "feature") %>%
  mutate(feature = case_when(
    grepl("gest_days", feature, fixed = TRUE) ~ "gest_days",
    grepl("dist", feature, fixed = TRUE) ~ "dist",
    grepl("mother_age", feature, fixed = TRUE) ~ "mother_age",
    grepl("weight_sd", feature, fixed = TRUE) ~ "weight_sd",
    grepl("father_age", feature, fixed = TRUE) ~ "father_age",
    TRUE ~ feature
  ))




prepost_res
all_anovas


featureselect_model_res <- left_join(prepost_res, all_anovas, by = "feature") %>%
  select(-pre_feature)

View(featureselect_model_res)



### INTERACTION TABLE

interactions <- fread(paste0(comp_dir, "huge_interaction_anova.csv")) %>%
  as_tibble() %>%
  filter(!is.na(var2)) %>%
  mutate(var1 = case_when(
    grepl("gest_days", var1, fixed = TRUE) ~ "gest_days",
    grepl("dist", var1, fixed = TRUE) ~ "dist",
    grepl("mother_age", var1, fixed = TRUE) ~ "mother_age",
    grepl("weight_sd", var1, fixed = TRUE) ~ "weight_sd",
    grepl("father_age", var1, fixed = TRUE) ~ "father_age",
    TRUE ~ var1
  )) %>%
  mutate(var2 = case_when(
    grepl("gest_days", var2, fixed = TRUE) ~ "gest_days",
    grepl("dist", var2, fixed = TRUE) ~ "dist",
    grepl("mother_age", var2, fixed = TRUE) ~ "mother_age",
    grepl("weight_sd", var2, fixed = TRUE) ~ "weight_sd",
    grepl("father_age", var2, fixed = TRUE) ~ "father_age",
    TRUE ~ var2
  )) %>%
  rename("p" = "Pr(>Chi)",
         "feature1" = "var1",
         "feature2" = "var2") %>%
  left_join(., keylist_final, by = c("feature1" = "feature")) %>%
  left_join(., keylist_final, by = c("feature2" = "feature")) %>%
  select(feature1, feature2, Deviance, p, longname.x, longname.y) 

View(interactions)





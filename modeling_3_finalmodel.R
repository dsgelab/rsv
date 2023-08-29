
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
library(gridExtra)
library(dcurves)



basic_vars <- fread("/data/projects/project_pvartiai/rsv/wrangle/preprocess_3_mbr_hilmo_outcomes.csv",
                    select = c("TNRO", "outcome", "age_at_outcome", "gest_days", "SYNTYMAPAINO", "LAPSEN_SYNTYMAPVM")) %>%
  as_tibble 


# load datasets: predict_data and all_bds
setwd("/data/projects/project_pvartiai/rsv/modeling")
predict_datapath <- "/data/projects/project_pvartiai/rsv/modeling/"
comp_datapath <- "/data/projects/project_pvartiai/rsv/predictors/"


# training data with non-feasible variables filtered out
compact_data <- fread(paste0(predict_datapath, 
                             "development_set_only_feasible_variables.csv")) %>%
  as_tibble %>%
  # add birth month variable to the predict data 
  mutate(birth_month = as.numeric(month(LAPSEN_SYNTYMAPVM))) 


recent_data <- compact_data %>%
  filter(epidemic_year > 2006)

# # load composite data (Pre-AIC-data) if needed
 # composite_data <- fread(paste0(comp_datapath, "composite_data_all_candidate_predictors.csv")) %>%
 #   as_tibble

# ids and birth dates
all_bds <- fread("/data/projects/project_pvartiai/rsv/wrangle/preprocess_3_mbr_hilmo_outcomes.csv", 
                 select = c("TNRO", "LAPSEN_SYNTYMAPVM")) %>% as_tibble



# key list for final data names
name_vec <- c(
  "id",
  "RSV hospitalisation", 
  "RSV hospitalisation lasting 3 days or more",
  "Duration of RSV hospitalisation",
  "Gestational age at birth",
  "Months from birth to next estimated epidemic peak",
  "Birth weight in Z score",
  "Mother's age at birth, years",
  "Twin sibling",
  "Male sex",
  "Older siblings aged less than 4 years",
  "Older siblings aged 4-7 years",
  "Down syndrome",
  "Sibling hospitalised for viral bronchiolitis or wheezing at age 0-4 years",
  "Maternal smoking during pregnancy",
  "Neonatal respiratory conditions in a term child",
  "Birth date",
  "Birth year",
  "Birth month",
  "Year of the next RSV epidemic",
  "Esophagus malformations",
  "Asthma in first-degree relative",
  "Congenital heart defect requiring operation during the first year of life",
  "Lower complexity atrial or ventricular septal defect")



keylist_final <- tibble(
  feature = colnames(compact_data),
  longname = name_vec) %>% 
  mutate(longname = str_wrap(longname, 26))



setwd("/data/projects/project_pvartiai/rsv/functions/")
save(keylist_final, file = "keylist_final_variable_names.R")






# # # #      V A R I A B L E   N A M E S  


all_features_compact <- setdiff(names(compact_data), 
                                c("TNRO", 
                                  "outcome", 
                                  "outcome_3days",
                                  "outcome_duration",
                                  "LAPSEN_SYNTYMAPVM", 
                                  "birth_year", 
                                  "epidemic_year",
                                  # now modeling with birth month
                                  "birth_month",
                                  "bpd"
                                )) # replace with "dist" if modeling with birth month and vice versa. 

# continuous variables using distance from epidemic (instead of birth month) 

cont_vars <- c("dist", "gest_days", "weight_sd", "mother_age")


#     S P L I N E    K N O T     L O C A T I O N S   

rcsknots_dist <- with(recent_data, c(rcspline.eval(dist, nk = 4, knots.only = TRUE))) 

rcsknots_gestdays <- with(recent_data, c(203, 238, rcspline.eval(gest_days, nk = 4, knots.only = TRUE))) 

rcsknots_weightsd <- with(recent_data, c(rcspline.eval(weight_sd, nk = 4, knots.only = TRUE))) 

rcsknots_motherage <- with(recent_data, c(rcspline.eval(mother_age, nk = 4, knots.only = TRUE))) 

# # we can also explore with setting more knots
# knots_gestdays <- c(220, 240, 257, 276, 283)


# vector of knot names for the loop
rcsknot_names <- c(
  "rcsknots_dist", 
  "rcsknots_gestdays",
  "rcsknots_weightsd",
  "rcsknots_motherage")

#### continuous variable spline function strings
#### create strings for rcs

# initialize the loop
cont_string_fixedknots_rcs <- NULL

# loop for creating a string for each continuous variable
for(i in 1:4) {
  cont_string_fixedknots_rcs <- paste0(cont_string_fixedknots_rcs, 
                                       'rcs(', 
                                       cont_vars[i], 
                                       ', parms = ', 
                                       rcsknot_names[i],
                                       ')+')
}




# # # # # # #     F O R M U L A     O B J E C T S   

# custom variables to be removed
not_these_vars <- c("bpd", "even_epidemic_year", "neonate_substance", "any_ab_pregnancy")

# other than continuous variables; compact data
other_vars_compact <- setdiff(all_features_compact, c(cont_vars, not_these_vars))
other_string_compact <- paste(other_vars_compact, collapse = "+")

# combine continuous variables to others
allvars_string_compact_rcs <- paste(c(cont_string_fixedknots_rcs, other_string_compact), collapse = "")

# join start_string to the variable name strings
start_string <- "outcome ~ "

# complete formula string 
formula_string_rcs <- paste(start_string, allvars_string_compact_rcs)

# formula objects 
# formula for compact data and variables, without interactions
final_form_simple <- formula(formula_string_rcs)

# save the final formula object
setwd("/data/projects/project_pvartiai/rsv/functions/model/")
save(final_form_simple, file = "final_formula_object_nointeract.R")

# save knot locations
save(rcsknots_dist, file = "rcsknots_dist.R")
save(rcsknots_gestdays, file = "rcsknots_gestdays.R")
save(rcsknots_motherage, file = "rcsknots_motherage.R")
save(rcsknots_weightsd, file = "rcsknots_weightsd.R")


## test if it works
# asdf <- get(load("final_formula_object_nointeract.R"))
# load("rcsknots_motherage.R")



#  F I N A L    M O D E L    F I T S    A N D   C O E F S  

final_simple_fit <- glm(formula = final_form_simple,
                        data = recent_data,
                        family = binomial())

summary(final_simple_fit)

#     C O E F F I C I E N T S    O F    T H E    M O D E L 


final_coef <- data.frame(feature = rownames(coef(summary(final_simple_fit))), 
                         coef = NA, 
                         se = NA, 
                         p = NA)

final_coef["coef"] <- (coef(summary(final_simple_fit))[,1] %>% as_tibble)
final_coef["se"] <- coef(summary(final_simple_fit))[,2]
final_coef["p"] <- coef(summary(final_simple_fit))[,4]




setwd("/data/projects/project_pvartiai/rsv/results/composite_model/")
write.csv(final_coef, "final_model_coefficients_nointeract.csv")


# # RMS fit for final simple model
#
# # data dist for fitting the model (requirement for prediction)
# ddist <- datadist(recent_data)
# options("datadist" = ddist)
#
# # the fit 
# rcs_fit <- lrm(data = recent_data,
#                x = TRUE, y = TRUE,
#                formula = final_form_simple)
#
#
# ### long code to get the anova table
#
# final_anova_table <- anova(rcs_fit)
# # names for anova table
# final_anova_names <- final_anova_table %>% row.names
# final_anova_table <- data.frame(final_anova_table, row.names = NULL)
# final_anova_table$feature <- final_anova_names
# 
# final_anova_table_refined <- final_anova_table %>% as_tibble %>%
#   rename(chisq = Chi.Square,
#          df = d.f.,
#          p = P) %>%
#   select(feature, chisq, p, df) %>%
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




# final n and mean of all variables

# final n of all variables

n_post_2007 <- recent_data %>% 
  select(any_of(c(other_vars_compact))) %>%
  mutate(n_tot = 1) %>%
  summarise(across(everything(), .fns = ~ sum(., na.rm = T))) %>%
  t %>%
  as.data.frame() %>%
  rownames_to_column(var = "feature") %>%
  rename(n_post_2007 = V1)

n_all_training_data <- compact_data %>% 
  select(any_of(c(other_vars_compact))) %>%
  mutate(n_tot = 1) %>%
  summarise(across(everything(), .fns = ~ sum(., na.rm = T))) %>%
  t %>%
  as.data.frame() %>%
  rownames_to_column(var = "feature") %>%
  rename(n_all_training = V1)

n_all <- left_join(n_post_2007, n_all_training_data) %>%
  mutate(ntot_2007 = 622588,
         ntot_all = 1126959) %>%
  mutate(perc_2007 = n_post_2007/ntot_2007,
         perc_tot = n_all_training / ntot_all) %>%
  select(-ntot_all, -ntot_2007)


cont_vars_post_2007 <- recent_data %>%
  select(any_of(c(cont_vars))) %>%
  pivot_longer(cols = everything()) %>%
  group_by(name) %>%
  summarise(median_post_2007 = median(value, na.rm = T),
            iqr_post_2007 = IQR(value, na.rm = T),
            mean_post_2007 = mean(value, na.rm = T),
            sd_post_2007 = sd(value, na.rm = T))

cont_vars_all_training <- compact_data %>%
  select(any_of(c(cont_vars))) %>%
  pivot_longer(cols = everything()) %>%
  group_by(name) %>%
  summarise(median_alltrain = median(value, na.rm = T),
            iqr_alltrain = IQR(value, na.rm = T),
            mean_alltrain = mean(value, na.rm = T),
            sd_alltrain = sd(value, na.rm = T))

all_cont <- left_join(cont_vars_all_training, cont_vars_post_2007)

all_cont
n_all

setwd("/data/projects/project_pvartiai/rsv/results/composite_model/")
write.csv(all_cont, "continuous_variable_mean_and_median.csv",
          row.names = FALSE)

write.csv(n_all, "binary_variable_count_and_perc.csv",
          row.names = FALSE)




### check interactions - huge massive anova

### Anova of the final model

# Anova of the final model
official_anova <- anova(final_simple_fit, test = "LRT")

anova_res <- official_anova %>%
  as.data.frame %>%
  rownames_to_column(var = "feature") 


write.csv(anova_res, "anova_final_model_nointeract.csv", 
          row.names = FALSE)


# Check interactions and their LRTs
all_possible_interactions_form <- update(final_form_simple, ~ . + 
                                           
                                           rcs(dist, parms = rcsknots_dist)*rcs(gest_days, parms = rcsknots_gestdays) + 
                                           rcs(dist, parms = rcsknots_dist)*twin + 
                                           rcs(dist, parms = rcsknots_dist)*sib_0_4 + 
                                           rcs(dist, parms = rcsknots_dist)*sib_4_7 + 
                                           rcs(dist, parms = rcsknots_dist)*any_severe_chd + 
                                           rcs(dist, parms = rcsknots_dist)*male_gender + 
                                           
                                           rcs(gest_days, parms = rcsknots_gestdays)*any_severe_chd + 
                                           rcs(gest_days, parms = rcsknots_gestdays)*sib_0_4 + 
                                           rcs(gest_days, parms = rcsknots_gestdays)*sib_4_7 + 
                                           rcs(gest_days, parms = rcsknots_gestdays)*twin + 
                                           rcs(gest_days, parms = rcsknots_gestdays)*down + 
                                           rcs(gest_days, parms = rcsknots_gestdays)*asd_or_vsd_only +
                                           rcs(gest_days, parms = rcsknots_gestdays)*smoking_neodg + 
                                           rcs(gest_days, parms = rcsknots_gestdays)*rcs(weight_sd, parms = rcsknots_weightsd) + 
                                           rcs(weight_sd, parms = rcsknots_weightsd)* rcs(dist, parms = rcsknots_dist) + 
                                           
                                           down*any_severe_chd + 
                                           down*asd_or_vsd_only +
                                           smoking_neodg*any_family_asthma +
                                           sib_resp_hosp*smoking_neodg + 
                                           smoking_neodg*any_family_asthma + 
                                           asd_or_vsd_only*term_breathing + 
                                           asd_or_vsd_only*sib_4_7+
                                           smoking_neodg*sib_0_4 + 
                                           sib_0_4*any_severe_chd + 
                                           sib_0_4*sib_resp_hosp)


interact_fit <- glm(formula = all_possible_interactions_form,
                    family = binomial(),
                    data = recent_data)

huge_anova <- anova(interact_fit, test = "LRT")

huge_anova_res <- huge_anova %>%
  as.data.frame() %>%
  rownames_to_column(var = "feature") %>%
  as_tibble %>%
  separate(feature, into = c("var1", "var2"), sep = ":")


setwd("/data/projects/project_pvartiai/rsv/results/composite_model/")
write.csv(huge_anova_res, "huge_interaction_anova.csv")


# examine only interactions
huge_anova_res %>%
  filter(!is.na(var2)) %>%
  arrange(desc(Deviance)) %>%
  print(n=100)

#examine only main effects
huge_anova_res %>%
  filter(is.na(var2)) %>% 
  arrange(desc(Deviance)) %>%
  print(n=100)




# # # # # # #     S A V E    F I N A L    M O D E L    O B J E C T   
setwd("/data/projects/project_pvartiai/rsv/functions/model/")

save(final_simple_fit, file = "final_model_fit.R")

#define helper function to remove unnecessary data from glm fit objects to make storing them smaller and remove all individual-level data
strip.glm = function(cm) {
  
  cm$y = c()
  cm$model = c()
  cm$residuals = c()
  cm$fitted.values = c()
  cm$data = c()
  cm$weights = c()
  cm$prior.weights = c()
  cm$linear.predictors = c()
  cm$effects = c()

  
  return(cm)
}

export_object <- strip.glm(final_simple_fit)

save(export_object, file = "exportable_model_fit.R")





# # # # # # # # R M S OBJECT TO GET THE FUNCTION

dd <- datadist(recent_data)
options(datadist="dd")


# # the fit 
rcs_fit <- lrm(data = recent_data,
               x = TRUE, y = TRUE,
               formula = final_form_simple)

Function(rcs_fit)


# {-0.495649+0.1578121* dist-0.0086053703*pmax(dist,0)^3+0.029221215*pmax(dist-4,0)^3-0.027472357*pmax(dist-7,0)^3+0.0068565129*pmax(dist-11,0)^3-0.0083750411* gest_days-2.5928361e-06*pmax(gest_days-203,0)^3+3.47882e-06*pmax(gest_days-238,0)^3+1.5090076e-05*pmax(gest_days-257,0)^3-5.7195137e-05*pmax(gest_days-276,0)^3+4.7109473e-05*pmax(gest_days-283,0)^3-5.8903963e-06*pmax(gest_days-293,0)^3-0.11334548* weight_sd+0.022096044*pmax(weight_sd+1.7785123,0)^3-0.061201087*pmax(weight_sd+0.54131916,0)^3+0.041110883*pmax(weight_sd-0.22936323,0)^3-0.0020058398*pmax(weight_sd-1.6255334,0)^3-0.04860181* mother_age+0.00028470736*pmax(mother_age-21.467488,0)^3-0.00091230793*pmax(mother_age-28.172485,0)^3+0.00073228985*pmax(mother_age-32.366872,0)^3-0.00010468928*pmax(mother_age-39.277207,0)^3+0.26062728*twin+0.2120152*male_gender+0.87126927*sib_0_4+0.32130478*sib_4_7+0.82535689*down+0.62579856*sib_resp_hosp+0.20610924*smoking_neodg+0.33596109*term_breathing+1.134749*q39_confirmed+0.39850654*any_family_asthma+1.0605658*any_severe_chd+0.35697999*asd_or_vsd_only }




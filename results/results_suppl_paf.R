library(ggplot2)
library(tidyr)
library(dplyr)
library(data.table)
library(lubridate)
library(stringr)
library(graphPAF)
library(splines)
library(rms)





# load the data
data_path <- "/data/projects/project_pvartiai/rsv/modeling/"
model_path <- "/data/projects/project_pvartiai/rsv/functions/model/"
pred_path <- "/data/projects/project_pvartiai/rsv/predictors/"
val_path <- "/data/projects/project_pvartiai/rsv/validation_data/"


recent_data <- fread(paste0(data_path, "development_set_only_feasible_variables.csv")) %>%
  filter(epidemic_year >= 2007)



test_set <- fread(paste0(val_path, "validation_set_regression.csv")) %>%
  as_tibble() %>%
  distinct()


# Load model objects and knot locations

setwd("/data/projects/project_pvartiai/rsv/functions/model/")

final_form_simple <- get(load("/data/projects/project_pvartiai/rsv/functions/model/final_formula_object_nointeract.R"))
load("rcsknots_motherage.R")
load("rcsknots_dist.R") 
load("rcsknots_gestdays.R")
load("rcsknots_weightsd.R")
model_fit <- get(load("final_model_fit.R"))

setwd("/data/projects/project_pvartiai/rsv/functions/")
load("keylist_final_variable_names.R")

keylist_final$longname[keylist_final$feature == "dist"] <- "Months from birth to next\nestimated epidemic peak"


# PAF

# use only complete data on the paf calculations (very few missing overall)
paf_data <- recent_data %>%
  select(-any_of(c("outcome_3days", "outcome_duration", 
                   "LAPSEN_SYNTYMAPVM", "birth_year", 
                   "birth_month", "epidemic_year"))) %>%
  na.omit()

# names of all predictors
predictor_names <- setdiff(names(paf_data), 
                           c("TNRO", "outcome"))

# continuous predictors
cont_names <- c("gest_days", "dist", "weight_sd", "mother_age")

# binary predictors
bin_names <- setdiff(predictor_names, cont_names)


# character strings for formnula object
start_string <- "outcome ~ "
cont_string <- paste0('ns(', cont_names, ', df = 3)+') %>% paste(., collapse = "")
other_string <- paste(bin_names, collapse = "+")
allvars_string <- paste(c(cont_string, other_string), collapse = "")
formula_string <- paste(start_string, allvars_string)
ns_paf_form <- formula(formula_string)

# fit a model for PAF calculations
ns_paf_fit <- glm(formula = ns_paf_form, 
                  family = binomial(),
                  data = paf_data)


# function for discrete variable PAFs, using multivariate model (i.e. adjusted PAF)
categorical.paf.function <- function(var, data = paf_data, custom_model_object = ns_paf_fit) {
  # temp_form <- formula(paste0("outcome ~ ", var))
  
  # temp_unadjusted_fit <- glm(data = data, 
  #                            formula = temp_form,
  #                            family = binomial())
  
  paf <- PAF_calc_discrete(
    model = custom_model_object, 
    data = data,
    riskfactor = var,
    refval = 0)
  
  return(paf)
}

# test
# categorical.paf.function("term_breathing")

# function for continuous variable PAFs, using multivariate model (i.e. adjusted PAF)
continuous.paf.function <- function(var, data = paf_data, custom_model_object = ns_paf_fit) {
  
  # temp_unadjusted_fit <- glm(data = data, 
  #                            custom_formula = formula,
  #                            family = binomial())
  
  paf_df <- PAF_calc_continuous(
    model = custom_model_object,
    data = data,
    riskfactor_vec = var,
    q_vec = 0.01)
  
  paf <- paf_df$paf_q
  
  return(paf)
}



# functions to calculate AUC
cat.auc.function <- function(var, data = paf_data, formula = ns_paf_form) {
  temp_form <- formula(paste0("outcome ~ ", var))
  
  temp_unadjusted_fit <- glm(data = data, 
                             formula = temp_form,
                             family = binomial())
  
  predict <- predict(temp_unadjusted_fit, newdata = paf_data, type = "response")
  
  c_stat <- auc(paf_data$outcome, predict)
  
  return(c_stat)
  
}

continuous.auc.function <- function(var, data = paf_data) {
  temp_cont_form <- formula(paste0("outcome ~ ns(", var, ", df = 3)"))
  
  temp_unadjusted_fit <- glm(data = data, 
                             formula = temp_cont_form,
                             family = binomial())
  
  predict <- predict(temp_unadjusted_fit, newdata = paf_data, type = "response")
  
  c_stat <- auc(paf_data$outcome, predict)
  
  return(c_stat)
}

# continuous.paf.function("gest_days")

# initialize result dataframe for the loop
adjusted_paf_results <- tibble(feature = predictor_names,
                               paf = NA)

# for loop to get all pafs
for(i in 1:length(predictor_names)) {
  
  temp_var <- predictor_names[i]
  
  if(temp_var %in% cont_names) {
    temp_paf <- continuous.paf.function(temp_var)
    # temp_auc <- continuous.auc.function(temp_var)
  }
  
  
  if(temp_var %in% bin_names) {
    temp_paf <- categorical.paf.function(temp_var)
    # temp_auc <- cat.auc.function(temp_var)
  }
  
  
  adjusted_paf_results$paf[adjusted_paf_results$feature == temp_var] <- temp_paf
  # unadjusted_paf_results$auc[unadjusted_paf_results$feature == temp_var] <- temp_auc
  
  
  print(temp_var)
  
}


res <- adjusted_paf_results %>%
  left_join(keylist_final) %>%
  mutate(longname = as.factor(longname)) %>%
  mutate(longname = fct_reorder(longname, paf))


# plot
paf_plot <- res %>%
  ggplot(aes(x = paf,
         y = longname,
         label = feature)) +
  geom_col(width = 0.5) + 
  theme_bw(base_size = 13) + 
  scale_x_continuous(expand = c(0,0), limits = c(0,0.7)) + 
  labs(y = "Predictor",
       x = "Population attributable fraction")
paf_plot

# size
# https://localhost:8443/rstudio/graphics/plot_zoom_png?width=919&height=877

# save
ggsave(paf_plot,
       file = "suppl_paf.pdf",
       device = "pdf",
       dpi = 1200,
       height = 8.77,
       width = 9.19,
       path = "/data/projects/project_pvartiai/rsv/results/plots/")







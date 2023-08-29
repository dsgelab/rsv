# review 4 further stepwise exclusion

ibrary(data.table)
library(feather)
library(dplyr)
library(tibble)
library(caret)
library(leaps)
library(tidyr)
library(lubridate)
library(stringr)
library(rms)
library(pROC)
library(MASS)










################################################################################################################################################################################
# load the data
################################################################################################################################################################################
predict_datapath <- "/data/projects/project_pvartiai/rsv/modeling/"
comp_datapath <- "/data/projects/project_pvartiai/rsv/predictors/"
val_path <- "/data/projects/project_pvartiai/rsv/validation_data/"


# load development dataset
# this contains the traditional outcome.
compact_data <- fread(paste0(predict_datapath, 
                             "development_set_only_feasible_variables.csv")) %>%
  as_tibble %>%
  # add birth month variable to the predict data 
  mutate(birth_month = as.numeric(month(LAPSEN_SYNTYMAPVM))) 

recent_data <- compact_data %>%
  filter(epidemic_year > 2006)


test_set <- fread(paste0(val_path, "validation_set_regression.csv")) %>%
  as_tibble()


# all_data <- bind_rows(recent_data, test_set)


# load model objects and knots and formula
setwd("/data/projects/project_pvartiai/rsv/functions/model/")

# original formula
final_form_simple <- get(load("/data/projects/project_pvartiai/rsv/functions/model/final_formula_object_nointeract.R"))
load("rcsknots_motherage.R")
load("rcsknots_dist.R") 
load("rcsknots_gestdays.R")
load("rcsknots_weightsd.R")

# original fit
visualise_fit <- get(load("final_model_fit.R"))

setwd("/data/projects/project_pvartiai/rsv/functions/")
load("keylist_final_variable_names.R")

# predictor names
keylist_final %>% print(n = 100)

keylist_final$longname[keylist_final$feature == "gest_days"] <- "Gestational age\nat birth, weeks"
keylist_final$longname[keylist_final$feature == "dist"] <- "Months from birth to\nnext epidemic peak"

## rcs fit
# set datadist
ddist <- datadist(recent_data)

# set the datadist option
options(datadist = "ddist")

# RCS fit
rcs_fit <- lrm(formula = final_form_simple, 
               data = recent_data,
               x = TRUE,
               y = TRUE)


################################################################################################################################################################################
# AIC of each variable
################################################################################################################################################################################

a <- fastbw(rcs_fit, aics = 10000)


a$factors.deleted


# vector of predictors. Order determined by previous fastBW
variable_order <- c("weight_sd", 
                    "q39_confirmed", 
                    "asd_or_vsd_only", 
                    "twin", 
                    "down", 
                    "mother_age", 
                    "term_breathing", 
                    "smoking_neodg", 
                    "male_gender", 
                    "any_severe_chd",
                    "any_family_asthma",
                    "sib_4_7",
                    "sib_resp_hosp",
                    "gest_days",
                    "sib_0_4",
                    "dist")

# same but with names for formula object
variable_order_formnames <- c("rcs(weight_sd, parms = rcsknots_weightsd)", 
                              "q39_confirmed", 
                              "asd_or_vsd_only", 
                              "twin", 
                              "down", 
                              "rcs(mother_age, parms = rcsknots_motherage)", 
                              "term_breathing", 
                              "smoking_neodg", 
                              "male_gender", 
                              "any_severe_chd",
                              "any_family_asthma",
                              "sib_4_7",
                              "sib_resp_hosp",
                              "rcs(gest_days, parms = rcsknots_gestdays)",
                              "sib_0_4",
                              "rcs(dist, parms = rcsknots_dist)")


# initialize result frame for the loop
auc_frame <- data.frame(added_var = c(variable_order),
                        auc = NA,
                        auc_high = NA,
                        auc_low = NA,
                        n_of_vars = rev(1:16))



# for testing the loop
temp_data <- recent_data %>% sample_n(10000)


for (i in 1:length(variable_order_formnames)) {
  
  temp_test_data <- test_set
  
  temp_form <- formula(
    paste("outcome ~", paste(variable_order_formnames[i:16], collapse = "+"))
  )
  
  temp_fit <- glm(data = recent_data, 
                  formula = temp_form,
                  family = binomial)
  
  temp_test_data$predict <- predict(temp_fit, newdata = temp_test_data, type = "response")
  
  temp_auc <- ci.auc(temp_test_data$outcome, temp_test_data$predict)
  
  auc_frame$auc[i] <- temp_auc[2]
  auc_frame$auc_high[i] <- temp_auc[3]
  auc_frame$auc_low[i] <- temp_auc[1]

  
}



auc_frame2 <- auc_frame %>% 
  left_join(., keylist_final, by = c("added_var" = "feature")) %>%
  mutate(label = paste0(longname, " (", n_of_vars, ")")) %>%
  mutate(label = as.factor(label)) %>%
  mutate(label = fct_reorder(label, n_of_vars))

step_plot <- auc_frame2 %>%
  ggplot(., aes(
    y = auc,
    ymin = auc_low,
    ymax = auc_high,
    x = label)) + 
  
      geom_point() + 
      geom_errorbar(width = 0.1) +
      labs(x = "Added predictor\n(n:o of predictors in model)",
           y = "C-statistic in Finnish test data") + 
  theme_bw(base_size = 14) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 14))


step_plot

ggsave(plot = step_plot,
       filename = "suppl_stepwise.pdf",
       device = "pdf",
       width = 14,
       height = 8,
       dpi = 1200,
       path = "/data/projects/project_pvartiai/rsv/results/plots/")
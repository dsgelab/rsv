
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


# load more rich data, mainly for later sensitivity analyses
# this data contains info on e.g. BPD and different severities of congenital heart defects
# predict_data <- fread(paste0(predict_datapath, "development_set_final_variables.csv"))

recent_data <- compact_data %>%
  filter(epidemic_year > 2006)

# # load composite data (Pre-AIC-data) if needed
# composite_data <- fread(paste0(comp_datapath, "composite_data_all_candidate_predictors.csv")) %>%
#   as_tibble

# ids and birth dates
all_bds <- fread("/data/projects/project_pvartiai/rsv/wrangle/preprocess_3_mbr_hilmo_outcomes.csv", 
                 select = c("TNRO", "LAPSEN_SYNTYMAPVM")) %>% as_tibble




#### LOAD FUNCTIONS

# get.yearly.aucs

get.yearly.aucs <- function(year, custom_formula, data) {
  
  # filter the training data; leave 1 epidemic year off for testing
  data_without_year <- data %>% 
    filter(epidemic_year != year)
  
  print(paste("training set filtered for", year, "fitting the model..."))
  
  # fit the model to the training set
  fit_without_year <- glm(data = data_without_year, 
                          family = binomial(),
                          formula = custom_formula)
  
  print(paste(year, "fit complete"))
  
  # filter testing data
  subsample_1 <- data %>%
    filter(epidemic_year == year)
  
  
  # predict
  predict <- predict(fit_without_year, 
                     newdata = subsample_1, 
                     type = "response")
  
  # logit predict
  logit <- predict(fit_without_year, 
                   newdata = subsample_1)
  
  # bind predictions to the testing data
  predicted_data <- cbind(subsample_1, predict, logit) %>%
    as_tibble %>%
    # create 20 bins according to predicted probability
    mutate(prob_bin = ntile(predict, 10))
  
  # save auc and confidence intervals
  auc <- auc(predicted_data$outcome, predicted_data$predict) %>% as.numeric
  auc_low <- ci.auc(predicted_data$outcome, predicted_data$predict)[1]
  auc_high <- ci.auc(predicted_data$outcome, predicted_data$predict)[3]
  
  # create a result_auc dataframe to save aucs
  result_auc <- data.frame(epidemic_year = year,
                           value = auc,
                           ci_low = auc_low,
                           ci_high = auc_high,
                           metric = "auc")
  
  #####
  ### CIL AND SLOPE 
  # calibration slope
  slope_fit <- glm(data = predicted_data,
                   formula = outcome ~ logit,
                   family = binomial())
  
  slope <- coef(summary(slope_fit))[2,1]
  se_slope <- coef(summary(slope_fit))[2,2]
  
  slope_df <- tibble(
    value = slope,
    ci_high = slope + 1.96*se_slope,
    ci_low = slope - 1.96*se_slope,
    epidemic_year = year,
    metric = "slope")
  
  
  # offset model for calibration in the large
  offset_fit <- glm(data = predicted_data,
                    formula = outcome ~ offset(1*logit),
                    family = binomial())
  
  cil <- coef(summary(offset_fit))[1]
  se_cil <- coef(summary(offset_fit))[2]
  
  cil_df <- tibble(
    value = cil,
    ci_high = cil + 1.96*se_cil,
    ci_low = cil - 1.96*se_cil,
    epidemic_year = year,
    metric = "cil")
  
  
  ###
  #####
  
  
  
  
  result_metrics <- bind_rows(result_auc, slope_df, cil_df)
  
  # create a result_calib dataframe to save mean predicted and mean observed probabilities
  result_calib <- predicted_data %>%
    # group by binds
    group_by(prob_bin) %>%
    # calculate mean predicted and mean observed probs
    summarise(mean_pred = mean(predict, na.rm = TRUE),
              mean_obs = mean(outcome, na.rm = TRUE)) %>%
    ungroup %>%
    # select only calibration variables
    select(mean_pred, mean_obs) %>%
    # create a label for the epidemic year
    mutate(epidemic_year = year)
  
  # combine results into dataframe
  result <- left_join(result_calib, result_metrics, by = "epidemic_year")
  
  # return the result dataframe
  result
  
}


# epidemic.cv.loop
epidemic.cv.loop <- function(years, formula_object, dataset) {
  
  #prime the result data frame for the loop by running get.yearly.aucs in the first year  
  result_frame <- get.yearly.aucs(year = years[1],
                                  custom_formula = formula_object,
                                  data = dataset)
  
  # loop a function through the next years 
  for (i in 2:length(years)) {    
    this_year <- years[i]
    
    # function takes year and formula as arguments
    temp_results <- get.yearly.aucs(year = this_year, 
                                    # replace formula here
                                    custom_formula = formula_object,
                                    # use only more recent epidemic data
                                    data = dataset)
    
    result_frame <- bind_rows(result_frame, temp_results)
    print(paste(this_year, "auc and calib obtained!"))
  }
  return(result_frame)
}



# model object and stuff
setwd("/data/projects/project_pvartiai/rsv/functions/model/")

final_form_simple <- get(load("/data/projects/project_pvartiai/rsv/functions/model/final_formula_object_nointeract.R"))
evenodd_form <- update(final_form_simple, ~ . + even_epidemic_year)

load("rcsknots_motherage.R")
load("rcsknots_dist.R")
load("rcsknots_gestdays.R")
load("rcsknots_weightsd.R")
# whole model object 
model_object <- get(load("/data/projects/project_pvartiai/rsv/functions/model/final_model_fit.R"))


















##### SWEDISH  RESULTS

##### LOAD SWEDISH AUC RESULTS
swed_path <- "/data/projects/project_pvartiai/rsv/results/sweden/"
auc_swe_yearly <- get(load(paste0(swed_path, "yearly_auc.R")))

# overall_auc_3last <- get(load(paste0(swed_path, "auc_3_last.R")))

# overall_auc_swe <- tibble(
#   auc = overall_auc_3last[2],
#   ci_high = overall_auc_3last[3],
#   ci_low = overall_auc_3last[1],
#   epidemic_year = "All test\ndata",
#   Country = "Sweden")

indiv_auc_swe <- auc_swe_yearly %>%
  filter(epidemic_year %in% 2007:2017) %>%
  mutate(Country = "Sweden",
         epidemic_year = as.character(epidemic_year)) %>%
  rename(ci_low = ci_low_auc,
         ci_high = ci_auc_high,
         value = auc) %>%
  mutate(se_auc = (ci_high - value)/1.96) %>%
  mutate(metric = "auc")

meta_swe <- rma(value,
                sei = se_auc,
                data = indiv_auc_swe)

rma_swe <- tibble(
  value = meta_swe$b %>% as.numeric,
  ci_low = meta_swe$ci.lb,
  ci_high = meta_swe$ci.ub,
  epidemic_year = "Random\neffect\nmeta-analysis",
  Country = "Sweden",
  metric = "auc")

auc_swe_yearly <- bind_rows(indiv_auc_swe, rma_swe)



### CIL SWEDEN
cil_swe_yearly <- get(load(paste0(swed_path, "yearly_intercept.R")))


indiv_cil_swe <- cil_swe_yearly %>%
  filter(epidemic_year %in% 2007:2017) %>%
  rename(value = cil) %>%
  mutate(Country = "Sweden",
         epidemic_year = as.character(epidemic_year)) %>%
  mutate(se_cil = (ci_high - value)/1.96) %>%
  mutate(metric = "cil")

meta_swe <- rma(value,
                sei = se_cil,
                data = indiv_cil_swe)

rma_swe <- tibble(
  value = meta_swe$b %>% as.numeric,
  ci_low = meta_swe$ci.lb,
  ci_high = meta_swe$ci.ub,
  epidemic_year = "Random\neffect\nmeta-analysis",
  Country = "Sweden",
  metric = "cil")

cil_swe_yearly <- bind_rows(indiv_cil_swe, rma_swe)



#### SLOPE


slope_swe_yearly <- get(load(paste0(swed_path, "yearly_slope.R")))


indiv_slope_swe <- slope_swe_yearly %>%
  filter(epidemic_year %in% 2007:2017) %>%
  rename(value = slope) %>%
  mutate(Country = "Sweden",
         epidemic_year = as.character(epidemic_year)) %>%
  mutate(se_slope = (ci_high - value)/1.96) %>%
  mutate(metric = "slope")


meta_swe <- rma(value,
                sei = se_slope,
                data = indiv_slope_swe)

rma_swe <- tibble(
  value = meta_swe$b %>% as.numeric,
  ci_low = meta_swe$ci.lb,
  ci_high = meta_swe$ci.ub,
  epidemic_year = "Random\neffect\nmeta-analysis",
  Country = "Sweden",
  metric = "slope")

slope_swe_yearly <- bind_rows(indiv_slope_swe, rma_swe)



## swedish calib
swedish_calib <- get(load(paste0(swed_path, "yearly_calib.R"))) %>%
  filter(epidemic_year %in% 2007:2017) %>%
  mutate(Country = "Sweden")





















### AUC IN FINNISH TRAIN


# data
recent_data

# formulas

final_form_simple

basic_simple_fit <- epidemic.cv.loop(
  formula_object = final_form_simple,
  years = 2007:2017,
  data = recent_data)



simple_auc <- basic_simple_fit %>%
  # filter for AUC metric
  filter(metric == "auc") %>%
  select(epidemic_year, value, ci_low, ci_high, metric) %>%
  distinct() %>%
  mutate(epidemic_year = as.character(epidemic_year))

simple_auc$Country <- "Finland"

rma_auc <- simple_auc %>%
  mutate(se = (ci_high-value)/1.96) %>%
  select(epidemic_year, value, se)

meta <- rma(value, sei = se, data = rma_auc)


meta_auc <- data.frame(value = meta$b,
                       ci_high = meta$b + 1.96*meta$se,
                       ci_low = meta$b - 1.96*meta$se,
                       epidemic_year = "Random\neffect\nmeta-analysis",
                       Country = "Finland",
                       metric = "auc") %>%
  as_tibble()


finnish_aucs <- simple_auc %>%
  mutate(epidemic_year = as.character(epidemic_year)) %>%
  bind_rows(., meta_auc) 


#### Finnish CIL
simple_cil <-  basic_simple_fit %>%
  # filter for AUC metric
  filter(metric == "cil") %>%
  select(epidemic_year, value, ci_low, ci_high, metric) %>%
  distinct() %>%
  mutate(epidemic_year = as.character(epidemic_year))

simple_cil$Country <- "Finland"

rma_cil <- simple_cil %>%
  mutate(se = (ci_high-value)/1.96) %>%
  select(epidemic_year, value, se)

meta <- rma(value, sei = se, data = rma_cil)


meta_cil <- data.frame(value = meta$b,
                       ci_high = meta$b + 1.96*meta$se,
                       ci_low = meta$b - 1.96*meta$se,
                       epidemic_year = "Random\neffect\nmeta-analysis",
                       Country = "Finland",
                       metric = "cil") %>%
  as_tibble()


finnish_cil <- simple_cil %>%
  mutate(epidemic_year = as.character(epidemic_year)) %>%
  bind_rows(., meta_cil) 

#### Finnish slope

simple_slope <- basic_simple_fit %>%
  # filter for AUC metric
  filter(metric == "slope") %>%
  select(epidemic_year, value, ci_low, ci_high, metric) %>%
  distinct() %>%
  mutate(epidemic_year = as.character(epidemic_year))

simple_slope$Country <- "Finland"

rma_slope <- simple_slope %>%
  mutate(se = (ci_high-value)/1.96) %>%
  select(epidemic_year, value, se)

meta <- rma(value, sei = se, data = rma_slope)


meta_slope <- data.frame(value = meta$b,
                         ci_high = meta$b + 1.96*meta$se,
                         ci_low = meta$b - 1.96*meta$se,
                         epidemic_year = "Random\neffect\nmeta-analysis",
                         Country = "Finland",
                         metric = "slope") %>%
  as_tibble()


finnish_slope <- simple_slope %>%
  mutate(epidemic_year = as.character(epidemic_year)) %>%
  bind_rows(., meta_slope)


finnish_calib <- basic_simple_fit %>%
  select(mean_pred, mean_obs, epidemic_year) %>%
  mutate(Country = "Finland")





### Finnish metrics
finnish_aucs
finnish_cil
finnish_slope



# Swedish metrics
auc_swe_yearly
cil_swe_yearly
slope_swe_yearly


country_auc <- bind_rows(finnish_aucs, auc_swe_yearly) %>%
  select(epidemic_year, value, ci_low, ci_high, metric, Country)


country_cil <- bind_rows(finnish_cil, cil_swe_yearly) %>%
  select(epidemic_year, value, ci_low, ci_high, metric, Country)

country_slope <- bind_rows(finnish_slope, slope_swe_yearly) %>%
  select(epidemic_year, value, ci_low, ci_high, metric, Country)

# position, common for all metric plots
pos <- position_dodge(width = 0.2)



# auc plot

country_auc_plot <- country_auc %>%
  ggplot(aes(
    x = epidemic_year,
    y = value, 
    ymin = ci_low,
    ymax = ci_high,
    col = Country
  )) + 
  geom_point(position = pos,
             size = 1.5) + 
  geom_errorbar(position = pos,
                width = 0.2) + 
  theme_bw(base_size = 14) + 
  scale_color_manual(values =c("#9fbfd4", "#D6C533")) + 
  geom_vline(xintercept = 11.5, col = "darkgrey") + 
  theme(legend.position = "top") + 
  labs(y = "C-statistic",
       x = NULL)

country_auc_plot

# cil plot
country_cil %>% View()

country_cil_plot <- country_cil %>%
  ggplot(aes(
    x = epidemic_year,
    y = value, 
    ymin = ci_low,
    ymax = ci_high,
    col = Country
  )) + 
  geom_point(position = pos,
             size = 1.5) + 
  geom_errorbar(position = pos,
                width = 0.2) + 
  theme_bw(base_size = 14) + 
  scale_color_manual(values =c("#9fbfd4", "#D6C533")) + 
  geom_vline(xintercept = 11.5, col = "darkgrey") + 
  geom_hline(yintercept = 0, linetype = 2, size = 0.8) + 
  guides(color = FALSE) + 
  labs(y = "Calibration-in-the-large",
       x = NULL)

country_cil_plot

# slope plot

country_slope_plot <- country_slope %>%
  ggplot(aes(
    x = epidemic_year,
    y = value, 
    ymin = ci_low,
    ymax = ci_high,
    col = Country
  )) + 
  geom_point(position = pos,
             size = 1.5) + 
  geom_errorbar(position = pos,
                width = 0.2) + 
  theme_bw(base_size = 14) + 
  scale_color_manual(values =c("#9fbfd4", "#D6C533")) + 
  geom_vline(xintercept = 11.5, col = "darkgrey") + 
  geom_hline(yintercept = 1, linetype = 2, size = 0.8) + 
  guides(color = FALSE) + 
  labs(y = "Calibration slope",
       x = NULL)

country_slope_plot

my_plotlist <- list(country_auc_plot,
                    country_cil_plot,
                    country_slope_plot)

my_layout <- cbind(c(1,1,2,3))




# the plot
grid.arrange(grobs = my_plotlist, layout_matrix = my_layout)
g <- arrangeGrob(grobs = my_plotlist, layout_matrix = my_layout)

# size
# https://localhost:8443/rstudio/graphics/plot_zoom_png?width=1027&height=848

ggsave(g,
       filename = "suppl_indiv_years_metrics.pdf",
       path = "/data/projects/project_pvartiai/rsv/results/plots/",
       device = "pdf",
       dpi = 1200,
       height = 8.48,
       width = 10.27)



# Calib plots from 2 countries
swedish_calib
finnish_calib

all_calib <- bind_rows(swedish_calib, finnish_calib)



calib_plot <- all_calib %>%
  ggplot(., aes(
    x = mean_pred,
    y = mean_obs,
    group = factor(epidemic_year),
    col = Country)) + 
  geom_point(col = "black") +
  
  stat_smooth(span = 0.7,
              se = FALSE,
              size = 0.6, 
              inherit.aes = TRUE,
              linetype = 1) + 
  
  geom_abline(slope = 1, intercept = 0, linetype = 2) + 
  labs(x = "Predicted probability",
       y = "Observed outcome rate") + 
  
  scale_color_manual(values =c("#9fbfd4", "#D6C533")) +
  
  scale_y_continuous(expand = c(0,0),
                     limits = c(-0.001, 0.079)) + 
  scale_x_continuous(expand = c(0,0),
                     limits = c(-0.001, 0.079)) +
  scale_linetype_manual(values = c(1, 2)) + 
  guides(linetype=FALSE) + 
  
  facet_wrap(~Country) +
  
  theme_bw(base_size = 14) + 
  guides(color = FALSE) + 
  labs(subtitle = "A) Calibration in individual RSV epidemic years")



# the plot
calib_plot

# size
# https://localhost:8443/rstudio/graphics/plot_zoom_png?width=1185&height=722

ggsave(calib_plot,
       filename = "suppl_calib_plot.pdf",
       path = "/data/projects/project_pvartiai/rsv/results/plots/",
       device = "pdf",
       dpi = 1200,
       height = 7.22,
       width = 11.85)








### ALL CALIBRATION SUMMARISED

# function
se <- function(x) sqrt(var(x)/length(x))

# models
final_form_simple <- get(load("/data/projects/project_pvartiai/rsv/functions/model/final_formula_object_nointeract.R"))
load("rcsknots_motherage.R")
load("rcsknots_dist.R") 
load("rcsknots_gestdays.R")
load("rcsknots_weightsd.R")
model_fit <- get(load("final_model_fit.R"))


# test set
val_path <- "/data/projects/project_pvartiai/rsv/validation_data/"
test_set <- fread(paste0(val_path, "validation_set_regression.csv")) %>%
  as_tibble()

all_data <- bind_rows(recent_data, test_set)

all_data$predict <- predict(model_fit, newdata = all_data, type = "response")


fin_overall_calib <- all_data %>%
  select(outcome, predict) %>%
  mutate(prob_bin = ntile(predict, 10)) %>%
  group_by(prob_bin) %>%
  summarise(mean_pred = mean(predict, na.rm = T),
            mean_obs = mean(outcome, na.rm = T)) %>%
  na.omit() %>%
  mutate(Country = "Finland") 

#
swed_path

swe_overall_calib <- get(load(paste0(swed_path, "overall_calib.R"))) %>%
  na.omit() %>%
  mutate(Country = "Sweden")

all_overall_calib <- bind_rows(fin_overall_calib, swe_overall_calib)

overall_calib_plot <- all_overall_calib %>%
  ggplot(., aes(
    x = mean_pred,
    y = mean_obs,
    col = Country)) + 
  geom_point(col = "black") +
  
  stat_smooth(span = 1,
              se = FALSE,
              size = 1,
              linetype = 1,
              inherit.aes = TRUE) + 
  geom_abline(slope = 1, intercept = 0, linetype = 2) + 
  
  labs(x = "Predicted probability",
       y = "Observed outcome rate") + 
  
  scale_color_manual(values =c("#9fbfd4", "#D6C533")) +
  
  scale_y_continuous(expand = c(0,0),
                     limits = c(-0.001, 0.079)) + 
  scale_x_continuous(expand = c(0,0),
                     limits = c(-0.001, 0.079)) +
  scale_linetype_manual(values = c(1, 2)) + 
  guides(linetype=FALSE) + 
  
  facet_wrap(~Country) +
  
  theme_bw(base_size = 14) + 
  guides(color = FALSE) + 
  labs(subtitle = "B) Calibration in pooled data from epidemics 2007-2020")

overall_calib_plot

my_plotlist <- list(calib_plot, overall_calib_plot)
my_layout <- rbind(1,2)

grid.arrange(grobs = my_plotlist, layout_matrix = my_layout)

g <- arrangeGrob(grobs = my_plotlist, layout_matrix = my_layout)

# size:
# https://localhost:8443/rstudio/graphics/plot_zoom_png?width=1076&height=956


ggsave(calib_plot,
       filename = "suppl_calib_plot_with_ovearll.pdf",
       path = "/data/projects/project_pvartiai/rsv/results/plots/",
       device = "pdf",
       dpi = 1200,
       height = 10,
       width = 11)


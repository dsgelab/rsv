#### SCRIPT TO TEST THE VALIDITY OF RSV PREDICTION MODEL IN THE SWEDISH DATA

# First notes: 
# - it is possible that there are problems with the date variables when loading the data to R, if the data is from STATA. So it's good to check that.
# - Date variables are quite important for the classification of the results, they should be in R's date format. 
# - if any problems, do not hesitate to contact Pekka Vartiainen!


## REQUIREMENTS
# Following thigs should be in the data so that this script works.
# - birth date, named as "birth_date". 
# - outcome date, named as "outcome_date", meaning the date of RSV hospitalisation.
# - variables should be named as in here: https://docs.google.com/spreadsheets/d/1yNZhB1EFCURMLML-zeOcIdy47iuQvdL8bTjOtVyO2yI/edit#gid=163265107
# - The birth dates should be in between 1.6.2006 - 31.5.2020 (this is tested in the script)


# about the results
#
# The script produces several results
#   - "table 1", summary statistics
#   - discrimination (AUC) and calibration (calibration data according to the deciles, calibration-in-the-large, calibration slope) for
#       - the whole validation data
#       - for the 3 last epidemic years (2018-2020)
#       - For each epidemic year.
#   - Histogram of predicted probabilities
#   - Decision curve analysis object
#   - A model object where coefficients can be examined
#   - Monthly outcome rates
# all results - also the model object - of this script are aggregate data. No individual-level data in any form will be saved or exported. 



#### SET THESE FILE PATHS
# for example: "/home/Users/downloads/"

# where the data is stored
path_to_data <- "..."

# location of the model objects we sent
model_path <- "..."

# The place where the results should be saved
results_dir <- "..."


# load libraries

library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(forcats)
library(ggplot2)
library(pROC)
library(rms)
library(dcurves)
library(data.table)
library(arsenal)

## If some packages are not installed, they can be installed with the command
# install.packages("dplyr")
# install.packages("tidyr")
# install.packages("lubridate")
# install.packages("stringr")
# install.packages("forcats")
# install.packages("ggplot2")
# install.packages("dcurves")
# install.packages("pROC")
# install.packages("rms")
# install.packages("data.table")
# install.packages("arsenal")

# function for standard error
get.se <- function(x) {sqrt(var(x) / length(x))}

##### The data
path_to_data 

data <- fread(path_to_data) %>% 
  # change the data format to tidyverse form
  as_tibble %>%
  # just in case, desect row number variable
  select(-any_of("V1"))

## some checks for the data
# test if file names are same than in the model objects
var_names <- c(
  "outcome",
  "gest_days",
  "dist",
  "weight_sd",
  "mother_age",
  "twin",
  "male_gender",
  "sib_0_4",
  "sib_4_7",
  "down",
  "sib_resp_hosp",
  "smoking_neodg",
  "term_breathing",
  "q39_confirmed",
  "any_family_asthma",
  "any_severe_chd",
  "asd_or_vsd_only",
  "bpd")

# are all 17 variable names included in the data?


if(sum(var_names %in% colnames(data)) != 18) {
  stop("Variable names might be different or all variables are not included")
}


# if TRUE, everything ok!
# if FALSE, kindly check that the variable names match the previous list. 

## Birth date checks
# does the data contain a birth_date variable?
# The birth date is needed to group the results for exporting.
if(!("birth_date" %in% colnames(data))){
  stop("Birth date not included or variable has a different name")
}

# Are the birth dates between 1.6.2007 - 31.5.2020
# This should output an empty data. All birth dates should be in the range.
# This should reveal if there are problems with the date variables.
if(nrow(data %>%
        filter(!between(birth_date, 
                        lower = as.Date("2006-06-01"),
                        upper = as.Date("2020-05-31")))) != 0) {
  stop("Birth dates are outside 1.6.2007 - 31.5.2020")
}

# Create a variable "epidemic_year" to the data based on the child's birth date, 
# that indicates the year of the RSV epidemic a child is predisposed to. 
# Cutoff for this epidemic year is june

data <- data %>%
  mutate(birth_year = year(birth_date)) %>%
  mutate(epidemic_year = ifelse(month(birth_date) >= 6, birth_year +1, birth_year)) %>%
  select(-birth_year)




#### SUMMARY TABLE
data

table_data <- data %>%
  mutate(gw_cat = case_when(
    gest_days/7 >= 37 ~ "Term (>37)",
    gest_days/7 < 37 & gest_days/7 >= 33 ~ "33-37",
    gest_days/7 < 33 & gest_days/7 >= 29 ~ "29-33",
    gest_days/7 < 29 ~ "<29",
    is.na(gest_days) ~ NA_character_,
    TRUE ~ "something else?"
  )) %>%
  mutate(weight_cat = case_when(
    weight_sd < -2 ~ "<-2 SD",
    weight_sd >= -2 & weight_sd <= 2 ~ "Within 2SD",
    weight_sd > 2 ~ ">+2 SD",
    is.na(weight_sd) ~ NA_character_,
    TRUE ~ "something else?"
  )) %>%
  mutate(across(c(outcome, 
                  male_gender, 
                  weight_cat, 
                  twin, 
                  sib_0_4, 
                  sib_4_7, 
                  down, 
                  sib_resp_hosp,
                  smoking_neodg,
                  term_breathing,
                  q39_confirmed, 
                  any_family_asthma, 
                  any_severe_chd, 
                  asd_or_vsd_only), .fn = ~ as.factor(.)))



mycontrols  <- tableby.control(test=FALSE, 
                               total=TRUE,
                               numeric.stats = c("mean", "sd", "median", "q1q3", "range", "Nmiss"),
                               cat.stats = c("countpct", "Nmiss"),
                               cat.simplify = FALSE)

table_obj <- table_data %>%
  tableby(data = .,
          control = mycontrols,
          ~ outcome + gw_cat + weight_cat + mother_age +  male_gender + 
            twin + sib_0_4 + sib_4_7 + down + sib_resp_hosp + smoking_neodg + 
            term_breathing + q39_confirmed + any_family_asthma + any_severe_chd + asd_or_vsd_only)

table_print <- table_obj %>%
  summary(text = NULL,
          digits.pct = 3) %>% as_tibble(.name_repair = "minimal") 

names(table_print)[1] <- "feature"










#### THE VALIDATION SECTION


#### load model objects
# SET THE PATH ACCORDING TO WHERE THE MODEL OBJECTS ARE STORED
model_path

setwd(model_path)

# load spline knot locations, a parameter for the model fit 
load("rcsknots_motherage.R")
load("rcsknots_dist.R") 
load("rcsknots_gestdays.R")
load("rcsknots_weightsd.R")

# load the model fit sent to you
model_fit <- get(load(paste0(model_path, "exportable_model_fit.R")))


#### predict with the model object
# This gives the predicted probabilities for each id
data$prediction <- predict(model_fit, newdata = data, type = "response")
data$logit <- predict(model_fit, newdata = data)


# last 3 years of the data, using the same years than in Finnish test set
data_3_last <- data %>%
  filter(epidemic_year %in% c(2018:2020))


# histogram of predicted probabilities

density_df <- data.frame(
  epidemic_year = NULL,
  prediction = NULL,
  density = NULL)

# loop to get density curves for each epidemic year
for(i in 2007:2020) {
  
  temp_pred <- data %>%
    filter(epidemic_year == i) %>%
    pull(prediction)

# skip iteration if empty vector
  if(length(temp_pred) == 0) {
    print(paste0("Skipping ", i, " because there's no data"))
    next
  }

  temp_density <- density(temp_pred, na.rm = T)
  
  temp_density_df <- data.frame(
    prediction = temp_density$x,
    density = temp_density$y,
    epidemic_year = i
  )

  density_df <- bind_rows(density_df, temp_density_df)
  
}

# final result frame of the density data named as "hist"
hist <- density_df



# overall auc in the whole data
overall_auc <- as.numeric(ci.auc(data$outcome, data$prediction))
auc_3_last <- as.numeric(ci.auc(data_3_last$outcome, data_3_last$prediction))

# overall calibration in the whole data
overall_calib <- data %>%
  mutate(prob_bin = ntile(prediction, 10)) %>%
  group_by(prob_bin) %>%
  summarise(mean_pred = mean(prediction, na.rm = T),
            mean_obs = mean(outcome, na.rm = T), 
            n = n())

# overall calibration in 3 last years' data
calib_3_last <- data_3_last %>%
  mutate(prob_bin = ntile(prediction, 10)) %>%
  group_by(prob_bin) %>%
  summarise(mean_pred = mean(prediction, na.rm = T),
            mean_obs = mean(outcome, na.rm = T), 
            n = n())


# calibration slope and intercept in the whole data

# slope
slope_fit <- glm(data = data,
                 formula = outcome ~ logit,
                 family = binomial())

slope <- coef(summary(slope_fit))[2,1]
se_slope <- coef(summary(slope_fit))[2,2]

overall_slope <- tibble(
  slope = slope,
  ci_high = slope + 1.96*se_slope,
  ci_low = slope - 1.96*se_slope)


# intercept, calibration-in-the-large
offset_fit <- glm(data = data,
                  formula = outcome ~ offset(1*logit),
                  family = binomial())

cil <- coef(summary(offset_fit))[1]
se_cil <- coef(summary(offset_fit))[2]

overall_cil <- tibble(
  cil = cil,
  ci_high = cil + 1.96*se_cil,
  ci_low = cil - 1.96*se_cil)




# calibration slope and intercept in the 3 last years' data

# slope
slope_fit_3last <- glm(data = data_3_last,
                       formula = outcome ~ logit,
                       family = binomial())

slope_3last <- coef(summary(slope_fit_3last))[2,1]
se_slope_3last <- coef(summary(slope_fit_3last))[2,2]

overall_slope_3last <- tibble(
  slope = slope_3last,
  ci_high = slope_3last + 1.96*se_slope_3last,
  ci_low = slope_3last - 1.96*se_slope_3last)


# intercept, calibration-in-the-large
offset_fit_3last <- glm(data = data_3_last,
                        formula = outcome ~ offset(1*logit),
                        family = binomial())

cil_3last <- coef(summary(offset_fit_3last))[1]
se_cil_3last <- coef(summary(offset_fit_3last))[2]

overall_cil_3last <- tibble(
  cil = cil_3last,
  ci_high = cil_3last + 1.96*se_cil_3last,
  ci_low = cil_3last - 1.96*se_cil_3last)




# a loop to extract auc and calibration values for each epidemic year.
#
# this is done so that we can do meta-analyses 
# from the yearly values and the standard deviations
#
#

# create empty sets for the loop
yearly_auc <- NULL
yearly_calib <- NULL
yearly_intercept <- NULL
yearly_slope <- NULL


# the actual loop
for(i in 2007:2020) {
  
  
  data_for_year <- data %>%
    filter(epidemic_year == i)
  
  # if one of the epidemic years has no data, the loop skips to next year
  if(nrow(data_for_year) == 0) {
    print(paste0("Skipping ", i, " because there's no data"))
    next
  }
  
  # auc for each year
  # we will also count the overall n and n_hospitalised
  this_year_auc <- data_for_year %>%
    summarise(auc = as.numeric(auc(outcome, prediction)),
              ci_low_auc = ci.auc(outcome, prediction)[1],
              ci_auc_high = ci.auc(outcome, prediction)[3],
              epidemic_year = i,
              n_overall = n(),
              n_hosp = sum(outcome, na.rm = T))
  
  # bind the yearly results to the summary table 
  yearly_auc <- bind_rows(yearly_auc, this_year_auc)
  
  # calib for each year
  this_year_calib <- data_for_year %>%
    mutate(prob_bin = ntile(prediction, 10)) %>%
    group_by(prob_bin) %>%
    summarise(mean_pred = mean(prediction, na.rm = T),
              mean_obs = mean(outcome, na.rm = T), 
              ci_obs = 1.96*get.se(outcome),
              n = n()) %>%
    mutate(epidemic_year = i) %>%
    mutate(ci_obs_high = mean_obs + ci_obs,
           ci_obs_low = mean_obs - ci_obs) %>%
    select(-ci_obs)
  
  # bind the yearly results to the summary table
  yearly_calib <- bind_rows(yearly_calib, this_year_calib)
  
  
  # calibration slope
  slope_fit <- glm(data = data_for_year,
                   formula = outcome ~ logit,
                   family = binomial())
  
  slope <- coef(summary(slope_fit))[2,1]
  se_slope <- coef(summary(slope_fit))[2,2]
  
  slope_df <- tibble(
    slope = slope,
    ci_high = slope + 1.96*se_slope,
    ci_low = slope - 1.96*se_slope,
    epidemic_year = i)
  
  # bind results to the existing results data  
  yearly_slope <- bind_rows(yearly_slope, slope_df)
  
  # offset model for calibration in the large
  offset_fit <- glm(data = data_for_year,
                    formula = outcome ~ offset(1*logit),
                    family = binomial())
  
  cil <- coef(summary(offset_fit))[1]
  se_cil <- coef(summary(offset_fit))[2]
  
  cil_df <- tibble(
    cil = cil,
    ci_high = cil + 1.96*se_cil,
    ci_low = cil - 1.96*se_cil,
    epidemic_year = i)
  
  # bind the results to the existing results data
  yearly_intercept <- bind_rows(yearly_intercept, cil_df)
  
}



#### Decision curve analysis object

dca_data <- data %>%
  filter(epidemic_year %in% 2018:2020) %>%
  # create an aap variable 
  mutate(aap_criterion = case_when(
    gest_days < 203 ~ 1,
    
    # what's the name of the bpd variable
    bpd == 1 ~ 1,
    
    any_severe_chd == 1 ~ 1,
    TRUE ~ 0
  )) %>% select(outcome, prediction, aap_criterion)




dca_object <- dca(formula = outcome ~ prediction + aap_criterion,
                  data = dca_data,
                  thresholds = seq(0, 0.15, by = 0.001)) %>%
  as_tibble %>%
  mutate(Country = "Sweden")









#### RESULTS

# do a list for complete results, mainly for saving
results_list <- list(
  # auc in the complete validation data
  overall_auc = overall_auc,
  auc_3_last = auc_3_last,
  # calibration in the complete validation data
  overall_calib = overall_calib,
  calib_3_last = calib_3_last,
  # decision curve dataframe
  dca_object = dca_object,
  # histogram of predicted probabilities
  hist = hist,
  # overall calibration intercept
  overall_cil = overall_cil,
  overall_cil_3last = overall_cil_3last,
  # overall calibration slope
  overall_slope = overall_slope,
  overall_slope_3last = overall_slope_3last,
  ### yearly data
  # auc in each epidemic year
  yearly_auc = yearly_auc,
  # calibration in each epidemic year
  yearly_calib = yearly_calib,
  # Calibration intercept each year
  yearly_intercept = yearly_intercept, 
  # calibration slope each year
  yearly_slope = yearly_slope,
  # table object
  table_print = table_print)


# set the place for saving the files
setwd(results_dir)

# loop for saving all results as R objects and CSV objects, just in case
for(i in 1:length(results_list)) {
  
  temp_object <- results_list[[i]]
  
  # save R object
  save(temp_object, 
       file = paste0(names(results_list)[i], ".R"))
  
  # save csv file
  write.csv(temp_object, 
            file = paste0(names(results_list)[i], ".csv"),
            row.names = FALSE)
}





###### MODEL FITTING AND COEFFICIENTS IN THE SWEDISH DATA

# data for 2007-2017
data_train <- data %>%
  filter(epidemic_year %in% c(2007:2017))

## set the spline knot locations
knots_gestdays_sweden  <- with(data_train, c(203, 238, rcspline.eval(gest_days, nk = 4, knots.only = TRUE))) 
knots_motherage_sweden <- with(data_train, c(rcspline.eval(mother_age, nk = 4, knots.only = TRUE))) 
knots_dist_sweden <- with(data_train, c(rcspline.eval(dist, nk = 4, knots.only = TRUE))) 
knots_weightsd_sweden <- with(data_train, c(rcspline.eval(weight_sd, nk = 4, knots.only = TRUE))) 



## formula object
form <- formula(outcome ~ rcs(gest_days, parms = knots_gestdays_sweden) + 
                  rcs(dist, parms = knots_dist_sweden) + 
                  rcs(mother_age, parms = knots_motherage_sweden) + 
                  rcs(weight_sd, parms = knots_weightsd_sweden) + 
                  twin + male_gender + sib_0_4 + sib_4_7 + down + sib_resp_hosp + smoking_neodg + 
                  term_breathing + q39_confirmed + any_family_asthma + any_severe_chd + asd_or_vsd_only)



# train the model
new_model_fit <- glm(data = data_train,
                     formula = form,
                     family = binomial())


# Coefficients
coefficients <- coef(summary(new_model_fit)) %>%
  as.data.frame %>%
  rownames_to_column(var = "feature") %>%
  as_tibble() %>%
  rename(coef = "Estimate",
         se = "Std. Error", 
         p = "Pr(>|z|)") %>%
  select(feature, coef, se, p)

setwd(results_dir)
save(coefficients, file = "coefs_in_sweden.R")


## save the model object without individual level data
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

export_object <- strip.glm(new_model_fit)

setwd(results_dir)
save(export_object, file = "exportable_model_fit_sweden.R")



# monthly outcome rate
monthly_outcomes <- data %>%
  filter(outcome == 1) %>%
  mutate(outcome_month = month(outcome_date),
         outcome_year = year(outcome_date)) %>%
  group_by(outcome_month, outcome_year) %>%
  summarise(outcome_rate_sweden = n()) 


save(monthly_outcomes, file = "monthly_outcomes_sweden.R")
write.csv(monthly_outcomes, "monthly_outcomes_sweden.csv",
          row.names = FALSE)



##### PERCENTILE CUTOFFS

# count min, max and mean for percentiles of predicted probabilities
quant <- quantile(data$prediction, na.rm = T, probs = seq(0,1,0.01)) %>%
  as.data.frame() %>%
  rename(percentile_cutoff = ".") %>%
  mutate(percentile = 0:100)

percentile_table <- data %>%
  mutate(percentile = ntile(prediction, 100)) %>%
  group_by(percentile) %>%
  summarise(mean = mean(prediction, na.rm = T),
            low = min(prediction),
            high = max(prediction)) %>%
  left_join(quant)

setwd(results_dir)
save(percentile_table, file = "percentile_cutoffs_of_predictions.R")
write.csv(percentile_table, "percentile_cutoffs_of_predictions.csv",
          row.names = FALSE)






#### NNT analyses ####

# filter a subset of data
nnt_data <- data %>%
  filter(epidemic_year %in% 2018:2020)

# vector of cutoff probabilities for each percentile
cutoff <- nnt_data$prediction %>%
  quantile(., probs = seq(0,1,0.01), na.rm = T)

cutoff_perc <- seq(0,1,0.01)
cutoff_perc[101] <- 0


# initialize the loop, create empty result frame
result_frame <- tibble(
  n_treated = NA,
  percent_vaccinated = NA,
  nnt = NA,
  epidemic_year = NA
)


# prophylaxis efficacy
efficacy <- 0.6

for(i in 1:100) {
  
  temp_cutoff <- cutoff[i+1]
  temp_perc_vacc <- (1-cutoff_perc[i+1])*100
  
  n_treated_all <- nnt_data %>%
    filter(prediction >= temp_cutoff) %>%
    nrow()
  
  n_hosp_all_above_cutoff <- nnt_data %>%
    mutate(top_n = ifelse(prediction >= temp_cutoff, 1, 0)) %>%
    filter(top_n == 1) %>%
    group_by(outcome) %>%
    summarise(n = n()) %>%
    filter(outcome == 1) %>%
    pull(n)
  
  n_prevented_all <- n_hosp_all_above_cutoff*0.6  
  nnt_all <- n_treated_all/n_prevented_all
  
  temp_all_result_frame <- data.frame(
    n_treated = n_treated_all,
    percent_vaccinated = temp_perc_vacc,
    nnt = nnt_all,
    epidemic_year = "combined 3 years")
  
  result_frame <- bind_rows(result_frame, temp_all_result_frame)
  
  for(j in 1:3) {
    
    epidemic_year_j <- c(2018:2020)[j]
    
    n_treated <- nnt_data %>%
      filter(epidemic_year == epidemic_year_j) %>%
      filter(prediction >= temp_cutoff) %>%
      nrow()
    
    n_hosp_in_kids_above_cutoff <- nnt_data %>%
      filter(epidemic_year == epidemic_year_j) %>%
      mutate(top_n = ifelse(prediction >= temp_cutoff, 1, 0)) %>%
      filter(top_n == 1) %>%
      group_by(outcome) %>%
      summarise(n = n()) %>%
      filter(outcome == 1) %>%
      pull(n)
    
    prevented <- n_hosp_in_kids_above_cutoff*0.6
    
    nnt <- n_treated/prevented
    
    temp_result_frame <- data.frame(
      n_treated = n_treated,
      percent_vaccinated = temp_perc_vacc,
      nnt = nnt,
      epidemic_year = as.character(epidemic_year_j))
    
    result_frame <- bind_rows(result_frame, temp_result_frame)
    
  }  
  print(i)
  
}


overall_outcome_rate <- nnt_data %>%
  summarise(outcome_rate = mean(outcome, na.rm = T)) %>%
  mutate(epidemic_year = "combined 3 years")


outcome_rates <- nnt_data %>%
  group_by(epidemic_year) %>%
  summarise(ntot = n(),
            n_hosps = sum(outcome, na.rm = T)) %>%
  mutate(outcome_rate = n_hosps/ntot) %>%
  select(epidemic_year, outcome_rate) %>%
  mutate(epidemic_year = as.character(epidemic_year)) %>%
  bind_rows(overall_outcome_rate)


result_frame <- result_frame %>%
  na.omit() %>%
  mutate(epidemic_year = as.factor(epidemic_year)) %>%
  mutate(Country = "Sweden") %>%
  left_join(outcome_rates, by = "epidemic_year")

result_frame 


### NNT using 10% cutoff
# some parameters
efficacy <- 0.6
top_10_cutoff <- data$prediction %>%
  quantile(., probs = 0.90, na.rm = T)

all_hosps <- nnt_data %>%
  mutate(top_n = ifelse(prediction >= top_10_cutoff, 1, 0)) %>%
  group_by(epidemic_year) %>%
  summarise(n_hosp_all = sum(outcome, na.rm = T),
            n_top_10 = sum(top_n, na.rm = T),
            n_overall = n()) 

prevented_hosps <- nnt_data %>%
  mutate(top_n = ifelse(prediction >= top_10_cutoff, 1, 0)) %>%
  filter(top_n == 1) %>%
  group_by(epidemic_year) %>%
  summarise(n_hosps_in_top10 = sum(outcome, na.rm = T)) %>%
  mutate(prevented_hosps_in_top10 = n_hosps_in_top10*efficacy)

compare_top10_to_all <- left_join(all_hosps, prevented_hosps) %>%
  mutate(fraction_of_prevented = prevented_hosps_in_top10/n_hosp_all,
         nnt = n_top_10/prevented_hosps_in_top10)


setwd(results_dir)
save(result_frame, file = "nnt_results.R")
save(compare_top10_to_all, file = "nnt_in_10%_cutoff.R")







#### SCRIPT TO TEST THE VALIDITY OF RSV PREDICTION MODEL IN THE SWEDISH DATA



### REQUIREMENTS
# - For running the script, you need to have the model object as a file. Pekka will send it with a different message. 
# - Save the model files to a folder, and note the file path
# - Add 3 file paths to the script
# 	- path to the model files
# 	- Path to the data
# 	- Path where results should be saved.
#
#
## Following thigs should be in the data so that this script works.
#
# - birth date, named as "birth_date". 
# - variables should be named as in here: https://docs.google.com/spreadsheets/d/1yNZhB1EFCURMLML-zeOcIdy47iuQvdL8bTjOtVyO2yI/edit#gid=163265107
# - The birth dates should be in between 1.6.2006 - 31.5.2020 (this is tested in the script)
# - outcome date, named as "outcome_date", meaning the date of RSV hospitalisation This is only relevant for the definition of the peak month.


#  Some further thoughts: it is possible that there are problems with the date variables when loading the data to R, if the data is from STATA. 
# So it's good to check that. The date variables are quite important for classifying the results. 


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

# where the data is stored (also the file name
path_to_data <- "..."

# location of the model objects (just the path, not the file names)
model_path <- "..."

# The place where the results should be saved (just the path)
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

## If some packages are not installed, they can be installed with running these commands
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
  "asd_or_vsd_only")

# are all 17 variable names included in the data?


if(sum(var_names %in% colnames(data)) != 17) {
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
          digits.pct = 3) %>% as_tibble(.name_repair = "minimal") %>%
  rename("feature" = '')








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
hist <- data %>%
  select(prediction) %>%
  mutate(pred = round(prediction, digits = 2)) %>%
  mutate(pred = ifelse(pred > 0.2, ">0.2", pred)) %>%
  group_by(pred) %>%
  summarise(n = n())



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
  
  # auc for each year
  this_year_auc <- data_for_year %>%
    summarise(auc = as.numeric(auc(outcome, prediction)),
              ci_low_auc = ci.auc(outcome, prediction)[1],
              ci_auc_high = ci.auc(outcome, prediction)[3],
              epidemic_year = i)
  
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
  select(outcome, prediction)

dca_object <- dca(formula = outcome ~ prediction,
                  data = dca_data,
                  thresholds = seq(0, 0.15, by = 0.001)) %>%
  as_tibble %>%
  mutate(country = "sweden")









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
	group_by(outcome_year, outcome_month) %>%
	summarise(outcome_rate_sweden = n()) 

save(monthly_outcomes, file = "monthly_outcomes_sweden.R")
write.csv(monthly_outcomes, "monthly_outcomes_sweden.csv",
	row.names = FALSE)



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






# load datasets: predict_data and all_bds

setwd("/data/projects/project_pvartiai/rsv/modeling")
predict_datapath <- "/data/projects/project_pvartiai/rsv/modeling/"
comp_datapath <- "/data/projects/project_pvartiai/rsv/predictors/"

predict_data <- fread(paste0(predict_datapath, "development_set_final_variables.csv")) %>%
	as_tibble() %>%
	# add birth month variable to the predict data (not necessary for other than checking)
	mutate(birth_month = as.numeric(month(LAPSEN_SYNTYMAPVM)))

# more compact data, with non-feasible variables filtered out
compact_data <- fread(paste0(predict_datapath, 
														"development_set_only_feasible_variables.csv")) %>%
	as_tibble

# # load composite data if needed
# composite_data <- fread(paste0(comp_datapath, "composite_data_all_candidate_predictors.csv")) %>%
# 	as_tibble





# ids and birth dates
all_bds <- fread("/data/projects/project_pvartiai/rsv/wrangle/preprocess_3_mbr_hilmo_outcomes.csv", 
  select = c("TNRO", "LAPSEN_SYNTYMAPVM")) %>% as_tibble





# # # # # # #     F O R M U L A     O B J E C T S    # # # # # # 


# we create 2x4 formula objects, 4 for glm and 4 for rms methods
# 
# # formula objects
#
#	all_form ; no interactions, all final predictors
#	compact_form ; no interactions, only predictors from compact data
#	longhosp_form  ; outcome is hospitalization for 3 days
#	compact_longhosp_form ; outcome is hosp of 3 days, only compact_data variables

# notes
# 	- use 'compact' formulas for compact_data and other formulas for predict_data
#		- prefer compact data, as it is more relevant for prediction
# 	- do fancy AUC comparisons with each of these

# formula objects for rms stuff, using rcs for defining cubic splines
# all_form_rcs
# compact_form_rcs 
# longhosp_form_rcs 
# compact_longhosp_form_rcs 


# all features
all_features <- setdiff(names(predict_data), 
						c("TNRO", 
							"outcome", 
							"outcome_3days",
							"outcome_duration",
							"LAPSEN_SYNTYMAPVM", 
							"birth_year", 
							"epidemic_year",
							"birth_month")) # replace with "dist" if modeling with birth month

all_features_compact <- setdiff(names(compact_data), 
						c("TNRO", 
							"outcome", 
							"outcome_3days",
							"outcome_duration",
							"LAPSEN_SYNTYMAPVM", 
							"birth_year", 
							"epidemic_year",
							"birth_month"))

# continuous variables using distance from epidemic (instead of birth month)
# replace with dist
cont_vars <- c("gest_days", "dist", "weight_sd", "mother_age") 

# # Continuous variables, using birth_month. 
# cont_vars <- c("gest_days", "birth_month", "weight_sd", "mother_age")

# save fixed knot locations for continous variable
# use dist instead of birth month
knots_dist <- rcspline.eval(predict_data$dist, nk = 4, knots.only = TRUE)
knots_birthmonth <- rcspline.eval(predict_data$birth_month, nk = 4, knots.only = TRUE)
knots_gestdays <- rcspline.eval(predict_data$gest_days, nk = 4, knots.only = TRUE)
knots_weightsd <- rcspline.eval(predict_data$weight_sd, nk = 4, knots.only = TRUE)
knots_motherage <- rcspline.eval(predict_data$mother_age, nk = 4, knots.only = TRUE)


# # we can also explore with setting more knots
# knots_gestdays <- c(220, 240, 257, 276, 283)

# vector of knot names for the loop
knot_names <- c(
	"knots_gestdays", 
	"knots_dist", # replace with "knots_birthmonth", if needed
	"knots_weightsd",
	"knots_motherage")

# continuous variable spline functions using function rcspline eval,
# which fixes the knots for the fit object. Use a loop to create the strings

# initialize
cont_string_fixedknots <- NULL

# loop for creating a string for each continuous variable
for(i in 1:4) {
cont_string_fixedknots <- paste0(cont_string_fixedknots, 
									'rcspline.eval(', 
									cont_vars[i], 
									', knots = ', 
									knot_names[i],')+')
}

#string of rcs-function with continuous variables as arguments, for the lrm objects
cont_string_rcs <- paste0('rcs(', cont_vars, ', 4)+') %>% paste(., collapse = "")

# other than continuous variables
other_vars <- setdiff(all_features, cont_vars)
other_string <- paste(other_vars, collapse = "+")


# other than continuous variables, compact data
other_vars_compact <- setdiff(all_features_compact, cont_vars)
other_string_compact <- paste(other_vars_compact, collapse = "+")


# combine continuous variables to others
allvars_string_fixedknots <- paste(c(cont_string_fixedknots, other_string), collapse = "")
allvars_string_fixedknots_compact <- paste(c(cont_string_fixedknots, other_string_compact), collapse = "")
# for rms stuff
allvars_string_rcs <- paste(c(cont_string_rcs, other_string), collapse = "")
allvars_string_rcs <- paste(c(cont_string_rcs, other_string_compact), collapse = "")


# join start_string to the variable name strings
start_string <- "outcome ~ "
start_string_longhosp <- "outcome_3days ~"

#strings for fixed knots (glm)
formula_string_fixedknots <- paste(start_string, allvars_string_fixedknots)
# for compact data
formula_string_fixedknots_compact <- paste(start_string, allvars_string_fixedknots_compact)
# for long hosp
formula_string_fixedknots_longhosp <- paste(start_string_longhosp, 
																						allvars_string_fixedknots)
# for long hosp, compact data
formula_string_fixedknots_compact_longhosp <- paste(start_string_longhosp, 
																						allvars_string_fixedknots_compact)


# rcs stuff
formula_string_rcs <- paste(start_string, allvars_string_rcs)
formula_string_rcs_compact <- paste(start_string, allvars_string_rcs_compact)
formula_string_rcs_longhosp <- paste(start_string_longhosp, allvars_string_rcs)
formula_string_rcs_compact_longhosp <-  paste(start_string_longhosp, allvars_string_rcs_compact)
















# # # # # # # # # # #
# These two datas can be used in prediction 
#
#
#

# data with only most important predictors
compact_data
# data with all final predictors
predict_data






# # # # # # # # # # # # # # 
# these formula objects are ready to be used for prediction
#
#
#
#
#
#


# formula objects using rcspline.eval for splines
all_form <- formula(formula_string_fixedknots)
# formula for primary outcome in all data
compact_form <- formula(formula_string_fixedknots_compact)
# formula for long hospitalization in broad data
longhosp_form <- formula(formula_string_fixedknots_longhosp)
# formula for long hospitalization in the compact data
compact_longhosp_form <- formula(formula_string_fixedknots_compact_longhosp)

# formula objects using rcs for defining cubic splines
all_form_rcs <- formula(formula_string_rcs)
compact_form_rcs <- formula(formula_string_rcs_compact)
longhosp_form_rcs <- formula(formula_string_rcs_longhosp)
compact_longhosp_form_rcs <- formula(formula_string_rcs_compact_longhosp)








# # # # # # # # # # # #  M O D E L     F I T T I N G    # # # # # # #



# fit model using normal glm and fixed knots

# add epidemic interaction to formula
interaction_form <- update(compact_form, ~ . + # replace the formula object
	even_epidemic_year*rcspline.eval(dist, knots = knots_dist))

# fit
compact_fit <- glm(
	data = compact_data,
	family = binomial(),
	formula = epidemic_interaction_form_rcs)

# add epidemic interaction to formula
interaction_fullform <- update(all_form, ~ . + # replace the formula object
	even_epidemic_year*rcspline.eval(dist, knots = knots_dist))

full_final_fit <- glm(
	data = predict_data,
	family = binomial(),
	formula = epidemic_interaction_fullfinalform)





# these produce the same length
predict_compact <- predict(compact_fit, newdata = compact_data, type = "response")
predict_full <- predict(full_final_fit, newdata = predict_data, type = "response")


auc(compact_data$outcome, predict_compact)

length(predict)
length(predict_data$outcome)




rocplot <- plot(roc(predict_data$outcome, predict, 
		percent=F,
		boot.n=1000, 
		ci.alpha=0.9, 
		stratified=FALSE, 
		plot=TRUE, 
		grid=TRUE, 
		show.thres=TRUE, 
		legacy.axes = TRUE, 
		reuse.auc = TRUE,
# print.thres = c(0.30,0.35, 0.40, 0.45,0.48, 0.50,0.55, 0.60),#
		print.auc = TRUE, 
		print.thres.col = "blue", 
		ci=TRUE, ci.type="bars", 
		print.thres.cex = 0.7, 
		main = paste("ROC curve using","(N = ",nrow(predict_data),")")))


save(rocplot, file = "/home/pvartiai/RSV/results/auc_epidemic_interaction.rdata")


# exploring with other interactions





# # # # # # #       T O Y I N G    W I T H    R M S     # # # # #
# here we'll play with rms objects, and with Predict function, 
# to visualise the association of variables and outcome in a model
# - how to interpret log odds?

ddist = datadist(predict_data)
options("datadist" = ddist)

interact_fit_rcs <- lrm(
	data = predict_data,
	x = TRUE, y = TRUE, 
	formula = interaction_all_form_rcs
	)

plot <- ggplot(Predict(interact_fit_rcs, weight_sd))

# plot needs to be saved and opened



#
#
#



# # # # # #      V A L I D A T I O N     E X C E R C I S E     # # # # # #


interaction_explore_form <- update(only_month_interaction_step2_form, 
	# add interaction terms
	~ . + rcs(gest_days, 4):rcs(dist, 4))

# IN PREDICTIONS, WE USE A MODEL FIT WITHOUT ANY EPIDEMIC PARAMETRES OR INTERACTIONS
no_epidemic_things_form <- update(interaction_explore_form,  
	~ . - even_epidemic_year - rcs(dist, 4):even_epidemic_year)



# train with the development set
no_epidemic_things_fit <- glm(data = predict_data, 
												family = binomial(),
												formula = no_epidemic_things_form)


# THIS FIT CONTAINS BINARY EPIDEMIC YEAR AND ITS INTERACTION WITH BIRTH MONTH
interaction_explore_fit <- glm(data = predict_data, 
												family = binomial(),
												formula = interaction_explore_form)






# function to get yearly aucs CROSS-VALIDATED.
# function fits a new model in a sub-development data without the selected year's data,
# then predicts values to that year's data

get.yearly.aucs <- function(year, custom_formula, data = predict_data) {

	data_without_year <- data %>% 
		filter(epidemic_year != year)

	print(paste("training set filtering for", year))

	fit_without_year <- glm(data = data_without_year, 
												family = binomial(),
												formula = custom_formula)

	print(paste(year, "fit complete"))

	# filter a subsample
	subsample_1 <- data %>%
	filter(epidemic_year == year)


	# predict
	predict <- predict.glm(fit_without_year, 
		newdata = subsample_1, 
		type = "response")

	predicted_data <- cbind(subsample_1, predict) %>% as_tibble

	auc <- auc(predicted_data$outcome, predicted_data$predict) %>% as.numeric
	auc
}



# test the function
get.yearly.aucs(year = 1999, custom_formula = no_epidemic_things_form, data = predict_data %>% sample_n(10000))


# results dataframe for years that exist completely in our dataset

yearly_auc <- data.frame(epidemic_year = 1999:2017, 
	auc_no_epidemic_things = NA, 
	auc_binary_epidemic_interaction = NA)


# # # # # 
#
#
# for loop to get the cross-validated aucs for each year
# 
# no epidemic interactions are considered

for (i in 1:nrow(yearly_auc)) {

this_year <- yearly_auc$epidemic_year[i]

# function takes year and formula as arguments
temp_auc <- get.yearly.aucs(year = this_year, 
														# use a formula with no epidemic change data whatsoever
														custom_formula = no_epidemic_things_form, 
														data = predict_data)

yearly_auc[i,2] <- temp_auc
print(paste(this_year, "auc obtained!"))
}


# # # # #
#
# second for loop to get the aucs using a model with epidemic interaction variable
#
# considering a binary epidemic var and interaction with dist

for (i in 1:nrow(yearly_auc)) {

this_year <- yearly_auc$epidemic_year[i]

# function takes year and formula as arguments
temp_auc <- get.yearly.aucs(year = this_year, 
														# use a formula with epidemic interaction
														custom_formula = interaction_explore_form, 
														data = predict_data)
# change the variable indicator
yearly_auc[i,3] <- temp_auc

print(i)
}

# examine differences in aucs
yearly_auc %>%
	mutate(diff = auc_binary_epidemic_interaction - auc_no_epidemic_things) %>%
	summarise(sd_no_interact = mean(auc_no_epidemic_things),
						sd_interact = mean(auc_binary_epidemic_interaction))





predict_data[,"predict"] <- predict.glm(no_epidemic_things_fit,
																				newdata = predict_data,
																				type = "response")


auc(predict_data$outcome, predict_data$predict) # 0,76

predict_data <- predict_data %>% select(-predict)



## define a dataset with only candidate final variables

final_data <- predict_data %>%
	select(all_of(c(step2_cont_vars, step2_other_vars, "bpd"))


setwd("/data/projects/project_pvartiai/rsv/modeling/")
save(interaction_explore_form, file = "interaction_explore_form.R")




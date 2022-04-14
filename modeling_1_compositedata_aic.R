# MODELING SCRIPT 1

# we work with the composite data which combines variables from all different source registries.
# First, composite data includes all candidate predictors
# then we'll do the AIC-based backwards stepwise exclusion. Then we'll model some epidemic changes.

# to clarify, composite data is only build from the development set.


# composite data location
comp_path <- "/data/projects/project_pvartiai/rsv/predictors/"
composite_data_name <- "composite_data_all_candidate_predictors.csv"
all_possible_composite_data_name <- "composite_data_all_interesting_variables.csv"

# read composite data
composite_data <- fread(paste0(comp_path, composite_data_name)) %>% as_tibble
# all_possible_composite_data <- fread(paste0(comp_path, all_possible_composite_data_name)) %>% as_tibble

# ids and birth dates
all_bds <- fread("/data/projects/project_pvartiai/rsv/wrangle/preprocess_3_mbr_hilmo_outcomes.csv", 
  select = c("TNRO", "LAPSEN_SYNTYMAPVM")) %>% as_tibble




# build the first formula object

all_features <- setdiff(names(composite_data), c("TNRO", 
																									"outcome", 
																									"LAPSEN_SYNTYMAPVM", 
																									"birth_year", 
																									"epidemic_year",
																									"outcome_3days", 
																									"outcome_duration",
																									"birth_month"))

cont_vars <- c("gest_days", "dist", "weight_sd", "height_sd", "mother_age", "father_age") 
cont_string <- paste0('rcs(', cont_vars, ', 4)+') %>% paste(., collapse = "")

other_vars <- setdiff(all_features, cont_vars)
other_string <- paste(other_vars, collapse = "+")

allvars_string <- paste(c(cont_string, other_string), collapse = "")

start_string <- "outcome ~ "
start_longhosp_string <- "outcome_3days ~"

formula_string <- paste(start_string, allvars_string)
formula_longhosp_string <- paste(start_longhosp_string, allvars_string)

# formula objects
all_form <- formula(formula_string)
all_longhosp_form <- formula(formula_longhosp_string)

# add epidemic interaction
interaction_all_form <- update(all_form, ~ . + even_epidemic_year*rcs(dist, 4))
interaction_longhosp_form <- update(all_longhosp_form, 
																		~ . + even_epidemic_year*rcs(dist, 4))









# B U I L D    T H E   F I R S T    M O D E L    
# with all preselected variables (we'll do the AIC exclusion based on this variable list)




first_fit <- glm(data = (composite_data),
          family = binomial(),
          formula = all_form)

summary(first_fit)
# anova_table <- anova(first_fit)

multivar_results <- data.frame(feature = rownames(coef(summary(first_fit))), 
                              coef = NA, 
                              se = NA, 
                              p = NA)
multivar_results["coef"] <- (coef(summary(first_fit))[,1] %>% as_tibble)
multivar_results["se"] <- coef(summary(first_fit))[,2]
multivar_results["p"] <- coef(summary(first_fit))[,4]

setwd("/data/projects/project_pvartiai/rsv/results/composite_model/")
write.csv(multivar_results, "all_composite_model_coefs_2_edited_features.csv")









#  M O D E L  F O R    A I C    S E L E C T I O N 


# make the full model object suitable for 'fastbw'
ddist <- datadist(composite_data)
options("datadist" = ddist)

final_fit2 <- lrm(data = (composite_data),
          formula = interaction_all_form,
          x = TRUE, y = TRUE)

final_longhosp_fit <- lrm(data = (composite_data),
          formula = interaction_longhosp_form,
          x = TRUE, y = TRUE)


weird_na_coefs <- coefficients(final_fit2)[is.na(coefficients(final_fit2))] %>% 
  names

aic_multivar <- fastbw(final_fit2)

# select retained feature names as vector,
# and create formula strings again
dropped_vars <- setdiff(names(composite_data), 
  c(aic_multivar$names.kept, "TNRO", "outcome", "LAPSEN_SYNTYMAPVM"))

# 4 continuous variables in 3/2022
step2_cont_vars <- setdiff(cont_vars, dropped_vars) 
# 43 other variables
step2_other_vars <- setdiff(aic_multivar$names.kept, c(step2_cont_vars, "dist", "dist * even_epidemic_year"))

if(length(step2_other_vars) == 0) {
  step2_cont_string <- paste0('rcs(', step2_cont_vars, ', 4)') %>% paste(., collapse = "+")
} else {
  step2_cont_string <- paste0('rcs(', step2_cont_vars, ', 4)+') %>% paste(., collapse = "")
}

step2_other_string <- paste(step2_other_vars, collapse = "+")

# create formula object
step2_string <- paste(start_string, step2_cont_string, step2_other_string) 








# # # # # # # # # # C R E A T E     P R E D I C T     D A  T A     # # # 

# vague predictors
vague_vars <- c("valproate_father_drugs",
		"kefexin_father_drugs",
		"N06AB_father_drugs",
		"N06AX_father_drugs",
		"N06AX_mother_drugs",
		"N06AB_mother_drugs",
		"benz_father_drugs",
		"opioids_father_drugs",
		"macrolide_father_drugs",
		"ppi_father_drugs",
		"common_uti_ab_amorion_father_drugs",
		"valproate_mother_drugs",
		"kefexin_mother_drugs",
		"opioids_mother_drugs",
		"benz_mother_drugs",
		"macrolide_mother_drugs",
		"ppi_mother_drugs",
		"common_uti_ab_amorion_mother_drugs",
		"K11_REFLUX_sib_ep", 
		"ALLERG_RHINITIS_sib_ep",
		"F5_BEHEMOCHILD_sib_ep", 
		"F5_BEHAVE_sib_ep",
		"benz_mother_drug_pregnancy",
		"O209_neodg", "K11_REFLUX_father_ep",
		"K11_ACUTGASTR_father_ep",
		"L12_CELLULITIS_father_ep",
		"MIGRAINE_TRIPTAN_father_ep",
		"K11_REFLUX_mother_ep",
		"K11_ACUTGASTR_mother_ep",
		"L12_CELLULITIS_mother_ep",
		"O15_BREAST_LACT_OTHER_DIS_mother_ep",
		"N14_HYPERTROPHYBREAST_mother_ep"
		)


# Create 'predict_data', the final data to be used in the next script
predict_data <- composite_data %>%
	select(c(TNRO, 
						outcome, 
						outcome_3days, 
						outcome_duration,
						step2_cont_vars, 
						step2_other_vars, 
						bpd)) %>%
	select(-any_of(vague_vars)) %>%
 	left_join(., all_bds, by = "TNRO") %>%
	mutate(birth_year = as.numeric(year(LAPSEN_SYNTYMAPVM))) %>%
	mutate(epidemic_year = ifelse(dist <= 5, birth_year, (birth_year+1))) %>%
	mutate(even_epidemic_year = ifelse(epidemic_year %% 2 == 0, 1, 0)) 



setwd("/data/projects/project_pvartiai/rsv/modeling")
write.csv(predict_data, "development_set_final_variables.csv",
	row.names = FALSE)



# even more compact data
compact_data <- predict_data %>%
	select(-N14_FEMALEGENINF_mother_ep) %>%
	select(-ASTHMA_ACUTE_RESPIRATORY_INFECTIONS_sib_ep) %>%
	select(-N02BE_mother_drug_pregnancy) %>%
	mutate(any_ab_pregnancy = case_when(
		pregnancy_other_antibiotic == 1 ~ 1, 
		common_uti_ab_amorion_mother_drug_pregnancy == 1 ~1,
		TRUE ~ 0)) %>%
	select(-pregnancy_other_antibiotic, -common_uti_ab_amorion_mother_drug_pregnancy)

setwd("/data/projects/project_pvartiai/rsv/modeling")
write.csv(compact_data, "development_set_only_feasible_variables.csv",
	row.names = FALSE)


# The most important part of the script ends here. 
# What follows is the model fitting excercises and e.g. yearly 
# cross-validation, which is actually aimed to be done in a different script

# # #   F I N    # # #







# # # # # #  R E F I N E D    C O M P O S I T E    D A T A  # # # # #  ## # 
# # use variables only suitable for prediction model that go into AIC selection

# predict_data


# weird_na_coefs <- coefficients(final_fit2)[is.na(coefficients(final_fit2))] %>% 
#   names

# aic_multivar <- fastbw(final_fit2)

# aic_multivar_longhosp <- fastbw(final_longhosp_fit)


# # select retained feature names as vector,
# # and create formula strings again
# dropped_vars <- setdiff(names(composite_data), 
#   c(aic_multivar$names.kept, "TNRO", "outcome", "LAPSEN_SYNTYMAPVM"))

# # 4 continuous variables in 3/2022
# step2_cont_vars <- setdiff(cont_vars, dropped_vars) 
# # 43 other variables
# step2_other_vars <- setdiff(aic_multivar$names.kept, c(step2_cont_vars, vague_vars, "dist", "dist * even_epidemic_year"))

# if(length(step2_other_vars) == 0) {
#   step2_cont_string <- paste0('rcs(', step2_cont_vars, ', 4)') %>% paste(., collapse = "+")
# } else {
#   step2_cont_string <- paste0('rcs(', step2_cont_vars, ', 4)+') %>% paste(., collapse = "")
# }

# step2_other_string <- paste(step2_other_vars, collapse = "+")

# # create formula object
# step2_string <- paste(start_string, step2_cont_string, step2_other_string) 







# # # # # # # #      F U R T H E R    M O D E L I N G    # # # # #

# # Create 3 formula objects

# # without interaction
# step2_form <- step2_string %>%
#  formula

# #only 1 interaction with month
# only_month_interaction_step2_form <- update(step2_form, ~ . + even_epidemic_year*rcs(dist, 4))

# # if want to add interactions
# all_interaction_step2_form <- strsplit(step2_string, "\\+") %>% 
#   unlist %>%
#   setdiff(., c("even_epidemic_year")) %>%
#   paste(., "*even_epidemic_year", sep = "") %>%
#   paste(., collapse = "+") %>%
#   formula









# # # # # # # # # #   I N T E R AC T I O N   F I T T I N G 		# # # # # # # # # # #


# # cannot use lrm if we want the p value
# ddist = datadist(predict_data)
# options("datadist" = ddist)



# only_month_interaction_step2_fit <-  glm(data = (predict_data),
#           family = binomial(),
#           formula = step2_form) 




# # 	only_month_interaction_step2_form
# only_month_interaction_step2_fit <-  lrm(data = (predict_data),
#           x = TRUE, y = TRUE,
#           formula = only_month_interaction_step2_form) # choose formula from 'step2_form' or 'interaction_step2_form'


# ## glm
# only_month_interaction_step2_fit <-  glm(data = (predict_data),
# 					family = binomial(),
#           formula = only_month_interaction_step2_form)



# ddist = datadist(predict_data)
# options("datadist" = ddist)


# # 	step2_form
# no_interactions_lrm <-  lrm(data = (predict_data),
#           formula = step2_form, 
#           x = TRUE, y = TRUE)

# ## no interactions
# only_month_interaction_lrm <-  lrm(data = (predict_data),
#           x = TRUE, y = TRUE,
#           formula = only_month_interaction_step2_form) 

# # 	all_interactions_step2_form
# all_interactions_lrm <- lrm(data = (predict_data),
#           x = TRUE, y = TRUE,
#           formula = all_interaction_step2_form) 

# all_interactions_step2_fit <- glm(data = (predict_data),
#           family = binomial(),
#           formula = all_interaction_step2_form) 


# anova_table <- only_month_interaction_lrm %>% anova

# anova_table %>% rownames
# anova_table %>% rownames_to_column(., var = "feature") %>% as_tibble()


# ## save
# setwd("/data/projects/project_pvartiai/rsv/results/epidem_models/")
# write.csv(as.data.frame(anova_table), "anova_stepwise_aic_model.csv")


# # extract coefficients - ALL INTERACTIONS (WITH ODD EPIDEMIC YEAR)
# all_interaction_results <- data.frame(feature = rownames(coef(summary(all_interactions_step2_fit))), coef = NA, se = NA, p = NA)
# all_interaction_results["coef"] <- coef(summary(all_interactions_step2_fit))[,1]
# all_interaction_results["se"] <- coef(summary(all_interactions_step2_fit))[,2]
# all_interaction_results["p"] <- coef(summary(all_interactions_step2_fit))[,4]

# setwd("/data/projects/project_pvartiai/rsv/results/epidem_models/")
# write.csv(all_interaction_results, "all_interactions_with_odd_even_year.csv")



# # counts
# predict_data %>%
# 	select(TNRO, all_of(other_vars)) %>%
# 	select(-c(sib_0_4, sib_4_7, sib_over7, mother_tongue_mbr_structural)) %>%
#   pivot_longer(cols = -TNRO) %>%
#   filter(value == 1) %>%
#   group_by(name) %>%
#   summarise(n = n()) %>%
#   write.csv(., "composite_vars_counts.csv")




# ## save
# setwd("/data/projects/project_pvartiai/rsv/results/epidem_models/")
# write.csv(as.data.frame(anova_table), "anova_stepwise_aic_model.csv")

# # extract coefficients - NO INTERACTIONS
# no_interaction_results <- data.frame(feature = rownames(coef(summary(step2_fit))), coef = NA, se = NA, p = NA)
# no_interaction_results["coef"] <- coef(summary(step2_fit))[,1]
# no_interaction_results["se"] <- coef(summary(step2_fit))[,2]
# no_interaction_results["p"] <- coef(summary(step2_fit))[,4]

# setwd("/data/projects/project_pvartiai/rsv/results/epidem_models/")
# write.csv(no_interaction_results, "composite_stepwise_aic_2_edited_features.csv")
# print("yee")

# # extract coefficients - ONLY MONTH INTERACTION
# onlymonth_interaction_results <- data.frame(feature = rownames(coef(summary(only_month_interaction_step2_fit))), coef = NA, se = NA, p = NA)
# onlymonth_interaction_results["coef"] <- coef(summary(only_month_interaction_step2_fit))[,1]
# onlymonth_interaction_results["se"] <- coef(summary(only_month_interaction_step2_fit))[,2]
# onlymonth_interaction_results["p"] <- coef(summary(only_month_interaction_step2_fit))[,4]

# setwd("/data/projects/project_pvartiai/rsv/results/epidem_models/")
# write.csv(onlymonth_interaction_results, "composite_stepwise_aic_2_edited_features.csv")
# print("yee")





#  # # #    			 L I K E L I H O O D    	R A T I O 			T E S T   		# # #


# lrtest(no_interactions_lrm, only_month_interaction_lrm)
# lrtest(only_month_interaction_lrm, all_interactions_lrm)










# # # #     E X P L O R E    W I T H    O T H E R    I N T E R A C T I O N S   # # # # 

# # formula with no interactions
# step2_form

# # AIC-filtered formula with dist:even_epidemic_year -interaction
# only_month_interaction_step2_form


# # this script can be used to test many interactions or rcs definitions
# # for example, remove old rcs term and add another with 6 knots.
# #
# #
# #

# explore_form <- update(only_month_interaction_step2_form, 
# 	# add interaction terms
# 	~ . + rcs(gest_days, 4):rcs(dist, 4))

# interaction_explore_fit <- glm(data = predict_data, 
# 																formula = explore_form,
# 																	family = binomial())


# # likelihood ratio test
# lrtest(only_month_interaction_step2_fit, interaction_explore_fit)











# epidemic_data <- composite_data %>%
# 	mutate(birth_year = as.numeric(year(LAPSEN_SYNTYMAPVM))) %>%
# 	mutate(epidemic_year = ifelse(dist <= 5, birth_year, (birth_year+1))) %>%
# 	mutate(even_epidemic_year = ifelse(epidemic_year %% 2 == 0, 1, 0)) %>%
# 	mutate(epidemic_year = as.factor(epidemic_year)) %>%
# 	mutate(birth_yearmonth = paste(dist, birth_year, sep = "/")) %>%
# 	mutate(birth_yearmonth = as.factor(birth_yearmonth)) %>%
# 	mutate(dist_factor = as.factor(dist))



# # edit variables for the epidemiologically adjusted modeling

# # exclude birth month from continuous, will be included as categorical
# epidemic_cont_vars <- setdiff(step2_cont_vars, exclude_these)
# # keep other features the same
# step2_other_vars
# # create strings for formula objects 
# epidemic_cont_string <- paste0('rcs(', epidemic_cont_vars, ', 4)+') %>% paste(., collapse = "")
# epidemic_other_string <- paste(c(step2_other_vars, epidemic_vars), collapse = "+")

# epidemic_form <- paste(start_string, epidemic_cont_string, epidemic_other_string) %>%
#  formula




# epidemic_fit <-  glm(data = (composite_data),
#       family = binomial(),
#       formula = epidemic_form)



# epidemic_fit2 <- update(epidemic_fit, . ~ . + epidemic_year*rcs(dist, 4))





# epidemic_results <- data.frame(feature = rownames(coef(summary(epidemic_fit2))), coef = NA, se = NA, p = NA)
# epidemic_results["coef"] <- coef(summary(epidemic_fit2))[,1]
# epidemic_results["se"] <- coef(summary(epidemic_fit2))[,2]
# epidemic_results["p"] <- coef(summary(epidemic_fit2))[,4]


# # Cstat(epidemic_fit)
# # 0,783 for birth_yearmonth
# # 0,___ for interaction of even_epidemic_year and dist_factor

# epidemic_results
# setwd("/data/projects/project_pvartiai/rsv/results/composite_model/")
# write.csv(epidemic_results, "composite_aicexcluded_epidemicyear_interaction.csv")
# print("yee")




# review 2 model coefficients

library(data.table)
library(feather)
library(dplyr)
library(tibble)
library(tidyr)
library(lubridate)
library(stringr)
library(rms)
library(pROC)










################################################################################################################################################################################
# load the data
################################################################################################################################################################################
predict_datapath <- "/data/projects/project_pvartiai/rsv/modeling/"
comp_datapath <- "/data/projects/project_pvartiai/rsv/predictors/"
val_path <- "/data/projects/project_pvartiai/rsv/validation_data/"

# data related to updated outcome definition
hilmo_original_kids <- fread("/data/projects/project_pvartiai/rsv/wrangle/hilmo_original_kids.csv") %>%
  as_tibble()

# load hilmo diagnoses
hilmo_orig_dg_kids <- fread("/data/projects/project_pvartiai/rsv/wrangle/hilmo_orig_dg_kids.csv") %>%
  as_tibble()


# load infectious disease registry, and keep only rsv samples
ttr_rsv <- fread("/data/processed_data/thl_infectious_diseases/infectious_diseases_2022-05-24.csv") %>% 
  as_tibble %>%
  select(c(
    TNRO, diagnosis_method, laboratory_type,
    reporting_group, sampling_date)) %>%
  filter(reporting_group == "['RSV']") %>%
  filter(TNRO %in% all_ids) %>%
  left_join(., all_bds, by = "TNRO") %>%
  mutate(age_at_outcome = as.numeric(difftime(sampling_date, 
                                              LAPSEN_SYNTYMAPVM,
                                              units = "days"))) %>%
  filter(age_at_outcome < 365.25)

rsv_tests <- ttr_rsv %>%
  select(TNRO, LAPSEN_SYNTYMAPVM, sampling_date) %>%
  mutate(positive_rsv_test = 1)



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


all_data <- bind_rows(recent_data, test_set)


# load model objects and knots and formula
setwd("/data/projects/project_pvartiai/rsv/functions/model/")

final_form_simple <- get(load("/data/projects/project_pvartiai/rsv/functions/model/final_formula_object_nointeract.R"))
load("rcsknots_motherage.R")
load("rcsknots_dist.R") 
load("rcsknots_gestdays.R")
load("rcsknots_weightsd.R")
visualise_fit <- get(load("final_model_fit.R"))

setwd("/data/projects/project_pvartiai/rsv/functions/")
load("keylist_final_variable_names.R")

keylist_final %>% print(n = 100)

keylist_final$longname[keylist_final$feature == "gest_days"] <- "Gestational age\nat birth, weeks"
keylist_final$longname[keylist_final$feature == "dist"] <- "Months from birth to\nnext epidemic peak"


################################################################################################################################################################################
# CREATE NEW OUTCOME
################################################################################################################################################################################

rsv_dg_codes <- c("J210", "B974", "J205", "J121")

# FIND THE RSV DIAGNOSIS CASES

# select relevant diagnoses from hilmo
rsv_dgs <- hilmo_orig_dg_kids %>%
  filter(TNRO %in% all_data$TNRO) %>%
  filter(KOODI %in% rsv_dg_codes)

# hilmo ID's corresponding all possible RSV diagnosis
rsv_ids <- rsv_dgs$HILMO_ID %>% unique

# all possible HILMO entries containing RSV diagnosis, based on HILMO_ID
all_rsv_entries <- hilmo_original_kids %>%
  filter(HILMO_ID %in% rsv_ids)

# all inpatient entries (with rsv diagnosis). Inpatient: PALA == 1
# Period_duration indicates the duration of the hospitalization
rsv_inpatients <- all_rsv_entries %>%
  filter(PALA == 1|YHTEYSTAPA == "R80"|KIIREELLISYYS == "3") %>%
  mutate(period_duration = difftime(LPVM, TUPVA, units = "days"))

# Combine birth dates to the inpatient entries
rsv_inpatients <- left_join(rsv_inpatients, all_bds, by = "TNRO")

# calculate kids' age at hilmo entry
rsv_inpatients <- rsv_inpatients %>%
  mutate(age_at_outcome = difftime(TUPVA, LAPSEN_SYNTYMAPVM, units = "days"))

# filter only cases where age at admission is from 7 to 365 days
rsv_inpatients_agefilt <- rsv_inpatients %>%
  filter(age_at_outcome < 366 & age_at_outcome > 6)

# still 1086 duplficates!
# identify all duplicate FINREGISTRY IDs in rsv inpatient list

all_inp_ids <- rsv_inpatients_agefilt$TNRO

# duplicated ids (one row per single id)
duplicate_inp_ids <- all_inp_ids[duplicated(all_inp_ids)]

# only unique IDs
unique_inp_ids <- setdiff(all_inp_ids, duplicate_inp_ids)

#unique RSV hilmo entries
only_unique_rsv_inpatient_entries <- rsv_inpatients_agefilt %>%
  filter(TNRO %in% unique_inp_ids)

# All RSV entries from the duplicated, arranged by age
duplicated_rsv_inpatient_entries <- rsv_inpatients_agefilt %>%
  filter(TNRO %in% duplicate_inp_ids) %>%
  group_by(TNRO) %>%
  arrange(age_at_outcome, .by_group = TRUE)

# select the first inpatient rsv entry - the one with the youngest age
first_from_duplicates_rsv_entries <- duplicated_rsv_inpatient_entries %>%
  slice_head()


# outcomes is the final dataset of relevant hilmo entries
outcomes_rsv_dg <- bind_rows(only_unique_rsv_inpatient_entries,
                             first_from_duplicates_rsv_entries) %>%
  mutate(outcome_2 = 1) %>%
  select(-LAPSEN_SYNTYMAPVM)

diagnosis_based_outcome_ids <- outcomes_rsv_dg %>%
  filter(outcome_2 == 1) %>%
  pull(TNRO)


outcome_2 <- outcomes_rsv_dg %>%
  select(TNRO, outcome_2, age_at_outcome)

################################################################################################################################################################################
### outcome based on test and J21.x




inpatients <- hilmo_original_kids %>%
  filter(TNRO %in% all_data$TNRO) %>%
  filter(PALA == 1|YHTEYSTAPA == "R80"|KIIREELLISYYS == "3") %>%
  mutate(period_duration = difftime(LPVM, TUPVA, units = "days"))


# Combine birth dates to the inpatient entries
inpatients_bd <- left_join(inpatients, all_bds, by = "TNRO")

# calculate kids' age at hilmo entry
inpatients_bd <- inpatients_bd %>%
  mutate(age_at_outcome = difftime(TUPVA, LAPSEN_SYNTYMAPVM, units = "days"))


# filter only cases where age at admission is from 7 to 365 days
inpatients_agefilt <- inpatients_bd %>%
  filter(age_at_outcome < 366 & age_at_outcome > 6)


# exclude all hilmo IDs which have linked RSV diagnosis code
not_these_hilmo_ids <-  hilmo_orig_dg_kids %>%
  filter((KOODI %in% c("J210", "J205", "J121", "B974"))) %>%
  pull(HILMO_ID)

only_these_hilmo_ids_j21x <- hilmo_orig_dg_kids

# HOSPITALISATIONS at 1y without RSV-diagnoses
extra_inpatients <- inpatients_agefilt %>%
  filter(!(HILMO_ID %in% not_these_hilmo_ids)) %>%
  filter(HILMO_ID %in% only_these_hilmo_ids_j21x$HILMO_ID)

extra_inpatients_only <- extra_inpatients %>%
  filter(!(TNRO %in% diagnosis_based_outcome_ids))



#join with RSV test and include a recent test variable
extra_inp_with_test <- left_join(extra_inpatients_only, select(rsv_tests, -LAPSEN_SYNTYMAPVM), by = c("TNRO")) %>%
  mutate(positive_rsv_test = ifelse(is.na(positive_rsv_test), 0, positive_rsv_test)) %>%
  mutate(difftime_rsv_test = difftime(TUPVA, sampling_date, units = "days")) %>%
  mutate(recent_rsv_test = ifelse(abs(difftime_rsv_test) < 4, 1, 0)) %>%
  mutate(recent_rsv_test = ifelse(is.na(recent_rsv_test), 0, recent_rsv_test))


extra_inp_with_test_only <- extra_inp_with_test %>%
  filter(recent_rsv_test == 1) %>%
  filter(LAPSEN_SYNTYMAPVM > as.Date("2006-06-01"))


dupl_extra_inp_ids <- extra_inp_with_test_only %>%
  filter(duplicated(TNRO)) %>%
  pull(TNRO)

extra_inp_with_test_only_unique <- extra_inp_with_test_only %>%
  filter(!TNRO %in% dupl_extra_inp_ids)


extra_inp_with_test_only_dupl <- extra_inp_with_test_only %>%
  filter(duplicated(TNRO, fromLast = FALSE)|duplicated(TNRO, fromLast = TRUE))

extra_inp_with_test_only_dupl_firsts <- extra_inp_with_test_only_dupl %>%
  group_by(TNRO) %>%
  slice_min(TUPVA, with_ties = FALSE)


extra_inp_with_test_only <- bind_rows(extra_inp_with_test_only_unique, extra_inp_with_test_only_dupl_firsts)

rsv_hosp_with_test_and_j21 <- extra_inp_with_test_only %>%
  filter(recent_rsv_test == 1) %>%
  mutate(outcome_2 = 1)


outcome_3 <- rsv_hosp_with_test_and_j21 %>%
  select(TNRO, outcome_2, age_at_outcome)



new_outcome <- bind_rows(outcome_2, outcome_3) %>%
  rename(outcome = outcome_2)

data_with_new_outcome <- all_data %>%
  select(-outcome, -outcome_3days) %>%
  left_join(., new_outcome) %>%
  mutate(outcome = ifelse(is.na(outcome), 0, outcome))


data_with_new_outcome %>%
  filter(outcome == 1)



# check that old and new outcome data have same ids
setdiff(recent_data$TNRO, data_with_new_outcome$TNRO) # empty!


new_training_data <- data_with_new_outcome %>%
  filter(LAPSEN_SYNTYMAPVM < as.Date("2017-01-06"))

new_test_data <- data_with_new_outcome %>%
  filter(LAPSEN_SYNTYMAPVM >= as.Date("2017-01-06"))




################################################################################################################################################################################
# COMPARE MODEL COEFS
################################################################################################################################################################################
# we have already the old model
visualise_fit



fit_with_new_outcome <- glm(formula = final_form_simple,
                            data = new_training_data,
                            family = binomial())



old_coefs <- coef(summary(visualise_fit)) %>%
  as.data.frame() %>%
  rownames_to_column(var = "feature") %>%
  mutate(outcome = "J21.0") %>%
  rename("coef" = "Estimate", 
         "se" = 'Std. Error')


new_coefs <- coef(summary(fit_with_new_outcome)) %>%
  as.data.frame() %>%
  rownames_to_column(var = "feature") %>%
  mutate(outcome = "Extended (All\nRSV-associated\nhospitalisations)") %>%
  rename("coef" = "Estimate", 
         "se" = 'Std. Error')

pos <- position_dodge(width = 0.2)

compare_coefs <- bind_rows(old_coefs, new_coefs) %>%
  left_join(., keylist_final) %>%
  mutate(longname = case_when(
    feature == "rcs(dist, parms = rcsknots_dist)dist" ~ "Months from birth to next epidemic peak - spline term 1",
    feature == "rcs(dist, parms = rcsknots_dist)dist'" ~ "Months from birth to next epidemic peak - spline term 2",
    feature == "rcs(dist, parms = rcsknots_dist)dist''" ~ "Months from birth to next epidemic peak - spline term 3",
    feature == "rcs(gest_days, parms = rcsknots_gestdays)gest_days" ~ "Gestational age at birth - spline term 1",
    feature == "rcs(gest_days, parms = rcsknots_gestdays)gest_days'" ~ "Gestational age at birth - spline term 2",
    feature == "rcs(gest_days, parms = rcsknots_gestdays)gest_days''" ~ "Gestational age at birth - spline term 3",
    feature == "rcs(gest_days, parms = rcsknots_gestdays)gest_days'''" ~ "Gestational age at birth - spline term 4",
    feature == "rcs(gest_days, parms = rcsknots_gestdays)gest_days''''" ~ "Gestational age at birth - spline term 5",
    feature == "rcs(weight_sd, parms = rcsknots_weightsd)weight_sd" ~ "Birth weight in SD units - spline term 1",
    feature == "rcs(weight_sd, parms = rcsknots_weightsd)weight_sd'" ~ "Birth weight in SD units - spline term 2",
    feature == "rcs(weight_sd, parms = rcsknots_weightsd)weight_sd''" ~ "Birth weight in SD units - spline term 3",
    feature == "rcs(mother_age, parms = rcsknots_motherage)mother_age" ~ "Mother's age - spline term 1",
    feature == "rcs(mother_age, parms = rcsknots_motherage)mother_age'" ~ "Mother's age - spline term 2",
    feature == "rcs(mother_age, parms = rcsknots_motherage)mother_age''" ~ "Mother's age - spline term 3",
    is.na(longname) ~ feature, 
    TRUE ~ longname
  )) 

coef_plot <- compare_coefs %>%
  mutate(coef_ci = 1.96*se) %>%
  ggplot(., aes(
    y = str_wrap(longname, 30),
    x = coef,
    xmin = coef-coef_ci,
    xmax = coef+coef_ci,
    col = outcome)) + 
  geom_point(position = pos) + 
  geom_errorbarh(position = pos, height = 0.2) + 
  theme_bw(base_size = 14) + 
  labs(y = "Predictor",
       x = "Regression coefficient",
       color = "Outcome definition")



coef_plot


################################################################################################################################################################################
# COMPARE AUCs
################################################################################################################################################################################


# internal auc.
# old
old_train_outcome <- recent_data$outcome
old_train_pred <- predict(visualise_fit, newdata = recent_data, type = "response")

old_int_auc <- ci.auc(old_train_outcome, old_train_pred)

# external auc in validation data.
old_ext_outcome <- test_set$outcome
old_ext_pred <- predict(visualise_fit, newdata = test_set, type = "response")
old_ext_auc <- ci.auc(old_ext_outcome, old_ext_pred)


## NEW
# new int auc
new_train_outcome <- new_training_data$outcome
new_train_pred <- predict(fit_with_new_outcome, newdata = new_training_data, type = "response")

new_int_auc <- ci.auc(new_train_outcome, new_train_pred)

# new ext auc
new_test_outcome <- new_test_data$outcome
new_test_pred <- predict(fit_with_new_outcome, newdata = new_test_data, type = "response")

new_ext_auc <- ci.auc(new_test_outcome, new_test_pred)


rsv_tests %>%
  mutate(year = year(sampling_date)) %>%
  group_by(year) %>%
  summarise(n=n())


# COMPARE
actual_new_auc <- new_int_auc[2]
actual_old_auc <- old_int_auc[2]

new_int_auc %>% as.character
old_int_auc

headline <- "C-statistics estimated in internal validation"
old_text <- paste0(round(old_int_auc[2], 3), 
                   " (", 
                   round(old_int_auc[1], 3), 
                   " - ", 
                   round(old_int_auc[3], 3), 
                   ")")

new_text <- paste0(round(new_int_auc[2], 3), 
                   " (", 
                   round(new_int_auc[1], 3), 
                   " - ", 
                   round(new_int_auc[3], 3), 
                   ")")

all_text <- paste(headline, new_text, old_text, sep = "\n") %>% as.character()


# labeled_coef_plot <- coef_plot +
#   annotate(geom = "label", 
#            y = 25, 
#            x = 2.8, 
#            label = all_text,
#            size = 4)

labeled_coef_plot

coef_plot

mytable <- cbind(
  "Outcome\ndefinition" = c("Extended", "J21.0"),
  "C-statistic (95% CI)\nin internal validation" = c(new_text, old_text)
)

labeled_coef_plot <- coef_plot + annotation_custom(tableGrob(mytable), xmin = 1.5, ymin = 17)

labeled_coef_plot



ggsave(plot = labeled_coef_plot,
       filename = "suppl_review_outcome_coefs.pdf",
       device = "pdf",
       width = 9,
       height = 10,
       dpi = 1200,
       path = "/data/projects/project_pvartiai/rsv/results/plots/")
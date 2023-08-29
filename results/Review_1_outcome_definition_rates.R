# Review: outcome examination scripts

# libraries
library(data.table)
library(feather)
library(dplyr)
library(tibble)
library(tidyr)
library(lubridate)
library(stringr)






# load study population and basic characteristics
mbr_c <- fread("/data/projects/project_pvartiai/rsv/wrangle/preprocess_3_mbr_hilmo_outcomes.csv") %>%
  as_tibble

all_bds <- mbr_c %>%
  select(TNRO, LAPSEN_SYNTYMAPVM)

all_ids <- mbr_c %>% pull(TNRO)


# load hilmo kids
hilmo_original_kids <- fread("/data/projects/project_pvartiai/rsv/wrangle/hilmo_original_kids.csv") %>%
  as_tibble()

# load hilmo diagnoses
hilmo_orig_dg_kids <- fread("/data/projects/project_pvartiai/rsv/wrangle/hilmo_orig_dg_kids.csv") %>%
  as_tibble()


# load infectious disease registries
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




### DEFINE RSV DIAGNOSIS CODES
rsv_dg_codes <- c("J210", "B974", "J205", "J121")
# not included ("J205","J121")





# FIND THE J210 CASES

# select relevant diagnoses from hilmo
rsv_dgs <- hilmo_orig_dg_kids %>%
  filter(KOODI %in% "J210")

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
outcomes_j210 <- bind_rows(only_unique_rsv_inpatient_entries,
                      first_from_duplicates_rsv_entries) %>%
  mutate(outcome = 1) %>%
  mutate(outcome_3days = ifelse(period_duration > 2, 1, 0)) %>%
  select(-LAPSEN_SYNTYMAPVM)





#### B974
# select relevant diagnoses from hilmo
rsv_dgs <- hilmo_orig_dg_kids %>%
  filter(KOODI %in% "B974")
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
outcomes_b974 <- bind_rows(only_unique_rsv_inpatient_entries,
                           first_from_duplicates_rsv_entries) %>%
  mutate(outcome = 1) %>%
  mutate(outcome_3days = ifelse(period_duration > 2, 1, 0)) %>%
  select(-LAPSEN_SYNTYMAPVM) %>%
  filter(!(TNRO %in% outcomes_j210$TNRO))






##### J205


# select relevant diagnoses from hilmo
rsv_dgs <- hilmo_orig_dg_kids %>%
  filter(KOODI %in% "J205")
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
outcomes_j205 <- bind_rows(only_unique_rsv_inpatient_entries,
                           first_from_duplicates_rsv_entries) %>%
  mutate(outcome = 1) %>%
  mutate(outcome_3days = ifelse(period_duration > 2, 1, 0)) %>%
  select(-LAPSEN_SYNTYMAPVM) %>%
  filter(!(TNRO %in% outcomes_j210$TNRO))



##### J121


# select relevant diagnoses from hilmo
rsv_dgs <- hilmo_orig_dg_kids %>%
  filter(KOODI %in% "J121")
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
outcomes_j121 <- bind_rows(only_unique_rsv_inpatient_entries,
                           first_from_duplicates_rsv_entries) %>%
  mutate(outcome = 1) %>%
  mutate(outcome_3days = ifelse(period_duration > 2, 1, 0)) %>%
  select(-LAPSEN_SYNTYMAPVM) %>%
  filter(!(TNRO %in% outcomes_j210$TNRO))


outcomes_j210$dg <- "J210"

outcomes_j121$dg <- "J121"

outcomes_j205$dg <- "J205"
outcomes_j205 <- outcomes_j205 %>%
  filter(!(TNRO %in% outcomes_j121$TNRO))

outcomes_b974$dg <- "B974" 
outcomes_b974 <- outcomes_b974 %>%
  filter(!(TNRO %in% outcomes_j121$TNRO)) %>%
  filter(!(TNRO %in% outcomes_j205$TNRO))






outcomes_compare <- bind_rows(outcomes_j210, outcomes_j121, outcomes_j205, outcomes_b974)


outcomes_compare_tests <- left_join(outcomes_compare, select(rsv_tests, -LAPSEN_SYNTYMAPVM), by = c("TNRO")) %>%
  mutate(positive_rsv_test = ifelse(is.na(positive_rsv_test), 0, positive_rsv_test)) %>%
  mutate(difftime_rsv_test = difftime(TUPVA, sampling_date, units = "days")) %>%
  mutate(recent_rsv_test = ifelse(abs(difftime_rsv_test) < 7, 1, 0)) %>%
  mutate(recent_rsv_test = ifelse(is.na(recent_rsv_test), 0, recent_rsv_test))


outcomes_compare_tests$recent_rsv_test

outcome_comparison_table <- outcomes_compare_tests %>%
	left_join(., all_bds) %>%
  filter(LAPSEN_SYNTYMAPVM > as.Date("2006-06-01")) %>%
  group_by(dg) %>%
  summarise(n = n(),
            mean_age_at_outcome = mean(age_at_outcome, na.rm = T),
            median_age_at_outcome = median(age_at_outcome, na.rm = T),
            mean_hosp_duration = mean(period_duration, na.rm = T),
            rate_of_positive_tests_7days = mean(recent_rsv_test, na.rm = T))




outcome_comparison_table







### ALL HOSPITALISATIONS WHO HAVE POSITIVE RSV TEST




inpatients <- hilmo_original_kids %>%
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

# HOSPITALISATIONS at 1y without RSV-diagnoses
extra_inpatients <- inpatients_agefilt %>%
  filter(!(HILMO_ID %in% not_these_hilmo_ids))


extra_inpatients_only <- extra_inpatients %>%
  filter(!(TNRO %in% outcomes_compare_tests$TNRO))



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

rsv_hosp_without_dg <- extra_inp_with_test_only %>%
  filter(recent_rsv_test == 1) 

rsv_hosp_without_dg %>%
  select(difftime_rsv_test) 

dgs_of_extra_inps <- hilmo_orig_dg_kids %>%
  filter(HILMO_ID %in% rsv_hosp_without_dg$HILMO_ID)


dgs_of_extra_inps %>% group_by(KOODI) %>% summarise(n=n()) %>% arrange(desc(n))

rsv_hosp_without_dg %>%
  pull(period_duration) %>%
  as.numeric() %>%
  summary()

rsv_hosp_without_dg %>%
  summarise(mean_age = mean(age_at_outcome, na.rm = T),
            median_age = median(age_at_outcome, na.rm = T))

# preprocess: 

# defining basic vars and splitting into development and validation dataset. 

pred_path <- "/data/projects/project_pvartiai/rsv/predictors/"
fam_path <- "/data/projects/project_pvartiai/rsv/family/"



# S T A N D A R D     D E V I A T I O N S      O F      H E I G H T     A N D     W E I G H T 

mbr_c <- fread("/data/projects/project_pvartiai/rsv/wrangle/preprocess_3_mbr_hilmo_outcomes.csv") %>%
	as_tibble

all_bds <- fread("/home/pvartiai/RSV/data/wrangle/rsv_data_hilmo_and_mbr.csv",
                 select = c("TNRO", "LAPSEN_SYNTYMAPVM")) %>%
  as_tibble


sd_ref <- fread("/data/projects/project_pvartiai/rsv/refs/syntymakoko_ref.csv") %>%
	as_tibble() %>%
	mutate(across(everything(),
		gsub,
		pattern = ",",
		replacement = ".")) %>%
	mutate(across(everything(), as.numeric))


# 363000 head circumferences missing. Will exclude that. 
# 3000 lengths and 1000 weights missing.
# how is sex coded? 1 = male, 2 = female. In our data, only values are 1 and 2 (no unclears)
sd_data <- mbr_c %>%
	select(TNRO, gest_weeks, SUKUP, twin, SYNTYMAPAINO, SYNTYMAPITUUS, twin) %>%
	mutate(gest_weeks = round(gest_weeks, digits = 2)) %>%
	mutate(sex_is_boy = ifelse(SUKUP == 1, 1, 0),
		singleton = +(!twin)) %>%
	select(-SUKUP, -twin) %>%
	select(TNRO, gest_weeks, sex_is_boy, singleton, SYNTYMAPAINO, SYNTYMAPITUUS)



sd_data <- left_join(sd_data, sd_ref, by = c("gest_weeks", "singleton", "sex_is_boy"))

## testing
sd_data %>%
	mutate(weight_sd = (SYNTYMAPAINO - mu_paino)/sigma_paino,
		height_sd = (SYNTYMAPITUUS - mu_pituus)/sigma_pituu) %>%
	filter(weight_sd > 20)
## / testing / ##

sd_values <- sd_data %>%
	mutate(weight_sd = (SYNTYMAPAINO - mu_paino)/sigma_paino,
		height_sd = (SYNTYMAPITUUS - mu_pituus)/sigma_pituu) %>%
	select(TNRO, weight_sd, height_sd)


# sanity check for sd values
# there are individual cases with weight 10+SD:s. Let's code them as missing
sd_values %>%
	filter(weight_sd > 8) 

sd_values <- sd_values %>%
	mutate(weight_sd = ifelse(weight_sd > 10, NA_real_, weight_sd),
			height_sd = ifelse(height_sd > 10, NA_real_, height_sd)) 




setwd("/data/projects/project_pvartiai/rsv/predictors/")
write.csv(sd_values, "birth_size_sd_values.csv",
	row.names = FALSE)





# B A S E    P R E D I C T O R S 

# birth month, gest_weeks, sukupuoli, basic stuff
basic <- mbr_c %>%
	mutate(birth_month = as.numeric(month(LAPSEN_SYNTYMAPVM))) %>%
	select(TNRO, outcome, outcome_3days, 
			period_duration, gest_days, birth_month, 
			SUKUP, twin, LAPSEN_SYNTYMAPVM) %>%
	mutate(female_gender = as.factor(SUKUP - 1)) %>% select(-SUKUP) %>%
	rename(outcome_duration = period_duration)


# D I S T A N C E    T O     N E X T   E P I D E M I C S 


# add an epidemic peak variable
epidem_years <- basic %>%
	mutate(birth_year = as.numeric(year(LAPSEN_SYNTYMAPVM))) %>%
	mutate(epidemic_year = ifelse(birth_month <= 5, birth_year, (birth_year+1))) %>%
	# mutate(even_epidemic_year = ifelse(epidemic_year %% 2 == 0, 1, 0)) %>%
	select(TNRO, LAPSEN_SYNTYMAPVM, birth_year, epidemic_year)


# epidemic peaks
# these are defined by choosing the month during epidemic 
# (epidemic year being june-may)
# which has the highest number of cases
# December is coded with 0, that's the first month when there are epidemic peaks

# actual peaks
epidemic_peaks <- data.frame(epidemic_year = c(1998:2020),
                             day = rep(15, length(1998:2020)),
                             peak_month = c(12, 4, 2, 4, 1, 4, 1, 5, 12, 4, 2, 4, 3, 4, 2, 3, 3, 3, 2, 3, 1, 4, 2)
) %>%
	mutate(epidemic_year_for_date = ifelse(peak_month == 12, epidemic_year - 1, epidemic_year)) %>%
    mutate(peak_date = paste0(day, "/", peak_month, "/", epidemic_year_for_date) %>%
               as.Date(., format = "%d/%m/%Y")) %>%
    select(epidemic_year, peak_date)


# # our guess, based on average data (biennal variation before 2007, then averaging on march)
# guess_epidemic_peaks <- data.frame(
# 	epidemic_year = c(1998:2019),
# 	peak_month = c(1, 4, 1, 4, 1, 4, 1, 4, 1, 4, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3),
# 	next_peak_month = c(4, 1, 4, 1, 4, 1, 4, 1, 4, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3))


# average epidemic peaks
average_epidemic_peaks <- data.frame(epidemic_year = c(1998:2020),
                             day = rep(15, length(1998:2020)),
                             peak_month = rep(2, length(1998:2020)))%>%
	mutate(epidemic_year_for_date = ifelse(peak_month == 12, epidemic_year - 1, epidemic_year)) %>%
    mutate(peak_date = paste0(day, "/", peak_month, "/", epidemic_year_for_date) %>%
               as.Date(., format = "%d/%m/%Y")) %>%
    select(epidemic_year, peak_date)

# epidem_years <- left_join(epidem_years, epidemic_peaks, by = "epidemic_year")
epidem_years <- left_join(epidem_years, epidemic_peaks, by = "epidemic_year")

# function to recalibrate months according to the epidemic years, june-many
# calib.month <- function(x) {
# 	mo <- x+7
# 	mo <- ifelse(mo > 12, mo - 12, mo)
# 	mo
# }

### example data frame for calculating the distance
# data.frame(birth = c(1:12, 12, 9) %>% calib.month, 
# 					peak = c(12, 5, 2, 4, 3, 4, 12, 1:5, 12, 3) %>% calib.month,
# 					next_peak = c(3:5, 3, 2, 5, 1, 4, 2, 4, 1, 12, 12, 1) %>% calib.month) %>%
# 	mutate(dist = peak - birth) %>%
# 	mutate(dist_to_next =  12+(next_peak - birth)) %>%
# 	mutate(dist = ifelse(dist < 0, dist_to_next, dist))





dist_to_next <- epidem_years %>%
	mutate(dist = difftime(peak_date, LAPSEN_SYNTYMAPVM, units = "days")) %>%
	mutate(dist = round(as.numeric(dist) / 30, digits = 1)) %>%
	select(TNRO, dist)




# chds for base predictors [x]
chd_path <- paste0(pred_path, "congenital_heart_defects.csv")
common_chd_path <- paste0(pred_path, "common_benign_congenital_heart_defects.csv")

# all variables are relevant in the new chd dataset
chd_basic <- fread(chd_path) %>% 
	as_tibble 





# likely BPD [x]
bpd_path <- paste0(pred_path, "bpds.csv")
bpd <- fread(bpd_path) %>% as_tibble

# sisarukset, määrä ja outcome [x]
sib_path <- paste0(pred_path, "number_of_siblings.R")
sibs <- fread(sib_path) %>% as_tibble %>% select(-have_sibs)
sibs <- left_join(select(all_bds, TNRO), sibs)
sibs <- sibs %>%
	mutate(across(everything(), ~ ifelse(is.na(.x), 0, .x)))




# down [x]
down_path <- paste0(pred_path, "down_syndrome_1mo.csv")
down <- fread(down_path) %>% as_tibble

# vanhempien iät [x]
mo_path <- paste0(fam_path, "mothers.csv")
fa_path <- paste0(fam_path, "fathers.csv")
mo_age <- fread(mo_path, select = c("TNRO", "mother_age")) %>% as_tibble
fa_age <- fread(fa_path, select = c("TNRO", "father_age")) %>% as_tibble

# sds
sd_values


# join everything for a development set
base_sets <- as.list("basic", "chd_basic", "bpd", "sibs", "down", "mo_age", "fa_age", "sd_values")
base_vars <- all_bds %>% select(TNRO)
base_vars <- left_join(base_vars, basic, by = "TNRO")
base_vars <- left_join(base_vars, dist_to_next, by = "TNRO")
base_vars <- left_join(base_vars, sd_values, by = "TNRO")
base_vars <- left_join(base_vars, chd_basic, by = "TNRO")
base_vars <- left_join(base_vars, bpd, by = "TNRO")
base_vars <- left_join(base_vars, sibs, by = "TNRO")
base_vars <- left_join(base_vars, down, by = "TNRO")
base_vars <- left_join(base_vars, mo_age, by = "TNRO")
base_vars <- left_join(base_vars, fa_age, by = "TNRO")







# B I R T H    R E G I S T R Y    S T R U C T U R A L      V A R I A B L E S

# section [x]
mbr_vars <- mbr_c %>% 
	select(TNRO, c_section_birth)

# # mother tongue and chds will not be included to the prediction work
# mother tongue (skip the mother tongue,) 
# moto <- fread(paste0(pred_path, "mother_tongue.csv")) %>% as_tibble


# common chds for testing
common_chd_path <- paste0(pred_path, "common_benign_congenital_heart_defects.csv")
chd_common <- fread(common_chd_path) %>%
	as_tibble


# join datasets
mbr_structural <- all_bds %>% select(TNRO)
mbr_structural <- left_join(mbr_structural, mbr_vars, by = "TNRO")
mbr_structural <- left_join(mbr_structural, chd_common, by = "TNRO")

setwd("/data/projects/project_pvartiai/rsv/predictors/")
write.csv(mbr_structural, "mbr_structural_vars.csv",
	row.names = FALSE)



# development and validation set dates

development_set <- base_vars %>%
    filter(LAPSEN_SYNTYMAPVM < as.Date("2017-1-1")) %>%
    select(-LAPSEN_SYNTYMAPVM)

setwd("/data/projects/project_pvartiai/rsv/predictors/")
write.csv(development_set, "development_set.csv",
	row.names = FALSE)



validation_set <- basic_vars %>%
    filter(LAPSEN_SYNTYMAPVM >= as.Date("2017-1-1")) %>%
    filter(LAPSEN_SYNTYMAPVM <= as.Date("2018-12-31"))



# SENSITIVITY ANALYSIS VARIABLES
# birth year
mbr_vars_sensitivity <- mbr_c %>%
	select(TNRO, ASUINKUNTA, LAPSEN_SYNTYMAPVM)


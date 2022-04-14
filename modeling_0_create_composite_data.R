# CUSTOM ENDPOINTS

library(dplyr)
library(tidyr)
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
library(lrtest)


# load development set and birth dates

pred_path <- "/data/projects/project_pvartiai/rsv/predictors/"
dev_path <- paste0(pred_path, "development_set.csv")


development_set <- fread(dev_path) %>% as_tibble

all_bds <- fread("/data/projects/project_pvartiai/rsv/wrangle/preprocess_3_mbr_hilmo_outcomes.csv", 
  select = c("TNRO", "LAPSEN_SYNTYMAPVM")) %>% as_tibble



# vectors of all the important variable names

# drugs

all_possible_drugs <- c(
"valproate",
"N05AH",
"kefexin",
"opioids",
"inhaled_steroids",
"benz",
"macrolide",
"inhaled_opening_drugs",
"N06AX",
"montelucast",
"ppi",
"fluorocinolone",
"po_cortison",
"adrenaline",
"M01AE",
"N02BE",
"N06AB",
"common_uti_ab_amorion"
)

cool_fa_drugs <- all_possible_drugs %>%
	setdiff(., c("montelucast", "M01AE", "fluorocinolone", "N02BE"))

cool_mo_drugs <- all_possible_drugs %>%
	setdiff(., c("montelucast", "M01AE", "fluorocinolone", "N02BE"))


cool_preg_drugs <- all_possible_drugs

# structural variables from birth registry

cool_birthreg_struct <- c(
"c_section_birth",
"only_vsd",
"only_common_asd"
)

all_interesting_sib_ep <- c(
'J10_BRONCHIOLITIS',
'J10_ASTHMACOPDKELA',
'H8_SUP_ACUTE',
'AB1_GASTROENTERITIS_NOS',
'AB1_VIRAL_SKIN_MUCOUS_MEMBRANE',
'CHILDHOOD_ALLERGY',
'ASTHMA_ACUTE_RESPIRATORY_INFECTIONS',
'ASTHMA_PNEUMONIA',
'J10_LOWERINF',
'CPAP',
'J10_BRONCHITIS',
'K11_APHTA_RECUR_INCLAVO',
'J10_ACUTELOWERNAS',
"P16_INFECTIONS_SPECIFIC_PERINA_PERIOD",
'J10_CHRONSINUSITIS',
'AB1_VARICELLA',
'J10_ACUTEUPPERINFEC',
'G6_MIGRAINE_NO_AURA',
'J10_INFLUENZA',
'SLEEP',
'H8_MIDDLEMASTOID',
'J10_VIRALPNEUMO',
'K11_STOMATITIS',
'G6_MIGRAINE',
'J10_PNEUMONIA',
'J10_LARYNGITIS',
'K11_REFLUX',
'ALLERG_RHINITIS',
'H8_CHRON_SUPOTITIS',
'AB1_INTESTINAL_INFECTIONS',
"L12_URTICARIA",
'POLLENALLERG',
'F5_BEHEMOCHILD',
'F5_BEHAVE',
"L12_ATOPIC",
"J10_TONSILLECTOMY",
"RX_PARACETAMOL_NSAID"
)

# sibling end points

cool_sib_ep <- c(
'other_j21_endpoint',
"sib_rsv_hospitalization",
'J10_ASTHMACOPDKELA',
'CHILDHOOD_ALLERGY',
'ASTHMA_ACUTE_RESPIRATORY_INFECTIONS',
'K11_REFLUX',
'ALLERG_RHINITIS',
"L12_URTICARIA",
'F5_BEHEMOCHILD',
'F5_BEHAVE',
"L12_ATOPIC"
 )



# neonatal diagnoses

all_possible_neodg <- c(
"Q39",
"P961",
"P58",
"P24",
"term_rds",
"premie_rds",
"P04_other",
"O354",
"O355",
"P044",
"O40",
"P22_other_unsp",
"O209",
"O342",
"P70",
"P221",
"P94",
"Q67_68",
"O990",
"O993",
"O995",
"O9980"
)

# for the prediction model
cool_neodg <- c(
"smoking",
"Q39",
"P961",
"P58",
"P24",
"term_rds",
"premie_rds",
"P04_other",
"O354",
"O355",
"P044",
"O40",
"P22_other_unsp",
"O209",
"O342",
"P70",
"P221",
"P94",
"Q67_68"
)

# parents disease endpoints


all_possible_parent_ep <- c(
'O15_BREAST_LACT_OTHER_DIS',
'F5_OTHERSUB',
'E4_CONGEIOD',
'F5_SEDAHYP',
'F5_SUBSNOALCO',
'F5_CANNABIS',
'AB1_VIRAL_HEPATITIS',
'L12_URTICA_ALLERG',
'K11_CARIES_DENTIN',
'F5_BIPO',
'F5_BULIMIA',
'F5_PSYCHOTH',
'N14_HYPERTROPHYBREAST',
'K11_REFLUX',
'F5_SOMATOFORM',
'F5_EMOPER',
'J10_LOWERINF',
'F5_KELAMENT',
'G6_MIGRAINE_WITH_AURA_TRIPTAN',
'L12_CELLULITIS',
'G6_HEADACHE',
'VWXY20_INTENTI_SELF_P_EXPOS_OTHER_UNSPE_CHEMIC_NOXIO_SUBST',
'K11_PULP_PERIAPICAL',
'K11_OESSTODUO',
'ALCOHOLACUTE10',
'F5_PHOBANX',
'G6_MIGRAINE_NO_AURA_TRIPTAN',
'F5_STRESSOTH',
'J10_CHRONRHINITIS',
'VWXY20_SUICI_OTHER_INTENTI_SELF_H',
'L12_PAPULOSQUAMOUS',
'E4_PCOS',
'J10_LOWCHRON',
'RX_CROHN_1STLINE', # glucocorticoids
'F5_DEPRESSIO',
'N14_INFLUTH',
'K11_IBS',
'N14_URETHRAOTH',
'G6_MIGRAINE',
'N14_VAGINITIS',
'F5_ANXIETY',
'J10_ASTHMACOPDKELA',
'F5_MOOD',
'N14_URINOTH',
'F5_PANIC',
'PULM_INFECTIONS',
'ALCOHOL_RELATED',
'F5_ALLANXIOUS',
'G6_SLEEPAPNO',
'G6_SLEEPAPNO_INCLAVO',
'E4_OVARDYS',
'ANTIDEPRESSANTS',
'L12_OTHERSKINSUBCUTIS',
'J10_INFLUPNEU',
'K11_INTESTOTH',
'O15_PUERP_SEPSIS',
'J10_PNEUMONIA',
'G6_NERPLEX',
'N14_FEMALEGENINF',
'N14_SALPHOOPH',
'N14_BREAST',
'G6_EPIPAROX',
'E4_ENDOGLAND',
'O15_PUERP_INFECT_OTHER',
'J10_SINUSITIS',
'CHILDHOOD_ALLERGY',
'H8_MED_SUPP',
'H8_EUSTSALP',
'N14_MESNRUIRREG',
'H8_MIDDLEMASTOID',
'L12_DERMATITISNAS',
'O15_EXCESS_VOMIT_PREG',
'ALLERG_RHINITIS',
'MIGRAINE_TRIPTAN',
'O15_HAEMORRH_EARLY_PREG',
'L12_URTICARIA',
'H8_NONSUPPNAS',
'N14_ENDOMETRIOSIS',
'E4_PCOS_CONCORTIUM',
'AB1_INTESTINAL_INFECTIONS',
'J10_TONSILLECTOMY',
'L12_DERMATITISECZEMA',
'L12_ATOPIC',
'N14_FEMALEGENNONINF',
'E4_HYTHYNAS',
'L12_INFECT_SKIN',
'J10_UPPERDIS',
'M13_KNEEDERANGEMENTS',
'E4_THYROID',
'J10_CHRONTONSADEN',
'K11_ORAL',
'K11_GINGIVITIS_PERIODONTAL',
'H7_ALLERGICCONJUNCTIVITIS',
'RX_ANTIHYP',
'E4_HYTHY_AI_STRICT',
'O15_ABORT_MEDICAL',
'K11_APPENDACUT',
'N14_PYELONEPHR',
'FG_CVD',
'O15_PREG_ABORT',
'O15_ABORT_SPONTAN',
'N14_RENALTUB',
'GEST_DIABETES'
)

cool_fa_ep <- c(
"F5_KELAMENT",
"K11_REFLUX",
"K11_ACUTGASTR",
"L12_URTICARIA",
"L12_CELLULITIS",
"E4_CONGEIOD",
"J10_ASTHMACOPDKELA",
"MIGRAINE_TRIPTAN"
)

cool_mo_ep <- c(
"F5_KELAMENT",
"K11_REFLUX",
"K11_ACUTGASTR",
"L12_URTICARIA",
"L12_CELLULITIS",
"O15_BREAST_LACT_OTHER_DIS",
"E4_CONGEIOD",
"J10_ASTHMACOPDKELA",
"N14_HYPERTROPHYBREAST",
"N14_FEMALEGENINF",
"O15_PREG_ABORT"
)


# combine the vectors to a single list of interesting variables
cool_vars <- c(
	cool_fa_drugs,
	cool_mo_drugs,
	cool_birthreg_struct,
	cool_sib_ep,
	cool_preg_drugs,
	cool_neodg,
	cool_fa_ep,
	cool_mo_ep
	)

# single list of all possible interesting variables. Might be useful for plotting
all_possible_vars <- c(
	cool_fa_drugs,
	cool_mo_drugs,
	cool_birthreg_struct,
	all_interesting_sib_ep,
	cool_preg_drugs,
	all_possible_neodg,
	all_possible_parent_ep,
	all_possible_parent_ep
	)


# variable source registries for prediction model
cool_var_sources <- c(
	rep("father_drugs", length(cool_fa_drugs)),
	rep("mother_drugs", length(cool_mo_drugs)),
	rep("mbr_structural", length(cool_birthreg_struct)),
	rep("sib_ep", length(cool_sib_ep)),
	rep("mother_drug_pregnancy", length(cool_preg_drugs)),
	rep("neodg", length(cool_neodg)),
	rep("father_ep", length(cool_fa_ep)),
	rep("mother_ep", length(cool_mo_ep))
)

# variable source names for all possible variables
all_possible_var_sources <- c(
	rep("father_drugs", length(cool_fa_drugs)),
	rep("mother_drugs", length(cool_mo_drugs)),
	rep("mbr_structural", length(cool_birthreg_struct)),
	rep("sib_ep", length(all_interesting_sib_ep)),
	rep("mother_drug_pregnancy", length(cool_preg_drugs)),
	rep("neodg", length(all_possible_neodg)),
	rep("father_ep", length(all_possible_parent_ep)),
	rep("mother_ep", length(all_possible_parent_ep))
	)


# combine variable name and source to a data frame. Again twice for prediction model variables and for all possible variables
cool_var_df <- data.frame(var = cool_vars, source = cool_var_sources) %>%
    as_tibble

all_possible_var_df <- data.frame(var = all_possible_vars, source = all_possible_var_sources) %>%
    as_tibble


# soucre names

unique(cool_var_sources)

allnames <- c(
	"father_drugs",
	"mother_drugs",
	"mbr_structural",
	"sib_ep",
	"mother_drug_pregnancy",
	"neodg",
	"father_ep",
	"mother_ep"
)

filenames <- c(
	"meds_father_beforepreg_wide_17022022.csv",
	"meds_mother_beforepreg_wide_17022022.csv",
	"mbr_structural_vars.csv",
	"sibling_endpoints_wide_15022022.csv",
	"meds_pregnancy_wide_17022022.csv",
	"neonatal_diagnoses_feb_22.csv",
	"father_endpoints_wide_15022022.csv", 
	"mother_endpoints_wide_15022022.csv"
)


filename_df <- data.frame(source = allnames, filename = filenames) %>% as_tibble



## loop to read all the predictor datasets and extract variables of interest

composite_data <- left_join(development_set, all_bds, by = "TNRO")

# loop that reads all the predictor files and
# extracts preselectedf variables
for(i in 1:length(allnames)) {

tempname <- allnames[i]

# replace cool_var_df with all_possible_var_df if 
# want to examine all possible vars
tempvar_names <- cool_var_df %>% # replacement here
	filter(source == tempname) %>%
	pull(var)

	tempdata <- fread(paste0(pred_path, filenames[i]), 
						select = c("TNRO", tempvar_names)) %>% as_tibble %>%
		rename_with(~ paste0(., "_", tempname), -TNRO)

	composite_data <- left_join(composite_data, tempdata, by = "TNRO")

	print(i)

}




#
#
#
composite_data
#
#
#




# create features that combine information from end points
composite_data <- composite_data %>%
	# create term breathing difficulties
	mutate(
		other_term_breathing = case_when(
			term_rds_neodg == 1 ~ 1,
			(P24_neodg == 1 & gest_days > 258) ~ 1,
			(P22_other_unsp_neodg & gest_days > 258) == 1 ~ 1,
			TRUE ~ 0)) %>%
	# term hypoglycemia
	mutate(
		term_hypoglycemia = case_when(
				(P70_neodg == 1 & gest_days > 258) ~ 1,
			TRUE ~ 0)) %>%
	mutate(
		# Valproate?
		neonate_substance = case_when(
			P04_other_neodg == 1 ~ 1,
			P961_neodg == 1 ~ 1, 
			P044_neodg == 1 ~ 1,
			O355_neodg == 1 ~ 1,
			O354_neodg == 1 ~ 1,
			opioids_mother_drug_pregnancy == 1 ~ 1,
			TRUE ~ 0)) %>%
	mutate(
		mother_asthma = case_when(
			(inhaled_steroids_mother_drugs == 1 |
			 J10_ASTHMACOPDKELA_mother_ep == 1) ~ 1,
			TRUE ~ 0),
		father_asthma = case_when(
			(inhaled_steroids_father_drugs == 1 |
			 J10_ASTHMACOPDKELA_father_ep == 1) ~ 1,
			TRUE ~ 0)) %>%
# composites of drugs during pregnancy
	mutate(
		pregnancy_inhalation_meds = case_when(
			inhaled_opening_drugs_mother_drug_pregnancy == 1 ~ 1,
			inhaled_steroids_mother_drug_pregnancy  == 1 ~ 1,
			TRUE ~ 0),
		pregnancy_other_antibiotic = case_when(
			kefexin_mother_drug_pregnancy == 1 ~ 1,
			fluorocinolone_mother_drug_pregnancy == 1 ~ 1,
			macrolide_mother_drug_pregnancy == 1 ~ 1,
			TRUE ~ 0)) %>%
	select(-c(
		"term_rds_neodg",
		"premie_rds_neodg",

		"P24_neodg",
		"P22_other_unsp_neodg",

		"P70_neodg",


		"P04_other_neodg",
		"P961_neodg",
		"P044_neodg",
		"O355_neodg",
		"O354_neodg",

		"J10_ASTHMACOPDKELA_mother_ep",
		"J10_ASTHMACOPDKELA_father_ep",
		"J10_ASTHMACOPDKELA_father_ep",
		"J10_ASTHMACOPDKELA_mother_ep",
		"inhaled_steroids_mother_drugs",
		"inhaled_steroids_father_drugs",

		"inhaled_opening_drugs_mother_drug_pregnancy",
		"inhaled_steroids_mother_drug_pregnancy",
		"kefexin_mother_drug_pregnancy",
		"fluorocinolone_mother_drug_pregnancy",
		"macrolide_mother_drug_pregnancy",
		"opioids_mother_drug_pregnancy"
		)) %>%
	mutate(birth_year = as.numeric(year(LAPSEN_SYNTYMAPVM))) %>%
	mutate(epidemic_year = ifelse(birth_month <= 5, birth_year, (birth_year+1))) %>%
	mutate(even_epidemic_year = ifelse(epidemic_year %% 2 == 0, 1, 0)) 


# code for defining a categorical variable for birth year and birth month
# composite_data <- composite_data %>%
# 	mutate(epidemic_year = as.factor(epidemic_year)) %>%
# 	mutate(birth_yearmonth = paste(birth_month, birth_year, sep = "/")) %>%
# 	mutate(birth_yearmonth = as.factor(birth_yearmonth)) %>%
# 	mutate(birth_month_factor = as.factor(birth_month))



# save composite data for further use
# we can further define the variables later, but we'll currently do the 
# stepwise AIC filtering with the original variables. 

setwd("/data/projects/project_pvartiai/rsv/modeling")
write.csv(composite_data, "composite_data_all_candidate_predictors.csv",
					row.names = FALSE)





#  C R E A T E   A N D   S A V E   A L L   P O S S I B L E  V A R I A B L E S
#   C O M P O S I T E   D A T A   # 
# create and save all possible composite data



## loop to read all the predictor datasets and extract variables of interest

all_possible_composite_data <- left_join(development_set, all_bds, by = "TNRO")

# loop that reads all the predictor files and
# extracts preselectedf variables
for(i in 1:length(allnames)) {
		# temporary source variable name
		tempname <- allnames[i]

		# replace cool_var_df with all_possible_var_df if 
		# want to examine all possible vars
		tempvar_names <- all_possible_var_df %>% # replacement here
			filter(source == tempname) %>%
			pull(var)

			tempdata <- fread(paste0(pred_path, filenames[i]), 
								select = c("TNRO", tempvar_names)) %>% as_tibble %>%
				rename_with(~ paste0(., "_", tempname), -TNRO)

			all_possible_composite_data <- left_join(all_possible_composite_data, tempdata, by = "TNRO")

			print(i)

}






# create features that combine information from end points
all_possible_composite_data <- all_possible_composite_data %>%
	# create term breathing difficulties
	mutate(
		other_term_breathing = case_when(
			term_rds_neodg == 1 ~ 1,
			(P24_neodg == 1 & gest_days > 258) ~ 1,
			(P22_other_unsp_neodg & gest_days > 258) == 1 ~ 1,
			TRUE ~ 0)) %>%
	# term hypoglycemia
	mutate(
		term_hypoglycemia = case_when(
				(P70_neodg == 1 & gest_days > 258) ~ 1,
			TRUE ~ 0)) %>%
	mutate(
		# Valproate?
		neonate_substance = case_when(
			P04_other_neodg == 1 ~ 1,
			P961_neodg == 1 ~ 1, 
			P044_neodg == 1 ~ 1,
			O355_neodg == 1 ~ 1,
			O354_neodg == 1 ~ 1,
			opioids_mother_drug_pregnancy == 1 ~ 1,
			TRUE ~ 0)) %>%
	mutate(
		mother_asthma = case_when(
			(inhaled_steroids_mother_drugs == 1 |
			 J10_ASTHMACOPDKELA_mother_ep == 1) ~ 1,
			TRUE ~ 0),
		father_asthma = case_when(
			(inhaled_steroids_father_drugs == 1 |
			 J10_ASTHMACOPDKELA_father_ep == 1) ~ 1,
			TRUE ~ 0)) %>%
# composites of drugs during pregnancy
	mutate(
		pregnancy_inhalation_meds = case_when(
			inhaled_opening_drugs_mother_drug_pregnancy == 1 ~ 1,
			inhaled_steroids_mother_drug_pregnancy  == 1 ~ 1,
			TRUE ~ 0),
		pregnancy_other_antibiotic = case_when(
			kefexin_mother_drug_pregnancy == 1 ~ 1,
			fluorocinolone_mother_drug_pregnancy == 1 ~ 1,
			macrolide_mother_drug_pregnancy == 1 ~ 1,
			TRUE ~ 0)) %>%
	# remove old 'raw' variables used to define vars abofe
	select(-c(
		"term_rds_neodg",
		"premie_rds_neodg",

		"P24_neodg",
		"P22_other_unsp_neodg",

		"P70_neodg",


		"P04_other_neodg",
		"P961_neodg",
		"P044_neodg",
		"O355_neodg",
		"O354_neodg",

		"J10_ASTHMACOPDKELA_mother_ep",
		"J10_ASTHMACOPDKELA_father_ep",
		"J10_ASTHMACOPDKELA_father_ep",
		"J10_ASTHMACOPDKELA_mother_ep",
		"inhaled_steroids_mother_drugs",
		"inhaled_steroids_father_drugs",

		"inhaled_opening_drugs_mother_drug_pregnancy",
		"inhaled_steroids_mother_drug_pregnancy",
		"kefexin_mother_drug_pregnancy",
		"fluorocinolone_mother_drug_pregnancy",
		"macrolide_mother_drug_pregnancy"
		)) %>%
	mutate(birth_year = as.numeric(year(LAPSEN_SYNTYMAPVM))) %>%
	mutate(epidemic_year = ifelse(birth_month <= 5, birth_year, (birth_year+1))) %>%
	mutate(even_epidemic_year = ifelse(epidemic_year %% 2 == 0, 1, 0)) 

#
#
#
all_possible_composite_data
#
#
#

# save the file
setwd("/data/projects/project_pvartiai/rsv/predictors")
write.csv(all_possible_composite_data, "composite_data_all_interesting_variables.csv",
					row.names = FALSE)


	
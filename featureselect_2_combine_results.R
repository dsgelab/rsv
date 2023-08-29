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


allnames <- c(
	"neodg",
	"mbr_structural",
	"mother_ep",
	"father_ep",
	"sib_ep",
	"meds_mother_beforepreg",
	"meds_father_beforepreg",
	"meds_mother_preg",
	"meds_sib")

res_dir <- "/data/projects/project_pvartiai/rsv/results/featureselect/"


for(i in 1:length(allnames)) {

name <- allnames[i]

countname <- paste0(name, "_count.csv")
univarname <- paste0(name, "_univar.csv")
multivarname <- paste0(name, "_multivar.csv")
aicname <- paste0(name, "_aic_multivar.csv")


# ... <- fread(paste0(res_dir, ))



count <- fread(paste0(res_dir, countname)) %>% as_tibble %>%
	rename(feature = name) %>%
		select(-V1)

# base_pred_coefs <- fread(paste0(res_dir, "base_pred_coefs.csv")) %>% as_tibble %>%
# 	select(-V1) %>%
# 	mutate(or = exp(coef)) %>%
# 	select(feature, or, p, coef, se)

univar <- fread(paste0(res_dir, univarname)) %>% as_tibble %>%
	select(-V1) %>%
	mutate(or = exp(coef)) %>%
	select(feature, or, p, coef, se) 

#  base_and_uni <- bind_rows(base_pred_coefs, univar)

base_and_uni <- univar

multivar <- fread(paste0(res_dir, multivarname)) %>% as_tibble %>%
	select(-V1) %>%
	mutate(or = exp(coef)) %>%
	select(feature, or, p, coef, se)

aic <- fread(paste0(res_dir, aicname)) %>% as_tibble %>%
	select(-V1) %>%
	mutate(or = exp(coef)) %>%
	select(feature, or, p, coef, se)



uni_multi <- full_join(base_and_uni, multivar, by = "feature", suffix = c("_uni", "_multi"))
unimulti_aic <- full_join(uni_multi, aic, by = "feature", suffix = c("", "_aic")) %>%
# 	mutate(basepredictor = ifelse(feature %in% base_pred_coefs$feature, 1, 0)) %>%
	rename(or_aic = or,
		p_aic = p,
		coef_aic = coef,
		se_aic = se)


all <- left_join(unimulti_aic, count, by = "feature") %>%
	arrange(desc(feature)) 


setwd("/data/projects/project_pvartiai/rsv/results_refined/")
write.csv(all, paste0(name, "_featureselect_results.csv"))

print(name)
}


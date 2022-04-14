# Kela drug purchases. 
# This script extracts drug purchases done during pregnancy and last 3 years before childbirth.
# Drug purchases are separated for father and mother before pregnancy and during pregnancy.

# The raw file reading is done using a for loop, reading 1 file at a time 
# (the files are for drug purchases during calendar year). 

# Certain interesting drugs are hand-picked.
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



# directories
kela_purch_dir <- "/data/processed_data/kela_purchase/"
# family members

# UPDATE PATHS to "/home/pvartiai/RSV/data/family/..."
sibling_path <- "/home/pvartiai/RSV/data/wrangle/siblings_from_mbr_and_relatives.csv"

mothers_path <- "/home/pvartiai/RSV/data/mothers.csv"
fathers_path <- "/home/pvartiai/RSV/data/fathers.csv" 


#ids, outcome, gest.age, mother_id
basic_vars <- fread("/home/pvartiai/RSV/data/id_and_basic_vars_all_study_patients.csv",
                    select = c("TNRO", "outcome", "gest_days", "LAPSEN_SYNTYMAPVM")) %>%
    as_tibble 

# rescue_drugs is a vector of interesting drug atc codes that we at least want to keep,
# regardless of regularity. i.e. p.o. glucocorticoids can be used as 
# a temporary help for severe asthma

# rescue drugs

inhaled_steroids <- c("R03BA", "R03AK")
inhaled_opening_drugs <- "R03AC"
montelucast <- "R03DC"
adrenaline <- "C01CA"
po_cortison <- "H02AB"
thyroxin <- "H03AA"
ppi <- "A02BC"
valproate <- c("N03AG")
cortison_cream <- c("D07AA", "D07AB", "D07AC")
antihistamin <- c("R06AE", "R06AX")
nasal_steroid <- c("R01AD")
opioids <- c("N02AX", "N02AJ", "N02AA")
benz <- c("N05BA")

#antibiotics
common_uti_ab_amorion <- c("J01EA", "J01CA", "J01XE", "J01EE")
penicillin <- c("J01CE")
kefexin <- c("J01DB")
fluorocinolone <- c("J01MA")
amorion_comp <- "J01CR"
macrolide <- "J01FA"


antibiotics <- c(common_uti_ab_amorion, penicillin, kefexin, fluorocinolone, amorion_comp, macrolide)

rescue_drugs <- c(inhaled_steroids, inhaled_opening_drugs,
                  montelucast, adrenaline, po_cortison, thyroxin, ppi, valproate,
                  cortison_cream, antihistamin, nasal_steroid, opioids, benz, antibiotics)


# family members - load the data and edit for drug purchase extraction

# Excluding row number variable, created in saving.
sibs <- fread(sibling_path) %>% as_tibble %>% select(-V1)
mothers <- fread(mothers_path) %>% as_tibble %>% select(-V1)
fathers <- fread(fathers_path) %>% as_tibble %>% select(-V1)

# extract birth date and gestational days so that we can compute roughly the 
# start of pregnancy, and filter drug purchases before/after it. 
viim_kk <- basic_vars %>% select(TNRO, gest_days)

#join to mother's ids
mother_drug <- left_join(mothers, viim_kk, by = "TNRO")

# calculate the start of pregnancy variable (viim_kk)
mother_drug <- mother_drug %>%
    mutate(kid_dob = as.Date(kid_dob)) %>%
    mutate(viim_kk = kid_dob - gest_days) %>%
    select(-gest_days)

father_drug <- left_join(fathers, viim_kk, by = "TNRO") %>%
    mutate(kid_dob = as.Date(kid_dob)) %>%
    mutate(viim_kk = kid_dob - gest_days) %>%
    select(-gest_days)




## 
# for loop that reads 
kela_preg_purchases <- NULL
kela_mother_beforepreg_purchases <- NULL
kela_father_purchases <- NULL

# 1995:2019
for (i in 1995:2019) {
    
    longi_dir <- "/data/processed_data/kela_purchase/"
    temp_name <- paste0("175_522_2020_LAAKEOSTOT_", i, ".csv.finreg_IDsp")
    temp_path <- paste0(longi_dir, temp_name)
    
    
    # temporary raw kela file
    temp_kela_file <- fread(temp_path,
                            select = c("HETU", "ATC", "OSTOPV")) %>% 
        as_tibble %>%
        rename(FINREGISTRYID = HETU, 
               PVM = OSTOPV) %>%
        # mutate ATC numbers to 
        mutate(ATC = substr(ATC, 1, 5))
    
    
    
    # mothers' purchases
    kela_mother_filtered <- temp_kela_file %>%
        filter(FINREGISTRYID %in% mother_drug$parent_TNRO) %>%
        rename(parent_TNRO = FINREGISTRYID)
    
    
    # mothers
    temp_md <- mother_drug %>%
        filter(parent_TNRO %in% kela_mother_filtered$parent_TNRO)
    
    temp_joined <- left_join(temp_md, kela_mother_filtered, 
                             by = "parent_TNRO")
    
    preg_purch <- temp_joined %>% 
        filter(PVM < kid_dob) %>%
        filter(PVM > viim_kk)
    
    beforepreg_purch <- temp_joined %>%
        filter(PVM < viim_kk) %>%
        filter(PVM > (viim_kk - years(5)))
    
    kela_preg_purchases <- bind_rows(kela_preg_purchases, preg_purch)
    kela_mother_beforepreg_purchases <- bind_rows(kela_mother_beforepreg_purchases, beforepreg_purch)
    
    rm(preg_purch, temp_joined, temp_md, kela_mother_filtered)
    
    
    
    # father
    kela_father_filtered <- temp_kela_file %>%
        filter(FINREGISTRYID %in% father_drug$parent_TNRO) %>%
        rename(parent_TNRO = FINREGISTRYID)
    
    
    temp_father_dob <- father_drug %>%
        filter(parent_TNRO %in% kela_father_filtered$parent_TNRO)
    
    
    father_joined <- left_join(temp_father_dob, kela_father_filtered, by = "parent_TNRO")
    
    father_purch <- father_joined %>%
        filter(PVM < viim_kk) %>%
        filter(PVM > (viim_kk - years(5)))
    
    kela_father_purchases <- bind_rows(kela_father_purchases, father_purch)
    
    rm(father_purch, father_joined, temp_father_dob, kela_father_filtered)
    
    print(i)
    
} # for loop ends #

#results of for loop
kela_preg_purchases
kela_mother_beforepreg_purchases
kela_father_purchases

# save the raw individual purchases
# setwd("/home/pvartiai/RSV/data/predictors/meds/")
# write.csv(kela_preg_purchases, "raw_indiv_purch_meds_pregnancy_longitudinal_17022022.csv")
# write.csv(kela_mother_beforepreg_purchases, "raw_indiv_purch_meds_mother_beforepreg_longitudinal_17022022.csv")
# write.csv(kela_father_purchases, "raw_indiv_purch_meds_father_beforepreg_longitudinal_17022022.csv")



meds_mother_pregnancy <- kela_preg_purchases %>%
    select(TNRO, parent_TNRO, ATC) %>%
    distinct 


# Filter only regular drug purchases or ones in the rescue list

meds_father_beforepreg <- kela_father_purchases %>%
    select(TNRO, parent_TNRO, ATC) %>%
    group_by(TNRO, ATC) %>%
    summarise(n = n()) %>%
    ungroup() %>%
    filter(n > 9 |
               (ATC %in% adrenaline) |
               (ATC %in% po_cortison & n > 3)|
               (ATC %in% montelucast & n > 4)|
               (ATC %in% nasal_steroid & n > 4)|
               (ATC %in% inhaled_opening_drugs & n > 4)|
               (ATC %in% antihistamin & n > 4)|
               (ATC %in% antibiotics & n > 4)
    )


meds_mother_beforepreg <- kela_mother_beforepreg_purchases %>%
    select(TNRO, parent_TNRO, ATC) %>%
    group_by(TNRO, ATC) %>%
    summarise(n = n()) %>%
    ungroup() %>%
    filter(n > 9|
               (ATC %in% adrenaline) |
               (ATC %in% po_cortison & n > 3)|
               (ATC %in% montelucast & n > 4)|
               (ATC %in% nasal_steroid & n > 4)|
               (ATC %in% inhaled_opening_drugs & n > 4)|
               (ATC %in% antihistamin & n > 4)|
               (ATC %in% antibiotics & n > 4)
    ) 



# function for extracting the more common atc classes.
pull.common.atcs <- function(data) {
    
    data %>%
        group_by(ATC) %>%
        summarise(atc_count = n()) %>%
        filter(atc_count > 2000) %>%
        pull(ATC)
}

group.certain.drugs <- function(data) {
    
    data %>%
        mutate(ATC = case_when(
            ATC %in% inhaled_steroids ~ "inhaled_steroids",
            ATC %in% inhaled_opening_drugs ~ "inhaled_opening_drugs",
            ATC %in% montelucast ~ "montelucast",
            ATC %in% adrenaline ~ "adrenaline",
            ATC %in% po_cortison ~ "po_cortison",
            ATC %in% thyroxin ~ "thyroxin",
            ATC %in% ppi ~ "ppi",
            ATC %in% valproate ~ "valproate",
            ATC %in% cortison_cream ~ "cortison_cream",
            ATC %in% antihistamin ~ "antihistamin",
            ATC %in% nasal_steroid ~ "nasal_steroid",
            
            ATC %in% opioids ~ "opioids",
            ATC %in% benz ~ "benz",
            
            ATC %in% common_uti_ab_amorion ~ "common_uti_ab_amorion",
            ATC %in% penicillin ~ "penicillin",
            ATC %in% kefexin ~ "kefexin",
            ATC %in% fluorocinolone ~ "fluorocinolone",
            ATC %in% macrolide ~ "macrolide",
            ATC %in% amorion_comp ~ "amorion_comp",
            TRUE ~ ATC
        ))
}


common_preg_drugs <- meds_mother_pregnancy %>% 
    pull.common.atcs()

common_mother_beforepreg_drugs <- meds_mother_beforepreg %>% 
    pull.common.atcs() 

common_father_drugs <- meds_father_beforepreg %>%
    pull.common.atcs() 


meds_mother_pregnancy <- meds_mother_pregnancy %>%
    filter(ATC %in% c(rescue_drugs, common_preg_drugs)) %>%
    group.certain.drugs()

meds_mother_beforepreg <- meds_mother_beforepreg %>%
    filter(ATC %in% c(rescue_drugs, common_mother_beforepreg_drugs)) %>%
    group.certain.drugs()

meds_father_beforepreg <- meds_father_beforepreg %>%
    filter(ATC %in% c(rescue_drugs, common_father_drugs)) %>%
    group.certain.drugs()


setwd("/home/pvartiai/RSV/data/predictors/meds/")
write.csv(meds_mother_pregnancy, "meds_pregnancy_longitudinal_17022022.csv",
    row.names = FALSE)
write.csv(meds_mother_beforepreg, "meds_mother_beforepreg_longitudinal_17022022.csv",
    row.names = FALSE)
write.csv(meds_father_beforepreg, "meds_father_beforepreg_longitudinal_17022022.csv",
    row.names = FALSE)


# test if there are duplicate rows ...

# wide 

all_ids <- basic_vars %>% select(TNRO)

pivot.drug.to.wide <- function(data, ids = all_ids) {
    tempwide <- data %>%
    select(TNRO, ATC) %>%
    filter(ATC != "") %>%
    distinct() %>%
    mutate(value = 1) %>%
    pivot_wider(values_from = value,
                names_from = ATC,
                values_fill = 0)

    temp_joined <- left_join(ids, tempwide, by = "TNRO")

    temp_joined[is.na(temp_joined)] <- 0

    temp_joined
}

meds_mother_beforepreg_wide <- pivot.drug.to.wide(data = meds_mother_beforepreg)
meds_father_beforepreg_wide <- pivot.drug.to.wide(data = meds_father_beforepreg)
meds_mother_pregnancy_wide <- pivot.drug.to.wide(data = meds_mother_pregnancy)


setwd("/data/projects/project_pvartiai/rsv/predictors/")
write.csv(meds_mother_pregnancy_wide, "meds_pregnancy_wide_17022022.csv",
        row.names = FALSE)
write.csv(meds_mother_beforepreg_wide, "meds_mother_beforepreg_wide_17022022.csv",
        row.names = FALSE)
write.csv(meds_father_beforepreg_wide, "meds_father_beforepreg_wide_17022022.csv",
        row.names = FALSE)


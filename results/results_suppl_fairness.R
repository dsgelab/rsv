# decision curve analysis

library(ggplot2)
library(tidyr)
library(dplyr)
library(data.table)
library(lubridate)
library(stringr)
library(dcurves)


# load the data
data_path <- "/data/projects/project_pvartiai/rsv/modeling/"
model_path <- "/data/projects/project_pvartiai/rsv/functions/model/"
pred_path <- "/data/projects/project_pvartiai/rsv/predictors/"



recent_data <- fread(paste0(data_path, "development_set_only_feasible_variables.csv")) %>%
  filter(epidemic_year >= 2007)



test_set <- fread(paste0(val_path, "validation_set_regression.csv")) %>%
  as_tibble() %>%
  distinct()


# Load model objects and knot locations

setwd("/data/projects/project_pvartiai/rsv/functions/model/")

final_form_simple <- get(load("/data/projects/project_pvartiai/rsv/functions/model/final_formula_object_nointeract.R"))
load("rcsknots_motherage.R")
load("rcsknots_dist.R") 
load("rcsknots_gestdays.R")
load("rcsknots_weightsd.R")
model_fit <- get(load("final_model_fit.R"))

setwd("/data/projects/project_pvartiai/rsv/functions/")
load("keylist_final_variable_names.R")

keylist_final %>% print(n = 100)


##### SENSITIVITY

# predict
test_set$pred <- predict(model_fit, newdata = test_set, type = "response")

### BPD data, used for aap calculation
bpd_path <- paste0(pred_path, "bpds.csv")
bpd <- fread(bpd_path) %>% as_tibble

## modified aap criteria (immunize/no immunize)
modified_aap <- test_set %>%
  left_join(., bpd, by = "TNRO") %>%
  mutate(aap_criterion = case_when(
    gest_days < 203 ~ 1,
    bpd == 1 ~ 1,
    any_severe_chd == 1 ~ 1,
    TRUE ~ 0
  )) %>%
  select(TNRO, aap_criterion)



#### XGBOOST
ml_path <- "/data/projects/project_pvartiai/rsv/machinelearning/"
ml_name <- "ValidationPredictions.txt"


# load XGboost results
# xgboost_res <- fread(paste0(ml_path, ml_name)) %>%
#   as_tibble %>%
#   select(-any_of("predictedOutcome")) %>%
#   left_join(., all_bds, by = "TNRO") %>%
#   mutate(birth_year = year(LAPSEN_SYNTYMAPVM),
#          birth_month = month(LAPSEN_SYNTYMAPVM)) %>%
#   mutate(epidemic_year = ifelse(birth_month < 6, birth_year, birth_year + 1)) %>%
#   select(-any_of(c("birth_month", "birth_year"))) %>%
#   rename(pred = predictedProb)




# xgboost_pred <- xgboost_res %>%
#   select(TNRO, pred) %>%
#   rename(xgboost_pred = pred)


# MOTHER TONGUE
pred_path <- "/data/projects/project_pvartiai/rsv/predictors/"
# moto <- fread(paste0(pred_path, "mother_tongue.csv")) %>% as_tibble


# moto_test <- left_join(test_set, moto) %>%
#   mutate(moto_other_finswe = ifelse(mother_tongue %in% c("fi", "swe"), 1, 0))


# moto_test %>%
#   group_by(moto_bin) %>%
#   summarise(auc = ci.auc(outcome, pred)[2],
#             auc_low = ci.auc(outcome, pred)[1],
#             auc_high = ci.auc(outcome, pred)[3])


# INCOME

income_dir <- "/data/processed_data/etk_pension/"
income_name <- "vuansiot_2022-05-12.csv"
income_path <- paste0(income_dir, income_name)

all_income <- fread(income_path) %>%
  as_tibble

all_income <- all_income %>%
  rename(parent_TNRO = id)


# PARENTS
parents_path <- "/data/projects/project_pvartiai/rsv/family/parents.csv"

parents <- fread(parents_path)

testset_parents <- parents %>%
  filter(TNRO %in% test_set$TNRO) %>% 
  as_tibble %>%
  left_join(., all_bds, by = "TNRO") %>%
  mutate(birth_year = year(LAPSEN_SYNTYMAPVM))


testset_parents_income <- all_income %>%
  filter(parent_TNRO %in% parents$parent_TNRO) %>%
  left_join(., testset_parents, by = "parent_TNRO")


year_income <- testset_parents_income %>%
  filter(vuosi == birth_year - 1) 



income_per_parent <- year_income %>%
  group_by(TNRO, parent_TNRO) %>%
  summarise(income_sum = sum(vuosiansio_indexed),
            year = vuosi,
            birth_year = birth_year)


income_per_kid <- income_per_parent %>%
  ungroup %>%
  group_by(TNRO) %>%
  # no duplicateds
  summarise(parental_income = sum(income_sum)) %>%
  mutate(income_quintile = ntile(parental_income, 5))


test_set_income <- test_set %>%
  left_join(income_per_kid, by = "TNRO") 

# test_set_moto <- test_set %>%
# left_join(moto, by = "TNRO")



get.se <- function(x) {sqrt(var(x) / length(x))}

income_auc <- test_set_income %>%
  group_by(income_quintile) %>%
  summarise(metric = as.numeric(auc(outcome, pred)),
            ci_low =  as.numeric(ci.auc(outcome, pred))[1],
            ci_high = as.numeric(ci.auc(outcome, pred))[3],
            median_income = median(parental_income, na.rm = T) %>%
              round(., digits = -2)) %>%
  na.omit() %>%
  mutate(label = "C-statistic")

income_rate <- test_set_income %>%
  group_by(income_quintile) %>%
  summarise(metric = mean(outcome, na.rm = T),
            ci_high = metric + 1.96*get.se(outcome),
            ci_low = metric - 1.96*get.se(outcome),
            median_income = median(parental_income, na.rm = T) %>%
              round(., digits = -2)) %>%
  na.omit() %>%
  mutate(label = "RSV hospitalisation rate")






auc_income <- income_auc %>%
  ggplot(., aes(x = as.factor(median_income),
                y = metric, 
                ymin = ci_low,
                ymax = ci_high)) + 
  geom_point() + 
  geom_errorbar(width = 0.1) + 
  labs(x = "Median parental income, EUR",
       y = "C-statistic",
       title = "A") + 
  theme_bw(base_size = 14)

rate_income <- income_rate %>%
  ggplot(., aes(x = as.factor(median_income),
                y = metric, 
                ymin = ci_low,
                ymax = ci_high)) + 
  geom_col(col = "white", fill = "grey",
           width = 0.4) + 
  geom_errorbar(width = 0.1) + 
  labs(x = "Median parental income, EUR",
       y = "RSV hospitalisation rate",
       title = "B") + 
  theme_bw(base_size = 14) + 
  scale_y_continuous(expand = c(0,0),
                     limits = c(0, 0.031))


auc_income
rate_income



my_layout <- cbind(c(1, 2))
my_layout

my_plotlist <- list(auc_income, rate_income)


grid.arrange(grobs = my_plotlist, layout_matrix = my_layout)

g <- arrangeGrob(grobs = my_plotlist, layout_matrix = my_layout)

# size
# https://localhost:8443/rstudio/graphics/plot.png?width=602&height=602&randomizer=526940368

ggsave(plot = g,
        filename = "suppl_fairness_without_eur.pdf",
        device = "pdf",
        width = 9,
        height = 9,
        dpi = 1200,
        path = "/data/projects/project_pvartiai/rsv/results/plots/")



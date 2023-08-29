# decision curve analysis

library(ggplot2)
library(tidyr)
library(dplyr)
library(data.table)
library(lubridate)
library(stringr)
library(dcurves)
library(graphPAF)


# load the data
data_path <- "/data/projects/project_pvartiai/rsv/modeling/"
model_path <- "/data/projects/project_pvartiai/rsv/functions/model/"
pred_path <- "/data/projects/project_pvartiai/rsv/predictors/"
val_path <- "/data/projects/project_pvartiai/rsv/validation_data/"


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


##### VALIDATION

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
# ml_path <- "/data/projects/project_pvartiai/rsv/machinelearning/"
# ml_name <- "ValidationPredictions.txt"


# # load XGboost results
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




dca_data <- tibble(
  TNRO = test_set$TNRO,
  outcome = test_set$outcome %>% as.numeric(),
  prob = test_set$pred) %>%
  left_join(., modified_aap, by = "TNRO") 

# no xgboost data for DCA  
# %>% left_join(., xgboost_pred, by = "TNRO")



finnish_dca <- dca(formula = outcome ~ prob + aap_criterion, 
                   data = dca_data,
                   thresholds = seq(0, 0.15, by = 0.001)) %>% 
  as_tibble %>%
  mutate(Country = "Finland") %>%
  mutate(label = fct_recode(label, 
                            "Prediction model" = "prob",
                            "AAP criteria" = "aap_criterion"))


#### SWEDISH OBJECT
swed_path <- "/data/projects/project_pvartiai/rsv/results/sweden/"

dca_swed <- get(load(paste0(swed_path, "dca_object.R"))) 

dca_swed <- dca_swed %>%
  mutate(label = fct_recode(label, 
                            "Prediction model" = "prediction",
                            "AAP criteria" = "aap_criterion")) 

# 
# both_dca <- bind_rows(finnish_dca, dca_swed)
# 
# decision_curve_fin <- both_dca %>%
#     ggplot(aes(y = net_benefit,
#              x = threshold,
#              group = label,
#              col = label)) + 
#   geom_line(size = 1) + 
#   scale_y_continuous(limits = c(-0.005, 0.016)) + 
#   theme_bw(base_size = 18) + 
#   scale_color_manual(values = c("black", 
#                                 "darkgrey",
#                                 "orange", 
#                                 "darkgreen")) + 
#   labs(y = "Net benefit",
#        x = "Risk threshold",
#        col = NULL)
# 
# 
# decision_curve_swe <- dca_swed %>%
#   mutate(label = fct_recode(label, 
#                             "Prediction model" = "prediction")) %>%
#   
#   ggplot(aes(y = net_benefit,
#              x = threshold,
#              group = label,
#              col = label)) + 
#   geom_line(size = 1) + 
#   scale_y_continuous(limits = c(-0.005, 0.016)) + 
#   theme_bw(base_size = 18) + 
#   scale_color_manual(values = c("black", 
#                                 "darkgrey",
#                                 "#D6C533")) + 
#   labs(y = "Net benefit",
#        x = "Risk threshold",
#        col = NULL)
# 
# decision_curve_swe
# 

### test combining the two
all_dca <- bind_rows(finnish_dca, dca_swed)

dca_faceted <- all_dca %>%
  ggplot(aes(y = net_benefit,
             x = threshold,
             group = label,
             col = label)) + 
  geom_line(size = 0.75) + 
  scale_y_continuous(limits = c(-0.0058, 0.0125),
                     expand = c(0,0)) + 
  theme_bw(base_size = 14) + 
  facet_wrap(~Country) + 
  scale_x_continuous(expand = c(0,0),
                     limits = c(0,0.13),
                     breaks = pretty_breaks(5)) + 
  scale_color_manual(values = c("black",
                                "darkgrey",
                                "tomato",
                                "royalblue3")) +
  labs(y = "Net benefit",
       x = "Threshold probability",
       subtitle = "A) Decision curve analysis",
       col = "Prediction method  ") + 
  theme(legend.position = "right",
        strip.background = element_rect(fill = "grey95"))

dca_faceted


# #### IN ONE IMAGE
# finnish_dca_2 <- finnish_dca %>%
#   mutate(label = fct_recode(label, "Treat all (Finland)" = "Treat All"))
# 
# dca_swed_2 <- dca_swed %>%
#   mutate(label = fct_recode(label, "Treat all (Sweden)" = "Treat All"))
# 
# all_dca_2 <- bind_rows(dca_swed_2, finnish_dca_2)
# 
# what_dca <- all_dca_2 %>%
#   mutate(label = fct_recode(label, 
#                             "Prediction model\nin Sweden" = "prediction",
#                             "Prediction model\nin Finland" = "prob"
#                             )) %>%
#   
#   ggplot(aes(y = net_benefit,
#              x = threshold,
#              group = label,
#              col = label)) + 
#   geom_line(size = 1) + 
#   scale_y_continuous(limits = c(-0.005, 0.016)) + 
#   theme_bw(base_size = 18) + 
#   scale_color_manual(values = c("black", 
#                                 "darkgrey",
#                                 "#D6C533",
#                                 "tomato",
#                                 "navy",
#                                 "orange")) + 
#   labs(y = "Net benefit",
#        x = "Risk threshold",
#        col = NULL)
# 





#### EXPLORE WITH PERCENTILE APPROACH

#### HYPOTHETICAL NNT CALCULATION IN FINLAND
# vector of cutoff probabilities for each percentile
cutoff <- test_set$pred %>%
  quantile(., probs = seq(0,1,0.01), na.rm = T)

cutoff_perc <- seq(0,1,0.01)
cutoff_perc[101] <- 0


# initialize the loop, create empty result frame
result_frame <- tibble(
  n_treated = NA,
  percent_vaccinated = NA,
  nnt = NA,
  epidemic_year = NA
)


# prophylaxis efficacy
efficacy <- 0.6

for(i in 1:100) {
  
  temp_cutoff <- cutoff[i+1]
  temp_perc_vacc <- (1-cutoff_perc[i+1])*100
  
  n_treated_all <- test_set %>%
    filter(pred >= temp_cutoff) %>%
    nrow()
  
  n_hosp_all_above_cutoff <- test_set %>%
    mutate(top_n = ifelse(pred >= temp_cutoff, 1, 0)) %>%
    filter(top_n == 1) %>%
    group_by(outcome) %>%
    summarise(n = n()) %>%
    filter(outcome == 1) %>%
    pull(n)
  
  n_prevented_all <- n_hosp_all_above_cutoff*0.6  
  nnt_all <- n_treated_all/n_prevented_all
  
  temp_all_result_frame <- data.frame(
    n_treated = n_treated_all,
    percent_vaccinated = temp_perc_vacc,
    nnt = nnt_all,
    epidemic_year = "combined 3 years")
  
  result_frame <- bind_rows(result_frame, temp_all_result_frame)
  
  for(j in 1:3) {
    
    epidemic_year_j <- c(2018:2020)[j]
    
    n_treated <- test_set %>%
      filter(epidemic_year == epidemic_year_j) %>%
      filter(pred >= temp_cutoff) %>%
      nrow()
    
    n_hosp_in_kids_above_cutoff <- test_set %>%
      filter(epidemic_year == epidemic_year_j) %>%
      mutate(top_n = ifelse(pred >= temp_cutoff, 1, 0)) %>%
      filter(top_n == 1) %>%
      group_by(outcome) %>%
      summarise(n = n()) %>%
      filter(outcome == 1) %>%
      pull(n)
    
    prevented <- n_hosp_in_kids_above_cutoff*0.6
    
    nnt <- n_treated/prevented
    
    temp_result_frame <- data.frame(
      n_treated = n_treated,
      percent_vaccinated = temp_perc_vacc,
      nnt = nnt,
      epidemic_year = as.character(epidemic_year_j))
    
    result_frame <- bind_rows(result_frame, temp_result_frame)
    
  }  
  print(i)
  
}


overall_outcome_rate <- test_set %>%
  summarise(outcome_rate = mean(outcome, na.rm = T)) %>%
  mutate(epidemic_year = "combined 3 years")


outcome_rates <- test_set %>%
  group_by(epidemic_year) %>%
  summarise(ntot = n(),
            n_hosps = sum(outcome, na.rm = T)) %>%
  mutate(outcome_rate = n_hosps/ntot) %>%
  select(epidemic_year, outcome_rate) %>%
  mutate(epidemic_year = as.character(epidemic_year)) %>%
  bind_rows(overall_outcome_rate)


result_frame <- result_frame %>%
  na.omit() %>%
  mutate(epidemic_year = as.factor(epidemic_year)) %>%
  left_join(outcome_rates, by = "epidemic_year")

result_frame_finland <- result_frame %>%
  mutate(Country = "Finland")

# load swedish NNT results
swedish_nnt <- get(load(paste0(swed_path, "nnt_results.R"))) %>%
  filter(n_treated > 1)


both_nnt <- bind_rows(result_frame_finland, swedish_nnt) %>%
  mutate(epidemic_year = fct_recode(epidemic_year,
                                    "Pooled data from\n2018-2020" = "combined 3 years")) %>%
  mutate(temp_year = paste0(epidemic_year, "\n(", round(outcome_rate*100, digits = 1), " %)")) %>%
  mutate(summary = as.factor(ifelse(epidemic_year == "Pooled data from\n2018-2020", 0, 1))) 

annotations <- both_nnt %>%
  select(Country, outcome_rate, epidemic_year, nnt, percent_vaccinated) %>%
  filter(round(percent_vaccinated,0) == 10) %>%
  filter(!(epidemic_year %in% c("2018", "2019", "2020"))) %>%
  unique() %>%
  mutate(nnt = 175) %>%
  mutate(label = paste0("RSV-hospitalisation\nrate = ", 
                        round(outcome_rate*100, digits = 2),
                        "%"))




nnt_plot <- both_nnt %>%
  ggplot(aes(
    x = percent_vaccinated,
    y = nnt,
    group = epidemic_year,
    col = epidemic_year,
    linetype = epidemic_year,
    size = epidemic_year
  )) + 
  geom_line() + 
  
  scale_y_continuous(limits = c(0, 230),
                     expand = c(0,0)) + 
  scale_x_continuous(expand = c(0,0)) +
  
  scale_color_manual(values = c("lightslategrey", "maroon1", "lightsalmon3", "black")) + 
  scale_linetype_manual(values = c(2,2,2,1)) + 
  scale_size_manual(values = c(0.7, 0.7, 0.7, 1)) + 
  
  guides(size = FALSE) + 
  
  facet_wrap(~Country) +
  labs(x = "Cutoff for predicted risk percentile, %",
       y = "NNT to prevent 1 RSV-hospitalisation",
       color = "RSV epidemic year",
       linetype = "RSV epidemic year",
       subtitle = "B) Hypothetical NNT for immunisation using the model") + 
  
  geom_text(
    data = annotations,
    aes(label = label,
        x = percent_vaccinated,
        y = nnt),
    inherit.aes = FALSE,
    show.legend = FALSE,
    size = 4,
    hjust = 0) + 
  theme_bw(base_size = 14) + 
  theme(legend.position = "right",
        strip.background = element_rect(fill = "grey95")) 

nnt_plot











#### combine plots

my_plotlist <- list(dca_faceted, nnt_plot)

my_layout <- rbind(c(1, 1), c(2,2))

grid.arrange(grobs = my_plotlist, layout_matrix = my_layout)

g <- arrangeGrob(grobs = my_plotlist, layout_matrix = my_layout)

# size: https://localhost:8443/rstudio/graphics/plot_zoom_png?width=1155&height=1068

ggsave(g,
       filename = "figure5.eps",
       path = "/data/projects/project_pvartiai/rsv/results/plots/",
       dpi = 1200,
       width = 11.51,
       height = 10.68,
       device = cairo_ps)



dca_faceted
ggsave(dca_faceted,
       filename = "figure5_panel_a.eps",
       path = "/data/projects/project_pvartiai/rsv/results/plots/",
       dpi = 1200,
       width = 11.51,
       height = 5.5,
       device = cairo_ps)





nnt_plot_no_labels <- both_nnt %>%
  ggplot(aes(
    x = percent_vaccinated,
    y = nnt,
    group = epidemic_year,
    col = epidemic_year,
    linetype = epidemic_year,
    size = epidemic_year
  )) + 
  geom_line() + 
  
  scale_y_continuous(limits = c(0, 230),
                     expand = c(0,0)) + 
  scale_x_continuous(expand = c(0,0)) +
  
  scale_color_manual(values = c("lightslategrey", "maroon1", "lightsalmon3", "black")) + 
  scale_linetype_manual(values = c(2,2,2,1)) + 
  scale_size_manual(values = c(0.7, 0.7, 0.7, 1)) + 
  guides(size = FALSE) + 
  facet_wrap(~Country) +
  labs(x = "Cutoff for predicted risk percentile, %",
       y = "NNT to prevent 1 RSV-hospitalisation",
       color = "RSV epidemic year",
       linetype = "RSV epidemic year",
       subtitle = "B) Hypothetical NNT for immunisation using the model") + 
  
  theme_bw(base_size = 14) + 
  theme(legend.position = "right",
        strip.background = element_rect(fill = "grey95")) 


ggsave(nnt_plot_no_labels,
       filename = "figure5_panel_b.eps",
       path = "/data/projects/project_pvartiai/rsv/results/plots/",
       dpi = 1200,
       width = 11.51,
       height = 5.5,
       device = cairo_ps)





######## NNNT in epidemics

# some parameters
efficacy <- 0.6
top_10_cutoff <- test_set$pred %>%
  quantile(., probs = 0.90, na.rm = T)

# data
nnt_data <- test_set %>%
  select(TNRO, epidemic_year, outcome, pred) %>%
  left_join(modified_aap, by = "TNRO") %>%
  mutate(top_10 = ifelse(pred >= top_10_cutoff, 1, 0))

all_hosps <- nnt_data %>%
  group_by(epidemic_year) %>%
  na.omit() %>%
  summarise(n_hosp_all = sum(outcome),
            n_top_10 = sum(top_10, na.rm = T),
            n_overall = n()) 

prevented_hosps <- nnt_data %>%
  filter(top_10 == 1) %>%
  group_by(epidemic_year) %>%
  summarise(n_hosps_in_top10 = sum(outcome, na.rm = T)) %>%
  mutate(prevented_hosps_in_top10 = n_hosps_in_top10*efficacy)


compare_top10_to_all <- left_join(all_hosps, prevented_hosps) %>%
  mutate(fraction_of_prevented = prevented_hosps_in_top10/n_hosp_all,
         nnt = n_top_10/prevented_hosps_in_top10)


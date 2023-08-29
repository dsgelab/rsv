# results_suppl_xgboost_disc_cal

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
# splines2 package and function 'ns' will (hopefully) 
# work better in predicting fitted values when using spline functions
# There was a problem in predicting using 'rcs' in rms package. See https://docs.google.com/document/d/1QTnRRcKFMXZ7mM8L60B5kSme4M87xZ4tLIn2iV316XM/edit#heading=h.jnbga7daq4jd
library(splines2)
library(tidymodels)
library(gridExtra)
library(metafor)





# load datasets: predict_data and all_bds

setwd("/data/projects/project_pvartiai/rsv/modeling")
predict_datapath <- "/data/projects/project_pvartiai/rsv/modeling/"
comp_datapath <- "/data/projects/project_pvartiai/rsv/predictors/"
val_path <- "/data/projects/project_pvartiai/rsv/validation_data/"
ml_path <- "/data/projects/project_pvartiai/rsv/machinelearning/"
ml_name <- "ValidationPredictions.txt"


# training data with non-feasible variables filtered out
compact_data <- fread(paste0(predict_datapath, 
                             "development_set_only_feasible_variables.csv")) %>%
  as_tibble %>%
  # add birth month variable to the predict data 
  mutate(birth_month = as.numeric(month(LAPSEN_SYNTYMAPVM))) 


recent_data <- compact_data %>%
  filter(epidemic_year > 2006)

# # load composite data (Pre-AIC-data) if needed
# composite_data <- fread(paste0(comp_datapath, "composite_data_all_candidate_predictors.csv")) %>%
#   as_tibble

# ids and birth dates
all_bds <- fread("/data/projects/project_pvartiai/rsv/wrangle/preprocess_3_mbr_hilmo_outcomes.csv", 
                 select = c("TNRO", "LAPSEN_SYNTYMAPVM")) %>% as_tibble



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

# just in case
test_set_2 <- test_set


#### XGBOOST
# load XGboost results
xgboost_res <- fread(paste0(ml_path, ml_name)) %>%
  as_tibble %>%
  select(-any_of("predictedOutcome")) %>%
  left_join(., all_bds, by = "TNRO") %>%
  mutate(birth_year = year(LAPSEN_SYNTYMAPVM),
         birth_month = month(LAPSEN_SYNTYMAPVM)) %>%
  mutate(epidemic_year = ifelse(birth_month < 6, birth_year, birth_year + 1)) %>%
  select(-any_of(c("birth_month", "birth_year"))) %>%
  rename(pred_xgb = predictedProb) %>%
  distinct()


test_set_x <- test_set %>%
  left_join(., xgboost_res, by = c("TNRO", "epidemic_year", "LAPSEN_SYNTYMAPVM", "outcome"))


#### AUC CALCULATIONS

# overall auc
overall_auc_obj <- as.numeric(ci.auc(test_set$outcome, test_set$pred))

overall_auc <- tibble(
  auc = overall_auc_obj[2],
  ci_high = overall_auc_obj[3],
  ci_low = overall_auc_obj[1],
  epidemic_year = "All test\ndata",
  model = "Clinical prediction model"
)

yearly_auc <- test_set %>%
  group_by(epidemic_year) %>%
  summarise(auc = as.numeric(auc(outcome, pred)),
            ci_low = as.numeric(ci.auc(outcome, pred)[1]),
            ci_high = as.numeric(ci.auc(outcome, pred)[3])) %>%
  mutate(se_auc = (ci_high - auc)/1.96) %>%
  mutate(epidemic_year = as.character(epidemic_year)) 

# meta-analyse AUC
meta_analysis <- rma(auc,
                     sei = se_auc,
                     data = yearly_auc)

rma <- tibble(
  auc = meta_analysis$b %>% as.numeric,
  ci_low = meta_analysis$ci.lb,
  ci_high = meta_analysis$ci.ub,
  epidemic_year = "Random effect\nmeta-analysis")

indiv_auc <- bind_rows(yearly_auc, rma, overall_auc) %>%
  mutate(model = "Clinical prediction model")




overall_auc_obj_x <- as.numeric(ci.auc(xgboost_res$outcome, xgboost_res$pred_xgb))

overall_auc_x <- tibble(
  auc = overall_auc_obj_x[2],
  ci_high = overall_auc_obj_x[3],
  ci_low = overall_auc_obj_x[1],
  epidemic_year = "All test\ndata",
  model = "XGboost")


yearly_auc_x <- xgboost_res %>%
  group_by(epidemic_year) %>%
  summarise(auc = as.numeric(auc(outcome, pred_xgb)),
            ci_low = as.numeric(ci.auc(outcome, pred_xgb)[1]),
            ci_high = as.numeric(ci.auc(outcome, pred_xgb)[3])) %>%
  mutate(se_auc = (ci_high - auc)/1.96) %>%
  mutate(epidemic_year = as.character(epidemic_year)) %>%
  mutate(model = "XGboost")

# meta-analyse AUC
meta_analysis_x <- rma(auc,
                       sei = se_auc,
                       data = yearly_auc_x)

rma_x <- tibble(
  auc = meta_analysis_x$b %>% as.numeric,
  ci_low = meta_analysis_x$ci.lb,
  ci_high = meta_analysis_x$ci.ub,
  epidemic_year = "Random effect\nmeta-analysis")


indiv_auc_x <- bind_rows(yearly_auc_x, rma_x, overall_auc_x) %>%
  mutate(model = "XGboost")

auc_comparison <- bind_rows(indiv_auc, indiv_auc_x) %>%
  mutate(epidemic_year = fct_relevel(epidemic_year,
                                     "2018", "2019", "2020", "Random effect\nmeta-analysis", "All test\ndata"))




auc_plot <- auc_comparison %>%
  ggplot(., aes(
    x = epidemic_year,
    y = auc,
    ymin = ci_low,
    ymax = ci_high,
    group = model,
    col = model)) + 
  
  geom_point(position = position_dodge(width = 0.2)) + 
  geom_errorbar(width = 0.2,
                position = position_dodge(width = 0.2)) + 
  geom_vline(xintercept = 3.5,
             linetype = 1,
             col = "grey") +
  scale_color_manual(values = c("grey", "navy")) + 
  labs(y = "C-statistic",
       x = NULL) +
  ggtitle("Discrimination") + 
  labs(col = NULL) +
  theme_bw(base_size = 14) + 
  theme(legend.position = "top",
        legend.margin = margin(c(0,0,0,0))) 

auc_plot




####### CALIBRATION


calib <- test_set %>%
  select(epidemic_year, outcome, pred) %>%
  mutate(pred_bin = ntile(pred, 10)) %>%
  group_by(epidemic_year, pred_bin) %>%
  summarise(mean_pred = mean(pred, na.rm = T),
            mean_obs = mean(outcome, na.rm = T)) %>%
  mutate(even_year = as.factor(ifelse(epidemic_year %% 2 == 0, "even", "odd"))) %>%
  na.omit() %>%
  mutate(model = "Clinical prediction model")

calib_x <- xgboost_res %>%
  select(epidemic_year, outcome, pred_xgb) %>%
  mutate(pred_bin = ntile(pred_xgb, 10)) %>%
  group_by(epidemic_year, pred_bin) %>%
  summarise(mean_pred = mean(pred_xgb, na.rm = T),
            mean_obs = mean(outcome, na.rm = T)) %>%
  mutate(even_year = as.factor(ifelse(epidemic_year %% 2 == 0, "even", "odd"))) %>%
  na.omit() %>%
  mutate(model = "XGboost")

# 
# all_calib <- test_set %>%
#   select(outcome, pred) %>%
#   mutate(pred_bin = ntile(pred, 10)) %>%
#   group_by(pred_bin) %>%
#   summarise(mean_pred = mean(pred, na.rm = T),
#             mean_obs = mean(outcome, na.rm = T)) %>% na.omit()
# 
# all_calib_x <- xgboost_res %>%
#   select(outcome, pred) %>%
#   mutate(pred_bin = ntile(pred, 10)) %>%
#   group_by(pred_bin) %>%
#   summarise(mean_pred = mean(pred, na.rm = T),
#             mean_obs = mean(outcome, na.rm = T)) %>% na.omit()

# 
# calib_plot <- calib %>%
#   ggplot(., aes(
#     x = mean_pred,
#     y = mean_obs,
#     group = factor(epidemic_year),
#     col = factor(even_year),
#     linetype = factor(even_year)
#   )) + 
#   geom_point() +
#   stat_smooth(span = 1,
#               se = FALSE,
#               size = 0.4, 
#               inherit.aes = TRUE) + 
#   geom_abline(slope = 1, intercept = 0) + 
#   labs(x = "Predicted probability",
#        y = "Observed outcome rate",
#        col = "Even/odd\nyear") + 
#   scale_color_manual(values = c("darkgrey", "black")) +
#   scale_linetype_manual(values = c(1, 2)) + 
#   guides(linetype=FALSE) + 
#   theme_bw(base_size = 18)
# 
# calib_plot
# 
# overall_calib_plot <- all_calib %>%
#   ggplot(., aes(
#     x = mean_pred,
#     y = mean_obs)) +
#   geom_point() +
#   stat_smooth(se = FALSE,
#               col = "black",
#               size = 0.4) +
#   geom_abline(slope = 1, intercept = 0)


# calib_plot
# auc_plot


#### XGBOOST calib plot
calib_together <- bind_rows(calib, calib_x)

labs <- calib_together %>%
  filter(pred_bin == 10) %>%
  select(epidemic_year, mean_pred, mean_obs, model) %>%
  rename(label = epidemic_year,
         x = mean_pred,
         y = mean_obs) %>%
  mutate(epidemic_year = label) 

labs$y[2] <- 0.063
labs$x[2] <- 0.048

calib_plot_x <- calib_together %>%
  ggplot(., aes(
    x = mean_pred,
    y = mean_obs,
    group = factor(epidemic_year)
  )) + 
  geom_point() +
  stat_smooth(span = 1,
              se = FALSE,
              size = 0.4, 
              inherit.aes = TRUE,
              col = "darkgrey") + 
  geom_abline(slope = 1, intercept = 0, linetype = 2) + 
  labs(x = "Predicted probability",
       y = "Observed outcome rate",
       col = "Even/odd\nyear") + 
  
  scale_y_continuous(expand = c(0,0),
                     limits = c(-0.001, 0.11)) + 
  scale_x_continuous(expand = c(0,0),
                     limits = c(-0.001, 0.11)) +
  scale_linetype_manual(values = c(1, 2)) + 
  guides(linetype=FALSE) + 
  facet_wrap(~model) + 
  theme(legend.position = "top") + 
  
  geom_text(
    data = labs,
    mapping = aes(x = x, y = y, label = label),
    hjust = -0.3,
    vjust = 0.2,
    color = "black") + 
  
  theme_bw(base_size = 14)

calib_plot_x



#### CALIB SLOPE AND INTERCEPT





prob.to.logit <- function(prob){
  
  odds = prob/(1-prob)
  logit = log(odds)
  
  return(logit)
}

test_set_x$logit <- predict(model_fit, newdata = test_set_x)

logit_data <- test_set_x %>%
  mutate(logit2 = prob.to.logit(pred),
         logit_x = prob.to.logit(pred_xgb)) %>%
  select(TNRO, outcome, epidemic_year, logit, logit2, logit_x)


#### OVERALL

# calibration slope
slope_fit <- glm(data = logit_data,
                 formula = outcome ~ logit,
                 family = binomial())

slope <- coef(summary(slope_fit))[2,1]
se_slope <- coef(summary(slope_fit))[2,2]

overall_slope <- tibble(
  slope = slope,
  ci_high = slope + 1.96*se_slope,
  ci_low = slope - 1.96*se_slope,
  epidemic_year = "Pooled data\nfrom 2018-2020")



# offset model for calibration in the large
offset_fit <- glm(data = logit_data,
                  formula = outcome ~ offset(1*logit),
                  family = binomial())

cil <- coef(summary(offset_fit))[1]
se_cil <- coef(summary(offset_fit))[2]

overall_cil <- tibble(
  cil = cil,
  ci_high = cil + 1.96*se_cil,
  ci_low = cil - 1.96*se_cil,
  epidemic_year = "Pooled data\nfrom 2018-2020")


## SAME FOR XGBOOST
# calibration slope
slope_fit_x <- glm(data = logit_data,
                   formula = outcome ~ logit_x,
                   family = binomial())

slope_x <- coef(summary(slope_fit_x))[2,1]
se_slope_x <- coef(summary(slope_fit_x))[2,2]

overall_slope_x <- tibble(
  slope = slope_x,
  ci_high = slope_x + 1.96*se_slope_x,
  ci_low = slope_x - 1.96*se_slope_x,
  epidemic_year = "Pooled data\nfrom 2018-2020")


# offset model for calibration in the large
offset_fit_x <- glm(data = logit_data,
                    formula = outcome ~ offset(1*logit_x),
                    family = binomial())

cil_x <- coef(summary(offset_fit_x))[1]
se_cil_x <- coef(summary(offset_fit_x))[2]

overall_cil_x <- tibble(
  cil = cil_x,
  ci_high = cil_x + 1.96*se_cil_x,
  ci_low = cil_x - 1.96*se_cil_x,
  epidemic_year = "Pooled data\nfrom 2018-2020")



overall_cil$model <- "Clinical prediction model" 
overall_slope$model <- "Clinical prediction model"
overall_cil_x$model <- "XGboost"
overall_slope_x$model <- "XGboost"





### YEARLY
yearly_intercept <- NULL
yearly_slope <- NULL
yearly_intercept_x <- NULL
yearly_slope_x <- NULL
# the actual loop
for(i in 2018:2020) {
  
  data_for_year <- logit_data %>%
    filter(epidemic_year == i)
  
  
  # calibration slope
  slope_fit <- glm(data = data_for_year,
                   formula = outcome ~ logit,
                   family = binomial())
  
  slope <- coef(summary(slope_fit))[2,1]
  se_slope <- coef(summary(slope_fit))[2,2]
  
  slope_df <- tibble(
    slope = slope,
    ci_high = slope + 1.96*se_slope,
    ci_low = slope - 1.96*se_slope,
    epidemic_year = i)
  
  # bind results to the existing results data  
  yearly_slope <- bind_rows(yearly_slope, slope_df)
  
  # offset model for calibration in the large
  offset_fit <- glm(data = data_for_year,
                    formula = outcome ~ offset(1*logit),
                    family = binomial())
  
  cil <- coef(summary(offset_fit))[1]
  se_cil <- coef(summary(offset_fit))[2]
  
  cil_df <- tibble(
    cil = cil,
    ci_high = cil + 1.96*se_cil,
    ci_low = cil - 1.96*se_cil,
    epidemic_year = i)
  
  # bind the results to the existing results data
  yearly_intercept <- bind_rows(yearly_intercept, cil_df)
  
  
  #### SAME FOR XGBOOST
  # calibration slope
  slope_fit_x <- glm(data = data_for_year,
                     formula = outcome ~ logit_x,
                     family = binomial())
  
  slope_x <- coef(summary(slope_fit_x))[2,1]
  se_slope_x <- coef(summary(slope_fit_x))[2,2]
  
  slope_df_x <- tibble(
    slope = slope_x,
    ci_high = slope_x + 1.96*se_slope_x,
    ci_low = slope_x - 1.96*se_slope_x,
    epidemic_year = i)
  
  # bind results to the existing results data  
  yearly_slope_x <- bind_rows(yearly_slope_x, slope_df_x)
  
  # offset model for calibration in the large
  offset_fit_x <- glm(data = data_for_year,
                      formula = outcome ~ offset(1*logit_x),
                      family = binomial())
  
  cil_x <- coef(summary(offset_fit_x))[1]
  se_cil_x <- coef(summary(offset_fit_x))[2]
  
  cil_df_x <- tibble(
    cil = cil_x,
    ci_high = cil_x + 1.96*se_cil_x,
    ci_low = cil_x - 1.96*se_cil_x,
    epidemic_year = i)
  
  # bind the results to the existing results data
  yearly_intercept_x <- bind_rows(yearly_intercept_x, cil_df_x)
  
  
}

# meta <- yearly_intercept %>%
#   mutate(se = (ci_high - cil)/1.96) %>%
#   rma(data = ., yi = cil, sei = se)




meta.analyse <- function(df, metric) {
  
  df <- df %>%
    mutate(epidemic_year = as.character(epidemic_year))
  
  df$se <- (df[["ci_high"]] - df[[metric]])/1.96
  
  value <- df[[metric]]
  se <- df[["se"]]
  
  meta <- rma(value, sei = se)
  
  meta_df <- tibble(
    ci_low = meta$ci.lb,
    ci_high = meta$ci.ub,
    epidemic_year = "Random effect\nmeta-analysis")
  
  meta_df[[metric]] <- meta$b %>% as.numeric
  
  df_2 <- bind_rows(df, meta_df)
  
  return(df_2)
  
}

yearly_intercept <- meta.analyse(df = yearly_intercept, metric = "cil")
yearly_intercept$model <- "Clinical prediction model"

yearly_slope <- meta.analyse(df = yearly_slope, metric = "slope")
yearly_slope$model <- "Clinical prediction model"

yearly_intercept_x <- meta.analyse(df = yearly_intercept_x, metric = "cil")
yearly_intercept_x$model <- "XGboost"

yearly_slope_x <- meta.analyse(df = yearly_slope_x, metric = "slope")
yearly_slope_x$model <- "XGboost"


compare_intercept <- bind_rows(yearly_intercept, 
                               yearly_intercept_x, 
                               overall_cil, 
                               overall_cil_x) %>%
  mutate(epidemic_year = fct_relevel(epidemic_year, 
                                     "2018", "2019", "2020", 
                                     "Random effect\nmeta-analysis",
                                     "Pooled data\nfrom 2018-2020"))
compare_slope <- bind_rows(yearly_slope, 
                           yearly_slope_x, 
                           overall_slope, 
                           overall_slope_x) %>%
  mutate(epidemic_year = fct_relevel(epidemic_year, 
                                     "2018", "2019", "2020", 
                                     "Random effect\nmeta-analysis",
                                     "Pooled data\nfrom 2018-2020"))

pos <- position_dodge(width = 0.2)
intercept_plot <- compare_intercept %>%
  ggplot(aes(x = epidemic_year,
             y = cil, 
             ymin = ci_low,
             ymax = ci_high,
             col = model)) + 
  geom_point(position = pos) + 
  geom_errorbar(width = 0.2,
                position = pos) + 
  labs(title = "Calibration-in-the-large", 
       y = "Intercept",
       x = NULL) + 
  geom_hline(yintercept = 0, linetype = 2, size = 0.75) + 
  geom_vline(xintercept = 3.5, col = "grey") + 
  theme_bw(base_size = 14) + 
  scale_color_manual(values = c("grey", "navy")) + 
  guides(color = FALSE)

intercept_plot

slope_plot <- compare_slope %>%
  ggplot(aes(x = epidemic_year,
             y = slope, 
             ymin = ci_low,
             ymax = ci_high,
             col = model)) + 
  geom_point(position = pos) + 
  geom_errorbar(width = 0.2,
                position = pos) + 
  labs(title = "Calibration slope",
       x = NULL) + 
  geom_hline(yintercept = 1, linetype = 2, size = 0.75) + 
  geom_vline(xintercept = 3.5, col = "grey") + 
  theme_bw(base_size = 14)+ 
  scale_color_manual(values = c("grey", "navy")) + 
  guides(color = FALSE)

slope_plot


my_plotlist <- list(auc_plot, 
                    intercept_plot, 
                    slope_plot, 
                    calib_plot_x)

my_layout <- cbind(c(1,1,2,2,3,3,4,4,4,4))

grid.arrange(grobs = my_plotlist, layout_matrix = my_layout)

g <- arrangeGrob(grobs = my_plotlist, layout_matrix = my_layout)

ggsave(plot = g,
       filename = "suppl_xgboost.pdf",
       device = "pdf",
       width = 9,
       height = 12,
       dpi = 1200,
       path = "/data/projects/project_pvartiai/rsv/results/plots/")


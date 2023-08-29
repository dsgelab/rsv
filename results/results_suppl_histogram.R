### SUPPL RESULTS HISTOGRAM

#### LOAD

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

#### LOAD FINNISH VALIDATION DATA
# load datasets: predict_data and all_bds

setwd("/data/projects/project_pvartiai/rsv/modeling")
predict_datapath <- "/data/projects/project_pvartiai/rsv/modeling/"
comp_datapath <- "/data/projects/project_pvartiai/rsv/predictors/"
val_path <- "/data/projects/project_pvartiai/rsv/validation_data/"
swed_path <- "/data/projects/project_pvartiai/rsv/results/sweden/"

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
  as_tibble()

# predict
test_set$pred <- predict(model_fit, newdata = test_set, type = "response")

train_data <- recent_data

train_data$pred <-  predict(model_fit, newdata = train_data, type = "response")


all_predictions <- bind_rows(train_data, test_set) %>%
  select(epidemic_year, pred) %>%
  mutate(test_data = as.factor(ifelse(epidemic_year %in% 2018:2020, 1, 0)))



fin_density <- all_predictions %>%
  mutate(epidemic_year = as.factor(epidemic_year)) %>%
  ggplot(aes(x = pred,
             col = epidemic_year,
             group = epidemic_year)) + 
  geom_density() + 
  scale_x_log10(breaks = c(0.002, 0.005, 0.01, 0.02, 0.05, 0.1),
                expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0, 1.1)) + 
  theme_bw(base_size = 14) + 
  labs(col = "RSV epidemic year",
       x = "Predicted probability of RSV-hospitalisation",
       y = "Density")



fin_density

ggsave(plot = fin_density,
       filename = "suppl_density.pdf",
       device = "pdf",
       width = 9,
       height = 9,
       dpi = 1200,
       path = "/data/projects/project_pvartiai/rsv/results/plots/")



### percentile cutoffs

all_predictions$pred %>%
  quantile(., probs = c(seq(0, 1, 0.01)), na.rm = T) %>%
  as.data.frame()

all_predictions %>%
  mutate(perc = ntile(pred, 100)) %>%
  group_by(perc) %>%
  summarise(max = max(pred)) %>%
  View()



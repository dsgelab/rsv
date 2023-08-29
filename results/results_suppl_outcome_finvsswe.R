### Outcomes: sweden vs finland


library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(forcats)
library(data.table)
library(feather)
library(ggplot2)
library(gridExtra)


# load outcome dates
outcome_dates <- fread("/data/projects/project_pvartiai/rsv/wrangle/preprocess_3_mbr_hilmo_outcomes.csv",
                       select = c("TNRO", "LAPSEN_SYNTYMAPVM", "outcome", "TUPVA", "age_at_outcome")) %>%
  as_tibble 


# Swedish outcomes per month
swed_path <- "/data/projects/project_pvartiai/rsv/results/sweden/"
swedish_outcomes <- get(load(paste0(swed_path, "monthly_outcomes_sweden.R"))) %>%
  ungroup() %>%
  mutate(outcome_date = paste0(outcome_year, "-",outcome_month, "-1")) %>%
  mutate(outcome_date = as.Date(outcome_date)) %>%
  select(outcome_date, outcome_rate_sweden) %>%
  mutate(Country = "Sweden") %>%
  rename(outcome_number = outcome_rate_sweden) %>%
  filter(outcome_date < "2021-01-01")




hosp <- outcome_dates %>%
  filter(outcome == 1) %>%
  select(TNRO, TUPVA, outcome) %>%
  rename(outcome_date = TUPVA) %>%
  mutate(month = floor_date(outcome_date, 'month')) %>%
  group_by(month) %>%
  summarise(n = n()) %>%
  filter(month >= as.Date("2006-09-01")) %>%
  mutate(Country = "Finland") %>%
  rename(outcome_number = n,
         outcome_date = month)

compare_outcomes <- bind_rows(swedish_outcomes, hosp) %>%
  mutate(facet = ifelse(outcome_date >= "2013-08-01", 1, 0))


outc_comp <- compare_outcomes %>%
  ggplot(aes(x = outcome_date,
             group = Country,
             y = outcome_number,
             col = Country)) + 
  geom_line(size = 1) + 
  scale_color_manual(values =c("#9fbfd4", "#D6C533")) + 
  theme_bw(base_size = 14) + 
  scale_x_date(breaks = pretty_breaks(n = 10),
               expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0), 
                     limits = c(-3, 800)) +
  facet_wrap(~ facet, 
             nrow = 2,
             scales = "free_x") + 
  theme(axis.text = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.position = "top",
        strip.text.x = element_blank()) + 
  labs(y = "Number of RSV-hospitalisation cases per month",
       x = "Calendar year")


outc_comp

ggsave(plot = outc_comp,
       filename = "outcome_comparison_finswe.pdf",
       device = "pdf",
       width = 9,
       height = 9,
       dpi = 1200,
       path = "/data/projects/project_pvartiai/rsv/results/plots/")

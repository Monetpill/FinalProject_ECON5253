
# Packages
library(tidyverse)
library(fixest)
library(modelsummary)


# Load Data 

qcew_cps_panel <- readRDS("qcew_cps_panel.rds")
cps_clean <- readRDS("cps_clean.rds")

# Build Immigrant Employment Share (Robustness Check 1)
immigrant_share_outcome <- cps_clean %>%
  filter(target_industry == 1) %>%
  group_by(STATEFIP, YEAR) %>%
  summarise(
    imm_emp_share = weighted.mean(immigrant * employed, ASECWT, na.rm = TRUE) /
      weighted.mean(employed, ASECWT, na.rm = TRUE),
    .groups = "drop"
  )

# Merge immigrant share onto panel
qcew_cps_panel <- qcew_cps_panel %>%
  left_join(immigrant_share_outcome, by = c("STATEFIP", "year" = "YEAR")) %>%
  mutate(
    est_immigrant_employment = total_employment * imm_emp_share,
    log_immigrant_employment = log(est_immigrant_employment)
  )

# Flag Sanctuary States (Robustness Check 2)
# States identified using Center for Immigration Studies sanctuary map
# (cis.org/Map-Sanctuary-Cities-Counties-and-States, accessed April 2026)
sanctuary_states <- c(6, 8, 9, 17, 25, 34, 35, 36, 41, 44, 49, 50, 53)
# CA, CO, CT, IL, MA, NJ, NM, NY, OR, RI, UT, VT, WA

# ---------- MODELS ----------

# Main DiD Event Study Model
# Outcome: log total employment in target industries
# Treatment: above median pre-treatment immigrant labor share
# Fixed effects: MSA and year | Clustered SE: state level
qcew_model2 <- feols(log(total_employment) ~ 
                       i(year, high_immigrant, ref = 2016) | 
                       cbsa_code + year,
                     data = qcew_cps_panel,
                     cluster = ~STATEFIP)

summary(qcew_model2)

# Robustness Check 1: Immigrant Employment as Outcome 
# Tests whether changes in total employment specifically affect immigrants
# or reflect broader macroeconomic trends
qcew_model3 <- feols(log_immigrant_employment ~
                       i(year, high_immigrant, ref = 2016) |
                       cbsa_code + year,
                     data = qcew_cps_panel %>% 
                       filter(is.finite(log_immigrant_employment)),
                     cluster = ~STATEFIP)

summary(qcew_model3)


# Robustness Check 2: Excluding Sanctuary States
# Sanctuary states may have faced lower enforcement intensity
# which could confound the main results
qcew_model_nosanctuary <- feols(
  log(total_employment) ~
    i(year, high_immigrant, ref = 2016) |
    cbsa_code + year,
  data = qcew_cps_panel %>% filter(!(STATEFIP %in% sanctuary_states)),
  cluster = ~STATEFIP
)

summary(qcew_model_nosanctuary)


# Combined Regression Table
modelsummary(
  list("Main Model" = qcew_model2,
       "Immigrant Employment" = qcew_model3,
       "No Sanctuary States" = qcew_model_nosanctuary),
  stars = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
  coef_rename = c(
    "year::2014:high_immigrant" = "2014 x High Immigrant",
    "year::2015:high_immigrant" = "2015 x High Immigrant",
    "year::2017:high_immigrant" = "2017 x High Immigrant",
    "year::2018:high_immigrant" = "2018 x High Immigrant",
    "year::2019:high_immigrant" = "2019 x High Immigrant",
    "year::2020:high_immigrant" = "2020 x High Immigrant"
  ),
  gof_map = c("nobs", "adj.r.squared"),
  notes = "Standard errors clustered at the state level. Reference year is 2016.",
)

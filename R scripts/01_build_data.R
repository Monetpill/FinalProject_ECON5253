
# Packages Required

library(tidyverse)
library(readxl)
library(janitor)
library(fixest)
library(modelsummary)


# CPS ASEC microdata must be downloaded manually from IPUMS CPS
# (https://cps.ipums.org) — free account required.
# Build an extract with the following:
#   Samples: ASEC 2014–2020
#   Variables: YEAR, STATEFIP, ASECFLAG, ASECWT, AGE, SEX,
#              EMPSTAT, NATIVITY, IND, INCTOT
# Download as .csv.gz, rename to cps_00004.csv.gz,
# and place in the working directory of this file before running this script.

cps_raw <- read_csv("cps_00004.csv.gz")

# Clean CPS and identify target industries
  # Industry codes follow the 2017 Census Classification Scheme
  cps_clean <- cps_raw %>%
  filter(
    AGE >= 16, AGE <= 64,
    ASECFLAG == 1,
    EMPSTAT != 0
  ) %>%
  mutate(
    employed = as.numeric(EMPSTAT == 10 | EMPSTAT == 12),
    immigrant = as.numeric(NATIVITY != 1),
    post = as.numeric(YEAR >= 2017),
    target_industry = case_when(
      IND %in% c(170,180,190,270,280,290) ~ 1,        # agriculture
      IND == 770 ~ 1,                                   # construction
      IND %in% c(1070,1080,1090,1170,1180,1190,
                 1270,1280,1290) ~ 1,                   # food manufacturing
      IND %in% c(8680,8690) ~ 1,                       # food services
      TRUE ~ 0
    )
  )
  
# Pre-treatment immigrant share by state (2014-2016)
# States above the median are classified as high immigrant
  pre_treatment <- cps_clean %>%
    filter(target_industry == 1, YEAR < 2017) %>%
    group_by(STATEFIP) %>%
    summarise(
      immigrant_share = weighted.mean(immigrant, ASECWT, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(high_immigrant = as.numeric(immigrant_share > median(immigrant_share)))
  
  summary(pre_treatment$immigrant_share)
  pre_treatment %>% count(high_immigrant)
  
  
# Download Census MSA Crosswalk
  temp <- tempfile(fileext = ".xls")
  download.file("https://www2.census.gov/programs-surveys/metro-micro/geographies/reference-files/2020/delineation-files/list1_2020.xls", 
                temp, mode = "wb")
  
  crosswalk <- read_xls(temp, skip = 2) %>%
    clean_names() %>%
    filter(!is.na(fips_state_code), !is.na(cbsa_code)) %>%
    mutate(area_fips = str_pad(paste0(fips_state_code, fips_county_code), 5, pad = "0")) %>%
    select(area_fips, cbsa_code, cbsa_title) %>%
    distinct()
  
  county_fips <- crosswalk %>% pull(area_fips) %>% unique()
  
  
# Pull QCEW Data via BLS API
# NOTE: This takes approximately 1.5 hours to run
  target_industries <- c("722", "23", "11", "311")
  
  pull_county <- function(fips, year) {
    url <- paste0("https://data.bls.gov/cew/data/api/", year, "/a/area/", fips, ".csv")
    tryCatch({
      read_csv(url, show_col_types = FALSE) %>%
        filter(industry_code %in% target_industries,
               own_code == 5) %>%
        select(area_fips, industry_code, annual_avg_emplvl, avg_annual_pay) %>%
        mutate(
          area_fips = as.character(area_fips),
          industry_code = as.character(industry_code),
          year = year
        )
    }, error = function(e) NULL)
  }
  
  for(yr in 2014:2020) {
    message("Pulling year: ", yr)
    year_data <- map_dfr(county_fips, pull_county, year = yr)
    saveRDS(year_data, paste0("qcew_", yr, ".rds"))
    message("Saved year: ", yr, " — rows: ", nrow(year_data))
  }
  
  qcew_raw <- map_dfr(2014:2020, function(yr) {
    readRDS(paste0("qcew_", yr, ".rds"))
  })
  
  saveRDS(qcew_raw, "qcew_raw.rds")
  

# Aggregate QCEW county data to MSA level
  crosswalk_clean <- crosswalk %>%
    filter(!is.na(area_fips), !is.na(cbsa_code)) %>%
    mutate(area_fips = str_pad(as.character(area_fips), 5, pad = "0")) %>%
    select(area_fips, cbsa_code, cbsa_title) %>%
    distinct()
  
  msa_panel <- qcew_raw %>%
    left_join(crosswalk_clean, by = "area_fips") %>%
    filter(!is.na(cbsa_code)) %>%
    group_by(cbsa_code, cbsa_title, industry_code, year) %>%
    summarise(
      employment = sum(annual_avg_emplvl, na.rm = TRUE),
      avg_pay = mean(avg_annual_pay, na.rm = TRUE),
      .groups = "drop"
    )
  
  msa_total <- msa_panel %>%
    group_by(cbsa_code, cbsa_title, year) %>%
    summarise(
      total_employment = sum(employment, na.rm = TRUE),
      avg_pay = mean(avg_pay, na.rm = TRUE),
      .groups = "drop"
    )
  
  crosswalk_with_state <- crosswalk %>%
    mutate(STATEFIP = as.numeric(str_sub(area_fips, 1, 2))) %>%
    select(cbsa_code, cbsa_title, STATEFIP) %>%
    distinct()
   
# Aggregate QCEW to MSA-year level and merge with CPS pre-treatment data
  
  qcew_cps_panel <- msa_total %>%
    left_join(crosswalk_with_state %>%
                select(cbsa_code, STATEFIP) %>%
                distinct(cbsa_code, .keep_all = TRUE),
              by = "cbsa_code") %>%
    left_join(pre_treatment %>%
                select(STATEFIP, immigrant_share, high_immigrant),
              by = "STATEFIP") %>%
    mutate(
      post = as.numeric(year >= 2017),
      log_employment = log(total_employment)
    ) %>%
    filter(!is.na(high_immigrant), is.finite(log_employment))
  
  saveRDS(qcew_cps_panel, "qcew_cps_panel.rds")
  

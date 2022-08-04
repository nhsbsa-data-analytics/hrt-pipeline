# pipeline.R --------------------------------------------------------------
# This script is used to run the RAP for the HRT publication

# 1. install required packages --------------------------------------------
req_pkgs <-
  c(
    "dplyr",
    "stringr",
    "data.table",
    "yaml",
    "openxlsx",
    "rmarkdown",
    "highcharter",
    "lubridate",
    "dbplyr",
    "tidyr",
    "readxl"
  )

# utils::install.packages(req_pkgs, dependencies = TRUE)
# 
# devtools::install_github("nhsbsa-data-analytics/hrtR",
#                          auth_token = Sys.getenv("GITHUB_PAT"))
# 
# devtools::install_github("nhsbsa-data-analytics/nhsbsaR")

invisible(lapply(c(req_pkgs,  "nhsbsaR", "hrtR"), library, character.only = TRUE))

# 2. set options ----------------------------------------------------------
hrt_options()

# 3. build fact table if new data available -------------------
con <- con_nhsbsa(
  dsn = "FBS_8192k",
  driver = "Oracle in OraClient19Home1",
  "DWCP",
  username = rstudioapi::showPrompt(title = "Username", message = "Username"),
  password = rstudioapi::askForPassword()
)

#only run if need to build new fact table``
#hrtR::create_fact(con)

#get max month and full fy
# bring in DIM.YEAR_MONTH_DIM
ym_dim <- dplyr::tbl(con,
                     from = dbplyr::in_schema("DIM", "YEAR_MONTH_DIM")) %>%
  # shrink table to remove unnecessary data
  dplyr::filter(
    YEAR_MONTH >= 201501L,
    YEAR_MONTH <= dplyr::sql(
      "MGMT.PKG_PUBLIC_DWH_FUNCTIONS.f_get_latest_period('EPACT2')"
    )
  ) %>%
  dplyr::select(
    YEAR_MONTH,
    FINANCIAL_YEAR
  )  %>%
  # add month counts for financial quarters and financial years to latest
  # complete periods
  dplyr::mutate(
    FY_COUNT = dbplyr::win_over(
      expr = dplyr::sql("count(distinct YEAR_MONTH)"),
      partition = "FINANCIAL_YEAR",
      con = con
    )
  )

# extract latest available month of data
ltst_month <- ym_dim %>%
  dplyr::filter(YEAR_MONTH == max(YEAR_MONTH, na.rm = TRUE)) %>%
  dplyr::pull(YEAR_MONTH)

# use ltst_month to automate tidy date
ltst_month_tidy <- as.character(paste0(ltst_month,"01"))
ltst_month_tidy <- base::as.Date(ltst_month_tidy, format = "%Y%m%d")
ltst_month_tidy <- format(ltst_month_tidy, "%B %Y")

#calculate date 24 months back for filtering charts
lt_st_month_min <- as.POSIXlt(base::as.Date(as.character(paste0(ltst_month,"01")), format = "%Y%m%d"))
lt_st_month_min$mon <- lt_st_month_min$mon - 23
lt_st_month_min <- as.numeric(format(lt_st_month_min, "%Y%m"))

# extract latest available full financial year
ltst_year <- ym_dim %>%
  dplyr::filter(FY_COUNT == 12) %>%
  dplyr::select(FINANCIAL_YEAR) %>%
  dplyr::filter(
    FINANCIAL_YEAR == max(FINANCIAL_YEAR, na.rm = TRUE)
  ) %>%
  dplyr::distinct() %>%
  dplyr::pull(FINANCIAL_YEAR)

imd_population <- function(){
  temp <- tempfile()
  imd_url <- utils::download.file(url = "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/populationandmigration/populationestimates/adhocs/13773populationsbyindexofmultipledeprivationimddecileenglandandwales2020/populationbyimdenglandandwales2020.xlsx",
                                  temp,
                                  mode = "wb")
  #read xlsx population file
  df <- readxl::read_xlsx(temp,
                          sheet = 2,
                          range = "A4:CO25",
                          col_names = TRUE)
  #remove blank row, rename columns, recode imd decile into imd quintile
  imd_pop_2020 <- df %>%
    filter_all(any_vars(!is.na(.))) %>%
    dplyr::rename(GENDER = ...1,
                  IMD_DECILE = `Deprivation Decile (IMD 2020)`) %>%
    tidyr::fill(GENDER) %>%
    dplyr::mutate(IMD_QUINTILE = factor(case_when(
      IMD_DECILE %in% c(1,2) ~ "1",
      IMD_DECILE %in% c(3,4) ~ "2",
      IMD_DECILE %in% c(5,6) ~ "3",
      IMD_DECILE %in% c(7,8) ~ "4",
      IMD_DECILE %in% c(9,10) ~ "5"
    ))) %>%
    tidyr::pivot_longer(cols = `0`:`90+`,
                        names_to = "AGE",
                        values_to = "POPULATION") %>%
    mutate(
      AGE = case_when(
        AGE == "90+" ~ as.numeric(90),
        TRUE ~ as.numeric(AGE)
      )
    ) %>%
    dplyr::mutate(AGE_BAND = case_when(
      AGE == 90 ~ "90+",
      AGE >= 85 ~ "85-89",
      AGE >= 80 ~ "80-84",
      AGE >= 75 ~ "75-79",
      AGE >= 70 ~ "70-74",
      AGE >= 65 ~ "65-69",
      AGE >= 60 ~ "60-64",
      AGE >= 55 ~ "55-59",
      AGE >= 50 ~ "50-54",
      AGE >= 45 ~ "45-49",
      AGE >= 40 ~ "40-44",
      AGE >= 35 ~ "35-39",
      AGE >= 30 ~ "30-34",
      AGE >= 25 ~ "25-29",
      AGE >= 20 ~ "20-24",
      AGE >= 15 ~ "15-19",
      AGE >= 10 ~ "10-14",
      AGE >= 5 ~ "05-09",
      TRUE ~ "00-04"),
      IMD_QUINTILE = as.numeric(IMD_QUINTILE)) %>%
    dplyr::group_by(GENDER, IMD_QUINTILE, AGE_BAND) %>%
    dplyr::summarise(POPULATION = sum(POPULATION), .groups = "drop") %>%
    mutate(
      GENDER = case_when(
        GENDER == "Males" ~ "M",
        TRUE ~ "F"
      )
    )
  return(imd_pop_2020)
}


imd_population_age_gender <- imd_population()
imd_population_age <- imd_population_age_gender %>%
  group_by(IMD_QUINTILE,
           AGE_BAND) %>%
  summarise(POPULATION = sum(POPULATION, na.rm = T))
imd_population <- imd_population_age_gender %>%
  group_by(IMD_QUINTILE) %>%
  summarise(POPULATION = sum(POPULATION, na.rm = T))

# 4. extract data tables from fact table -----------------------------------------
raw_data <- list()

#patient identification rates
#TODO: build as function in package
pi_extract_annual <- function(
  con,
  table = "HRT_FACT_DIM"
) {
fact <- dplyr::tbl(con,
                      from = table) %>% 
  filter(
    FINANCIAL_YEAR <= ltst_year
  ) %>%
  dplyr::group_by(FINANCIAL_YEAR,
                  `BNF paragraph name` = PARAGRAPH_NAME,
                  `BNF paragraph code` = PARAGRAPH_CODE,
                  PATIENT_IDENTIFIED) %>% 
  dplyr::summarise(ITEM_COUNT = sum(ITEM_COUNT, na.rm = T),
                   .groups = "drop") %>%
  dplyr::arrange(FINANCIAL_YEAR) %>%
  collect() %>%
  tidyr::pivot_wider(names_from = PATIENT_IDENTIFIED,
                     values_from = ITEM_COUNT) %>% 
  mutate(RATE = Y/(Y+N) * 100,
         `BNF paragraph code` = factor(`BNF paragraph code`, 
                                     levels = c("060401","070201"))) %>%
  dplyr::select(-Y, -N) %>% 
  tidyr::pivot_wider(names_from = FINANCIAL_YEAR,
                     values_from = RATE) %>% 
  dplyr:: arrange(`BNF paragraph code`) 

return(fact)
}

raw_data$pi_table_annual <- pi_extract_annual(con)

pi_extract_annual_excel <- function(
    con,
    table = "HRT_FACT_DIM"
) {
  fact <- dplyr::tbl(con,
                     from = table) %>% 
    filter(
      FINANCIAL_YEAR <= ltst_year
    ) %>%
    dplyr::group_by(FINANCIAL_YEAR,
                    PATIENT_IDENTIFIED) %>% 
    dplyr::summarise(ITEM_COUNT = sum(ITEM_COUNT, na.rm = T),
                     .groups = "drop") %>%
    dplyr::arrange(FINANCIAL_YEAR) %>%
    collect() %>%
    tidyr::pivot_wider(names_from = PATIENT_IDENTIFIED,
                       values_from = ITEM_COUNT) %>% 
    mutate(RATE = Y/(Y+N) * 100) %>%
    dplyr::select(-Y, -N) 
  
  return(fact)
}

raw_data$pi_excel_annual <- pi_extract_annual_excel(con)

pi_extract_monthly_excel <- function(
    con,
    table = "HRT_FACT_DIM"
) {
  fact <- dplyr::tbl(con,
                     from = table) %>% 
    filter(
      FINANCIAL_YEAR <= ltst_year
    ) %>%
    dplyr::group_by(FINANCIAL_YEAR,
                    YEAR_MONTH,
                    PATIENT_IDENTIFIED) %>% 
    dplyr::summarise(ITEM_COUNT = sum(ITEM_COUNT, na.rm = T),
                     .groups = "drop") %>%
    dplyr::arrange(FINANCIAL_YEAR,
                   YEAR_MONTH) %>%
    collect() %>%
    tidyr::pivot_wider(names_from = PATIENT_IDENTIFIED,
                       values_from = ITEM_COUNT) %>% 
    mutate(RATE = Y/(Y+N) * 100) %>%
    dplyr::select(-Y, -N) 
  
  return(fact)
}

raw_data$pi_excel_monthly <- pi_extract_monthly_excel(con)


#TODO: build as function in package
pi_extract_monthly <- function(
  con,
  table = "HRT_FACT_DIM"
) {
  fact <- dplyr::tbl(con,
                     from = table) %>% 
    dplyr::group_by(
                    YEAR_MONTH,
                    `BNF paragraph name` = PARAGRAPH_NAME,
                    `BNF paragraph code` = PARAGRAPH_CODE,
                    PATIENT_IDENTIFIED) %>% 
    dplyr::summarise(ITEM_COUNT = sum(ITEM_COUNT, na.rm = T),
                     .groups = "drop") %>%
    dplyr::arrange(YEAR_MONTH) %>%
    collect() %>%
    tidyr::pivot_wider(names_from = PATIENT_IDENTIFIED,
                       values_from = ITEM_COUNT) %>% 
    mutate(RATE = Y/(Y+N) * 100,
           `BNF paragraph code` = factor(`BNF paragraph code`, 
                                         levels = c("060401","070201"))) %>%
    dplyr::select(-Y, -N) %>% 
    dplyr:: arrange(`BNF paragraph code`) %>%
    rename(
      `Year Month` = 1,
      `Identified Patient Rate` = 4
    )
  
  return(fact)
}

raw_data$pi_excel_monthly <- pi_extract_monthly(con)

## national level data
national_extract_annual <- function(
  con,
  table = "HRT_FACT_DIM"
) {
  
  fact <- dplyr::tbl(con,
                     from = table) %>%
    dplyr::mutate(
      PATIENT_COUNT = case_when(
        PATIENT_IDENTIFIED == "Y" ~ 1,
        TRUE ~ 0
      )
    ) %>%
    dplyr::group_by(
      FINANCIAL_YEAR,
      PATIENT_ID,
      PATIENT_IDENTIFIED,
      PATIENT_COUNT
    ) %>%
    dplyr::summarise(
      ITEM_COUNT = sum(ITEM_COUNT, na.rm = T),
      ITEM_PAY_DR_NIC = sum(ITEM_PAY_DR_NIC, na.rm = T),
      .groups = "drop"
    ) 
  
  fact_national <- fact %>%
    dplyr::group_by(
      FINANCIAL_YEAR,
      PATIENT_IDENTIFIED
    ) %>%
    dplyr::summarise(
      ITEM_COUNT = sum(ITEM_COUNT, na.rm = T),
      ITEM_PAY_DR_NIC = sum(ITEM_PAY_DR_NIC, na.rm = T)/100,
      PATIENT_COUNT = sum(PATIENT_COUNT, na.rm = T),
      .groups = "drop"
    ) %>%
    dplyr::arrange(
      FINANCIAL_YEAR,
      desc(PATIENT_IDENTIFIED)
    ) %>%
    collect() %>%
    filter(
      FINANCIAL_YEAR <= ltst_year
    )
  
  return(fact_national)
  
}
raw_data$national_annual <- national_extract_annual(con)

national_extract_monthly <- function(
    con,
    table = "HRT_FACT_DIM"
) {
  
  fact <- dplyr::tbl(con,
                     from = table) %>%
    dplyr::mutate(
      PATIENT_COUNT = case_when(
        PATIENT_IDENTIFIED == "Y" ~ 1,
        TRUE ~ 0
      )
    ) %>%
    dplyr::group_by(
      FINANCIAL_YEAR,
      YEAR_MONTH,
      PATIENT_ID,
      PATIENT_IDENTIFIED,
      PATIENT_COUNT
    ) %>%
    dplyr::summarise(
      ITEM_COUNT = sum(ITEM_COUNT, na.rm = T),
      ITEM_PAY_DR_NIC = sum(ITEM_PAY_DR_NIC, na.rm = T),
      .groups = "drop"
    ) 
  
  fact_national <- fact %>%
    dplyr::group_by(
      FINANCIAL_YEAR,
      YEAR_MONTH,
      PATIENT_IDENTIFIED
    ) %>%
    dplyr::summarise(
      ITEM_COUNT = sum(ITEM_COUNT, na.rm = T),
      ITEM_PAY_DR_NIC = sum(ITEM_PAY_DR_NIC, na.rm = T)/100,
      PATIENT_COUNT = sum(PATIENT_COUNT, na.rm = T),
      .groups = "drop"
    ) %>%
    dplyr::arrange(
      FINANCIAL_YEAR,
      YEAR_MONTH,
      desc(PATIENT_IDENTIFIED)
    ) %>%
    collect()
  
  return(fact_national)
  
}
raw_data$national_monthly <- national_extract_monthly(con)

# data by bnf paragraph

paragraph_extract <- function(
    con,
    table = "HRT_FACT_DIM"
) {
  
  fact <- dplyr::tbl(con,
                     from = table) %>%
    dplyr::mutate(
      PATIENT_COUNT = case_when(
        PATIENT_IDENTIFIED == "Y" ~ 1,
        TRUE ~ 0
      )
    ) %>%
    dplyr::group_by(
      FINANCIAL_YEAR,
      PATIENT_ID,
      PATIENT_IDENTIFIED,
      SECTION_NAME,
      SECTION_CODE,
      PARAGRAPH_NAME,
      PARAGRAPH_CODE,
      PATIENT_COUNT
    ) %>%
    dplyr::summarise(
      ITEM_COUNT = sum(ITEM_COUNT, na.rm = T),
      ITEM_PAY_DR_NIC = sum(ITEM_PAY_DR_NIC, na.rm = T),
      .groups = "drop"
    ) 
  
  fact_paragraph <- fact %>%
    dplyr::group_by(
      FINANCIAL_YEAR,
      SECTION_NAME,
      SECTION_CODE,
      PARAGRAPH_NAME,
      PARAGRAPH_CODE,
      PATIENT_IDENTIFIED
    ) %>%
    dplyr::summarise(
      PATIENT_COUNT = sum(PATIENT_COUNT, na.rm = T),
      ITEM_COUNT = sum(ITEM_COUNT, na.rm = T),
      ITEM_PAY_DR_NIC = sum(ITEM_PAY_DR_NIC, na.rm = T)/100,
      .groups = "drop"
    ) %>%
    dplyr::arrange(
      FINANCIAL_YEAR,
      SECTION_CODE,
      PARAGRAPH_CODE,
      desc(PATIENT_IDENTIFIED)
    ) %>%
    collect() %>%
    filter(FINANCIAL_YEAR <= ltst_year)
  
  return(fact_paragraph)
  
}

raw_data$national_par_annual <- paragraph_extract(con)

# data by chemical substance paragraph

chem_sub_extract <- function(
    con,
    table = "HRT_FACT_DIM"
) {
  
  fact <- dplyr::tbl(con,
                     from = table) %>%
    dplyr::mutate(
      PATIENT_COUNT = case_when(
        PATIENT_IDENTIFIED == "Y" ~ 1,
        TRUE ~ 0
      )
    ) %>%
    dplyr::group_by(
      FINANCIAL_YEAR,
      PATIENT_ID,
      PATIENT_IDENTIFIED,
      SECTION_NAME,
      SECTION_CODE,
      PARAGRAPH_NAME,
      PARAGRAPH_CODE,
      CHEM_SUB_NAME,
      CHEM_SUB_CODE,
      UNIT_OF_MEASURE,
      PATIENT_COUNT
    ) %>%
    dplyr::summarise(
      TOTAL_QTY = sum(TOTAL_QTY, na.rm = T),
      ITEM_COUNT = sum(ITEM_COUNT, na.rm = T),
      ITEM_PAY_DR_NIC = sum(ITEM_PAY_DR_NIC, na.rm = T),
      .groups = "drop"
    ) 
  
  fact_paragraph <- fact %>%
    dplyr::group_by(
      FINANCIAL_YEAR,
      SECTION_NAME,
      SECTION_CODE,
      PARAGRAPH_NAME,
      PARAGRAPH_CODE,
      CHEM_SUB_NAME,
      CHEM_SUB_CODE,
      UNIT_OF_MEASURE,
      PATIENT_IDENTIFIED
    ) %>%
    dplyr::summarise(
      PATIENT_COUNT = sum(PATIENT_COUNT, na.rm = T),
      TOTAL_QTY = sum(TOTAL_QTY, na.rm = T),
      ITEM_COUNT = sum(ITEM_COUNT, na.rm = T),
      ITEM_PAY_DR_NIC = sum(ITEM_PAY_DR_NIC, na.rm = T)/100,
      .groups = "drop"
    ) %>%
    dplyr::arrange(
      FINANCIAL_YEAR,
      SECTION_CODE,
      PARAGRAPH_CODE,
      CHEM_SUB_CODE,
      desc(PATIENT_IDENTIFIED)
    ) %>%
    collect() %>%
    filter(FINANCIAL_YEAR <= ltst_year)
  
  return(fact_paragraph)
  
}

raw_data$chem_sub_annual <- chem_sub_extract(con)

# icb level data
icb_extract <- function(
    con,
    table = "HRT_FACT_DIM"
) {
  
  fact <- dplyr::tbl(con,
                     from = table) %>%
    dplyr::mutate(
      PATIENT_COUNT = case_when(
        PATIENT_IDENTIFIED == "Y" ~ 1,
        TRUE ~ 0
      )
    ) %>%
    dplyr::group_by(
      FINANCIAL_YEAR,
      PATIENT_ID,
      PATIENT_IDENTIFIED,
      REGION_NAME,
      REGION_CODE,
      STP_NAME,
      STP_CODE,
      PATIENT_COUNT
    ) %>%
    dplyr::summarise(
      ITEM_COUNT = sum(ITEM_COUNT, na.rm = T),
      ITEM_PAY_DR_NIC = sum(ITEM_PAY_DR_NIC, na.rm = T)
    ) 
  
  fact_stp <- fact %>%
    dplyr::group_by(
      FINANCIAL_YEAR,
      REGION_NAME,
      REGION_CODE,
      STP_NAME,
      STP_CODE,
      PATIENT_IDENTIFIED
    ) %>%
    dplyr::summarise(
      PATIENT_COUNT = sum(PATIENT_COUNT, na.rm = T),
      ITEM_COUNT = sum(ITEM_COUNT, na.rm = T),
      ITEM_PAY_DR_NIC = sum(ITEM_PAY_DR_NIC, na.rm = T)/100,
      .groups = "drop"
    ) %>%
    collect() %>%
    mutate(
      REGION_CODE_ORDER = case_when(
        REGION_CODE == "-" ~ 2,
        TRUE ~ 1
      ),
      STP_NAME_ORDER = case_when(
        STP_NAME == "UNKNOWN STP" ~ 2,
        TRUE ~ 1
      )
    ) %>%
    dplyr::arrange(
      FINANCIAL_YEAR,
      REGION_CODE_ORDER,
      REGION_CODE,
      STP_NAME_ORDER,
      STP_NAME,
      desc(PATIENT_IDENTIFIED)
    ) %>%
    select(
      -REGION_CODE_ORDER,
      -STP_NAME_ORDER
    ) %>%
    filter(FINANCIAL_YEAR <= ltst_year)
  
  return(fact_stp)
  
}

raw_data$icb_annual <- icb_extract(con)

# sex annual
gender_extract_annual <- function(
    con,
    table = "HRT_FACT_DIM"
) {
  
  fact <- dplyr::tbl(con,
                     from = table) %>%
    dplyr::mutate(
      PATIENT_COUNT = case_when(
        PATIENT_IDENTIFIED == "Y" ~ 1,
        TRUE ~ 0
      )
    ) %>%
    dplyr::group_by(
      FINANCIAL_YEAR,
      PATIENT_ID,
      PATIENT_IDENTIFIED,
      PDS_GENDER,
      PATIENT_COUNT
    ) %>%
    dplyr::summarise(
      ITEM_COUNT = sum(ITEM_COUNT, na.rm = T),
      ITEM_PAY_DR_NIC = sum(ITEM_PAY_DR_NIC, na.rm = T)
    ) 
  
  fact_gender <- fact %>%
    dplyr::group_by(
      FINANCIAL_YEAR,
      PDS_GENDER,
      PATIENT_IDENTIFIED
    ) %>%
    dplyr::summarise(
      PATIENT_COUNT = sum(PATIENT_COUNT, na.rm = T),
      ITEM_COUNT = sum(ITEM_COUNT, na.rm = T),
      ITEM_PAY_DR_NIC = sum(ITEM_PAY_DR_NIC, na.rm = T)/100,
      .groups = "drop"
    ) %>%
    dplyr::arrange(
      FINANCIAL_YEAR,
      PDS_GENDER,
      desc(PATIENT_IDENTIFIED)
    ) %>%
    collect() %>%
    filter(FINANCIAL_YEAR <= ltst_year)
  
  return(fact_gender)
  
}

raw_data$gender_annual <- gender_extract_annual(con)

ageband_extract_annual <- function(
    con,
    table = "HRT_FACT_DIM"
) {
  
  fact <- dplyr::tbl(src = con,
                     from = table) %>%
    dplyr::mutate(
      PATIENT_COUNT = case_when(
        PATIENT_IDENTIFIED == "Y" ~ 1,
        TRUE ~ 0
      )
    ) %>%
    dplyr::group_by(
      FINANCIAL_YEAR,
      PATIENT_ID,
      PATIENT_IDENTIFIED,
      DALL_5YR_BAND,
      PATIENT_COUNT
    ) %>%
    dplyr::summarise(
      ITEM_COUNT = sum(ITEM_COUNT, na.rm = T),
      ITEM_PAY_DR_NIC = sum(ITEM_PAY_DR_NIC, na.rm = T)
    ) 
  
  fact_age <- fact %>%
    dplyr::mutate(
      AGE_BAND = dplyr::case_when(
        is.na(DALL_5YR_BAND) ~ "Unknown",
        TRUE ~ DALL_5YR_BAND
      )
    ) %>%
    dplyr::group_by(
      FINANCIAL_YEAR,
      AGE_BAND,
      PATIENT_IDENTIFIED
    ) %>%
    dplyr::summarise(
      PATIENT_COUNT = sum(PATIENT_COUNT, na.rm = T),
      ITEM_COUNT = sum(ITEM_COUNT, na.rm = T),
      ITEM_PAY_DR_NIC = sum(ITEM_PAY_DR_NIC, na.rm = T)/100,
      .groups = "drop"
    ) %>%
    dplyr::arrange(
      FINANCIAL_YEAR,
      AGE_BAND,
      desc(PATIENT_IDENTIFIED)
    ) %>%
    collect() %>%
    filter(FINANCIAL_YEAR <= ltst_year)
  
  return(fact_age)
  
}

raw_data$ageband_annual <- ageband_extract_annual(con)

quintile_extract_annual <- function(
    con,
    table = "HRT_FACT_DIM"
) {
# breakdown by imd quintile
fact <- dplyr::tbl(con,
                   from = table) %>%
  dplyr::mutate(
    PATIENT_COUNT = case_when(
      PATIENT_IDENTIFIED == "Y" ~ 1,
      TRUE ~ 0
    ),
    IMD_QUINTILE = case_when(
      IMD_DECILE <= 2 ~ 1,
      IMD_DECILE <= 4 ~ 2,
      IMD_DECILE <= 6 ~ 3,
      IMD_DECILE <= 8 ~ 4,
      IMD_DECILE <= 10 ~ 5,
      TRUE ~ IMD_DECILE
    )
  ) %>%
  dplyr::group_by(
    FINANCIAL_YEAR,
    PATIENT_ID,
    PATIENT_IDENTIFIED,
    PATIENT_COUNT,
    IMD_QUINTILE
  ) %>%
  dplyr::summarise(
    ITEM_COUNT = sum(ITEM_COUNT, na.rm = T),
    ITEM_PAY_DR_NIC = sum(ITEM_PAY_DR_NIC, na.rm = T),
    .groups = "drop"
  ) 

table <- fact %>%
  dplyr::group_by(
    FINANCIAL_YEAR,
    PATIENT_IDENTIFIED,
    IMD_QUINTILE
  ) %>%
  dplyr::summarise(
    PATIENT_COUNT = sum(PATIENT_COUNT, na.rm = T),
    ITEM_COUNT = sum(ITEM_COUNT, na.rm = T),
    ITEM_PAY_DR_NIC = sum(ITEM_PAY_DR_NIC, na.rm = T)/100,
    .groups = "drop"
  ) %>%
  dplyr::arrange(
    FINANCIAL_YEAR,
    IMD_QUINTILE,
    desc(PATIENT_IDENTIFIED)
  ) %>%
  collect() %>%
  filter(
    FINANCIAL_YEAR <= ltst_year
  ) 
return(table)
}

raw_data$quintile_annual <- quintile_extract_annual(con)

quintile_age_extract_annual <- function(
    con,
    table = "HRT_FACT_DIM"
) {
  # breakdown by imd quintile
  fact <- dplyr::tbl(con,
                     from = table) %>%
    dplyr::mutate(
      PATIENT_COUNT = case_when(
        PATIENT_IDENTIFIED == "Y" ~ 1,
        TRUE ~ 0
      ),
      IMD_QUINTILE = case_when(
        IMD_DECILE <= 2 ~ 1,
        IMD_DECILE <= 4 ~ 2,
        IMD_DECILE <= 6 ~ 3,
        IMD_DECILE <= 8 ~ 4,
        IMD_DECILE <= 10 ~ 5,
        TRUE ~ IMD_DECILE
      )
    ) %>%
    dplyr::group_by(
      FINANCIAL_YEAR,
      PATIENT_ID,
      PATIENT_IDENTIFIED,
      PATIENT_COUNT,
      DALL_5YR_BAND,
      IMD_QUINTILE
    ) %>%
    dplyr::summarise(
      ITEM_COUNT = sum(ITEM_COUNT, na.rm = T),
      ITEM_PAY_DR_NIC = sum(ITEM_PAY_DR_NIC, na.rm = T),
      .groups = "drop"
    ) 
  
  table <- fact %>%
    dplyr::mutate(
      AGE_BAND = dplyr::case_when(
        is.na(DALL_5YR_BAND) ~ "Unknown",
        TRUE ~ DALL_5YR_BAND
      )
    ) %>%
    dplyr::group_by(
      FINANCIAL_YEAR,
      PATIENT_IDENTIFIED,
      AGE_BAND,
      IMD_QUINTILE
    ) %>%
    dplyr::summarise(
      PATIENT_COUNT = sum(PATIENT_COUNT, na.rm = T),
      ITEM_COUNT = sum(ITEM_COUNT, na.rm = T),
      ITEM_PAY_DR_NIC = sum(ITEM_PAY_DR_NIC, na.rm = T)/100,
      .groups = "drop"
    ) %>%
    dplyr::arrange(
      FINANCIAL_YEAR,
      AGE_BAND,
      IMD_QUINTILE,
      desc(PATIENT_IDENTIFIED)
    ) %>%
    collect() %>%
    filter(
      FINANCIAL_YEAR <= ltst_year
    ) 
  return(table)
}

raw_data$quintile_age_annual <- quintile_age_extract_annual(con)

#function for large standard error arrow
per_s_error <- function(base1, per1, base2, per2) {
  base1 <- base1
  per1 <- per1
  base2 <- base2
  per2 <- per2
  pooled_per <-
    ((base1 * per1 / 100) + (base2 * per2 / 100)) / (base1 + base2)
  s_error <-
    sqrt((pooled_per * (1 - pooled_per)) * ((1 / base1) + (1 / base2)))
  sig_value <- abs((per1 / 100 - per2 / 100) / s_error)
  change <- "no change"
  if (sig_value > 1.96) {
    change <- "change"
  }
  change
}


DBI::dbDisconnect(con)

# 5. data manipulation ----------------------------------------------------
# build function to apply sdc
apply_sdc <- function(data, level = 5, rounding = FALSE, round_val = 5, mask = as.numeric(NA)) {
  `%>%` <- magrittr::`%>%`
  rnd <- round_val
  if(is.character(mask)) {
    type <- function(x) as.character(x)
  } else {
    type <- function(x) x
  }
  data %>% dplyr::mutate(
    dplyr::across(
      where(is.numeric), 
      .fns = ~ dplyr::case_when(
        .x >= level & rounding == T ~ type(rnd * round(.x/rnd)),
        .x < level & .x > 0 & rounding == T ~ mask,
        .x < level & .x > 0 & rounding == F ~ mask,
        TRUE ~ type(.x)
      ),
      .names = "sdc_{.col}"
    )
  )
}

# annual
pi_data_annual <- raw_data$pi_excel_annual %>%
  apply_sdc() %>%
  select(
    FINANCIAL_YEAR,
    sdc_RATE
  ) %>%
  rename(
    "Financial Year" = 1,
    "Identified Patient Rate" = 2
  )

national_data <- raw_data$national_annual %>%
  select(
    FINANCIAL_YEAR,
    PATIENT_IDENTIFIED,
    PATIENT_COUNT,
    ITEM_COUNT,
    ITEM_PAY_DR_NIC
  ) %>%
  apply_sdc() %>%
  select(1,2,6,7,8) %>%
  dplyr::rename(
    "Financial Year" = 1,
    "Identified Patient Flag" = 2,
    "Total Identified Patients" = 3,
    "Total Items" = 4,
    "Total Net Ingredient Cost (GBP)" = 5
  )

nat_pop <- ons_national_pop(year = 2015:2020, area = "ENPOP")

nat_pop_data <- national_data %>%
  filter(
    `Total Identified Patients` != 0
  ) %>%
  mutate(
    `Mid-year Population Year` = as.numeric(substr(`Financial Year`, 1, 4))
  ) %>%
  select(
    `Financial Year`,
    `Mid-year Population Year`,
    `Total Identified Patients`
  ) %>%
  left_join(
    nat_pop, 
    by = c("Mid-year Population Year" = "YEAR")
  ) %>%
  mutate(
    `Patients per 1,000 Population` = `Total Identified Patients` / ENPOP * 1000 
  ) %>%
  rename(
    "Mid-year Population Estimate" = 4
  )

paragraph_annual <- raw_data$national_par_annual %>%
  apply_sdc() %>%
  select(1,2,3,4,5,6,10,11,12) %>%
  rename(
    "Financial Year" = 1,
    "BNF Section Name" = 2,
    "BNF Section Code" = 3,
    "BNF Paragraph Name" = 4,
    "BNF Paragraph Code" = 5,
    "Identified Patient Flag" = 6,
    "Total Identified Patients" = 7,
    "Total Items" = 8,
    "Total Net Ingredient Cost (GBP)" = 9
  )

chem_sub_annual <- raw_data$chem_sub_annual %>%
  apply_sdc() %>%
  select(1,2,3,4,5,6,7,8,9,14,15,16,17) %>%
  rename(
    "Financial Year" = 1,
    "BNF Section Name" = 2,
    "BNF Section Code" = 3,
    "BNF Paragraph Name" = 4,
    "BNF Paragraph Code" = 5,
    "Chemical Subtance" = 6,
    "Chemical Substance Code" = 7,
    "Unit of Measure" = 8,
    "Identified Patient Flag" = 9,
    "Total Quantity" = 10,
    "Total Identified Patients" = 11,
    "Total Items" = 12,
    "Total Net Ingredient Cost (GBP)" = 13
  )

icb_annual <- raw_data$icb_annual %>%
  apply_sdc() %>%
  select(1,2,3,4,5,6,10,11,12) %>%
  rename(
    "Financial Year" = 1,
    "NHS England Region Name" = 2,
    "NHS England Region Code" = 3,
    "ICB Name" = 4,
    "ICB Code" = 5,
    "Identified Patient Flag" = 6,
    "Total Identified Patients" = 7,
    "Total Items" = 8,
    "Total Net Ingredient Cost (GBP)" = 9
  )

gender_annual <- raw_data$gender_annual %>%
  apply_sdc() %>%
  select(1,2,3,7,8,9) %>%
  rename(
    "Financial Year" = 1,
    "Sex" = 2,
    "Identified Patient Flag" = 3,
    "Total Identified Patients" = 4,
    "Total Items" = 5,
    "Total Net Ingredient Cost (GBP)" = 6
  )

ageband_annual <- raw_data$ageband_annual %>%
  apply_sdc() %>%
  select(1,2,3,7,8,9) %>%
  rename(
    "Financial Year" = 1,
    "Age Band" = 2,
    "Identified Patient Flag" = 3,
    "Total Identified Patients" = 4,
    "Total Items" = 5,
    "Total Net Ingredient Cost (GBP)" = 6
  )

quintile_annual <- raw_data$quintile_annual %>%
  apply_sdc() %>%
  select(1,2,3,8,9,10) %>%
  left_join(
    imd_population,
    by = c("IMD_QUINTILE"= "IMD_QUINTILE")
  ) %>%
  mutate(
    `Patients per 1,000 Population` = sdc_PATIENT_COUNT / POPULATION * 1000
  ) %>%
  mutate(
    IMD_QUINTILE = case_when(
      is.na(IMD_QUINTILE) ~ as.character("Unknown"),
      IMD_QUINTILE == 1 ~ as.character("1 - Most deprived"),
      IMD_QUINTILE == 5 ~ as.character("5 - Least deprived"),
      TRUE ~ as.character(IMD_QUINTILE)
    )
  ) %>%
  select(1,2,3,7,4,5,6,8) %>%
  rename(
    "Financial Year" = 1,
    "Identified Patient Flag" = 2,
    "IMD Quintile" = 3,
    "Population" = 4,
    "Total Identified Patients" = 5,
    "Total Items" = 6,
    "Total Net Ingredient Cost (GBP)" = 7,
    "Patients per 1,000 Population" = 8
  )

quintile_age_annual <- raw_data$quintile_age_annual %>%
  apply_sdc() %>%
  select(1,2,3,4,9,10,11) %>%
  left_join(
    imd_population_age,
    by = c("IMD_QUINTILE"= "IMD_QUINTILE",
           "AGE_BAND" = "AGE_BAND")
  ) %>%
  mutate(
    `Patients per 1,000 Population` = sdc_PATIENT_COUNT / POPULATION * 1000
  ) %>%
  mutate(
    IMD_QUINTILE = case_when(
      is.na(IMD_QUINTILE) ~ as.character("Unknown"),
      IMD_QUINTILE == 1 ~ as.character("1 - Most deprived"),
      IMD_QUINTILE == 5 ~ as.character("5 - Least deprived"),
      TRUE ~ as.character(IMD_QUINTILE)
    )
  ) %>%
  select(1,2,3,4,8,5,6,7,9) %>%
  rename(
    "Financial Year" = 1,
    "Identified Patient Flag" = 2,
    "Age Band" = 3,
    "IMD Quintile" = 4,
    "Population" = 5,
    "Total Identified Patients" = 6,
    "Total Items" = 7,
    "Total Net Ingredient Cost (GBP)" = 8
  )


# 6. write data to .xlsx --------------------------------------------------
# create wb object
# create list of sheetnames needed (overview and metadata created automatically)
sheetNames <- c("Patient_Identification",
                "National_Total",
                "National_Population",
                "BNF_Paragraph",
                "Chemical_Substance", 
                "ICB",
                "Sex", 
                "Age_Band",
                "IMD_Quintile",
                "IMD_Quintile_Age")

wb <- create_wb(sheetNames)

#create metadata tab (will need to open file and auto row heights once ran)
meta_fields <- c(
  "BNF Paragraph Code",
  "BNF Paragraph Name",
  "Financial Year",
  "Year Month",
  "Financial Quarter",
  "Identified Patient",
  "Total Items",
  "Total Net Ingredient Cost (GBP)",
  "Total Patients"
)

meta_descs <-
  c(
    "The unique code used to refer to the British National Formulary (BNF) paragraph.",
    "The name given to a British National Formulary (BNF) paragraph. This is the next broadest grouping of the BNF therapeutical classification system after section, below chapter.",
    "The financial year to which the data belongs.",
    "The year and month to which the data belongs, denoted in YYYYMM format.",
    "The financial quarter to which the data belongs.",
    "This shows where an item has been attributed to an NHS number that has been verified by the Personal Demographics Service (PDS).",
    "The number of prescription items dispensed. 'Items' is the number of times a product appears on a prescription form. Prescription forms include both paper prescriptions and electronic messages.",
    "Total Net Ingredient Cost is the amount that would be paid using the basic price of the prescribed drug or appliance and the quantity prescribed. Sometimes called the 'Net Ingredient Cost' (NIC). The basic price is given either in the Drug Tariff or is determined from prices published by manufacturers, wholesalers or suppliers. Basic price is set out in Parts 8 and 9 of the Drug Tariff. For any drugs or appliances not in Part 8, the price is usually taken from the manufacturer, wholesaler or supplier of the product. This is given in GBP (£).",
    "Where patients are identified via the flag, the number of patients that the data corresponds to. This will always be 0 where 'Identified Patient' = N."
  )

create_metadata(wb,
                meta_fields,
                meta_descs
)

#### Patient identification
# write data to sheet
write_sheet(
  wb,
  "Patient_Identification",
  paste0("Hormone replacement therapy - England - 2015/2016 to ",
  ltst_year,
  " - Proportion of items for which an NHS number was recorded (%)"),
  c(
    "1. The below proportions reflect the percentage of prescription items where a NHS number was recorded."
  ),
  pi_data_annual,
  42
)

#left align columns A
format_data(wb,
            "Patient_Identification",
            c("A"),
            "left",
            "")

#right align columns and round to 2 DP 
format_data(wb,
            "Patient_Identification",
            c("B"),
            "right",
            "0.00")

#### National data
# write data to sheet
write_sheet(
  wb,
  "National_Total",
  paste0("Hormone replacement therapy - England - 2015/2016 to ",
         ltst_year,
         " - Yearly totals split by identified patients"),
  c(
    "1. Field definitions can be found on the 'Metadata' tab."
  ),
  national_data,
  42
)

#left align columns A
format_data(wb,
            "National_Total",
            c("A", "B"),
            "left",
            "")

#right align columns and round to 2 DP 
format_data(wb,
            "National_Total",
            c("C", "D"),
            "right",
            "#,##0")

format_data(wb,
            "National_Total",
            c("E"),
            "right",
            "#,##0.00")

#### National population
# write data to sheet
write_sheet(
  wb,
  "National_Population",
  paste0("Hormone replacement therapy - England - 2015/2016 to ",
         ltst_year,
         " - Population totals split by financial year"),
  c(
    "1. Some cells in this table are empty because ONS population estimates for 2021/2022 were not available prior to publication.",
    "2. ONS population estimates taken from https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates."
  ),
  nat_pop_data,
  42
)

#left align columns A
format_data(wb,
            "National_Population",
            c("A", "B"),
            "left",
            "")

#right align columns and round to 2 DP 
format_data(wb,
            "National_Population",
            c("C", "D"),
            "right",
            "#,##0")

format_data(wb,
            "National_Population",
            c("E"),
            "right",
            "0.00")

#### Paragraph annual
# write data to sheet
write_sheet(
  wb,
  "BNF_Paragraph",
  paste0("Hormone replacement therapy - England - 2015/2016 to ",
         ltst_year,
         " - Yearly totals split by BNF paragraph and identified patients"),
  c(
    "1. Field definitions can be found on the 'Metadata' tab.",
    "2. Statistical disclosure control has been applied to cells containing 5 or fewer patients or items. These cells will appear blank."
  ),
  paragraph_annual,
  42
)

format_data(wb,
            "BNF_Paragraph",
            c("A", "B", "C", "D", "E", "F"),
            "left",
            "")

format_data(wb,
            "BNF_Paragraph",
            c("G", "H"),
            "right",
            "#,##0")

format_data(wb,
            "BNF_Paragraph",
            c("I"),
            "right",
            "#,##0.00")

#### Chemical substance annual
# write data to sheet
write_sheet(
  wb,
  "Chemical_Substance",
  paste0("Hormone replacement therapy - England - 2015/2016 to ",
         ltst_year,
         " - Yearly totals split by chemical substance and identified patients"),
  c(
    "1. Field definitions can be found on the 'Metadata' tab.",
    "2. Statistical disclosure control has been applied to cells containing 5 or fewer patients or items. These cells will appear blank."
  ),
  chem_sub_annual,
  42
)

format_data(wb,
            "Chemical_Substance",
            c("A", "B", "C", "D", "E", "F", "G", "H", "I"),
            "left",
            "")

format_data(wb,
            "Chemical_Substance",
            c("J", "K", "L"),
            "right",
            "#,##0")

format_data(wb,
            "Chemical_Substance",
            c("M"),
            "right",
            "#,##0.00")

#### ICB annual
# write data to sheet
write_sheet(
  wb,
  "ICB",
  paste0("Hormone replacement therapy - England - 2015/2016 to ",
         ltst_year,
         " - Yearly totals split by ICB, BNF Paragraph and identified patients"),
  c(
    "1. Field definitions can be found on the 'Metadata' tab.",
    "2. Statistical disclosure control has been applied to cells containing 5 or fewer patients or items. These cells will appear blank."
  ),
  icb_annual,
  42
)

format_data(wb,
            "ICB",
            c("A", "B", "C", "D", "E", "F"),
            "left",
            "")

format_data(wb,
            "ICB",
            c("G", "H"),
            "right",
            "#,##0")

format_data(wb,
            "ICB",
            c("I"),
            "right",
            "#,##0.00")

#### Sex annual
# write data to sheet
write_sheet(
  wb,
  "Sex",
  paste0("Hormone replacement therapy - England - 2015/2016 to ",
         ltst_year,
         " - Totals by sex"),
  c(
    "1. Field definitions can be found on the 'Metadata' tab.",
    "2. Statistical disclosure control has been applied to cells containing 5 or fewer patients or items. These cells will appear blank.",
    "3. It is possible for a patient to be codified with gender 'unknown' or 'indeterminate'. Due to the low number of patients that these two groups contain the NHSBSA has decided to group these classifications together."
  ),
  gender_annual,
  42
)

format_data(wb,
            "Sex",
            c("A", "B", "C"),
            "left",
            "")

format_data(wb,
            "Sex",
            c("D", "E"),
            "right",
            "#,##0")

format_data(wb,
            "Sex",
            c("F"),
            "right",
            "#,##0.00")

#### Age annual
# write data to sheet
write_sheet(
  wb,
  "Age_Band",
  paste0("Hormone replacement therapy - England - 2015/2016 to ",
         ltst_year,
         " - Totals by age band"),
  c(
    "1. Field definitions can be found on the 'Metadata' tab.",
    "2. Statistical disclosure control has been applied to cells containing 5 or fewer patients or items. These cells will appear blank."
    ),
  ageband_annual,
  42
)

format_data(wb,
            "Age_Band",
            c("A", "B", "C"),
            "left",
            "")

format_data(wb,
            "Age_Band",
            c("D", "E"),
            "right",
            "#,##0")

format_data(wb,
            "Age_Band",
            c("F"),
            "right",
            "#,##0.00")

#### IMD quintile annual
# write data to sheet
write_sheet(
  wb,
  "IMD_Quintile",
  paste0("Hormone replacement therapy - England - 2015/2016 to ",
         ltst_year,
         " - Totals by IMD quintile"),
  c(
    "1. Field definitions can be found on the 'Metadata' tab.",
    "2. Statistical disclosure control has been applied to cells containing 5 or fewer patients or items. These cells will appear blank.",
    "3. Where a patient's postcode has not been able to to be matched to NSPL or the patient has not been identified the records are reported as 'unknown' IMD decile.",
    "4. ONS population estimates taken from https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/populationandmigration/populationestimates/adhocs/13773populationsbyindexofmultipledeprivationimddecileenglandandwales2020/populationbyimdenglandandwales2020.xlsx"
  ),
  quintile_annual,
  42
)

format_data(wb,
            "IMD_Quintile",
            c("A", "B", "C"),
            "left",
            "")

format_data(wb,
            "IMD_Quintile",
            c("D", "E", "F"),
            "right",
            "#,##0")

format_data(wb,
            "IMD_Quintile",
            c("G","H"),
            "right",
            "#,##0.00")

#### IMD quintile/age annual
# write data to sheet
write_sheet(
  wb,
  "IMD_Quintile_Age",
  paste0("Hormone replacement therapy - England - 2015/2016 to ",
         ltst_year,
         " - Totals by IMD quintile and age band"),
  c(
    "1. Field definitions can be found on the 'Metadata' tab.",
    "2. Statistical disclosure control has been applied to cells containing 5 or fewer patients or items. These cells will appear blank.",
    "3. Where a patient's postcode has not been able to to be matched to NSPL or the patient has not been identified the records are reported as 'unknown' IMD decile.",
    "4. ONS population estimates taken from https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/populationandmigration/populationestimates/adhocs/13773populationsbyindexofmultipledeprivationimddecileenglandandwales2020/populationbyimdenglandandwales2020.xlsx"
  ),
  quintile_age_annual,
  42
)

format_data(wb,
            "IMD_Quintile_Age",
            c("A", "B", "C", "D"),
            "left",
            "")

format_data(wb,
            "IMD_Quintile_Age",
            c("E", "F", "G"),
            "right",
            "#,##0")

format_data(wb,
            "IMD_Quintile_Age",
            c("H", "I"),
            "right",
            "#,##0.00")

#save file into outputs folder
openxlsx::saveWorkbook(wb,
                       "outputs/hrt.xlsx",
                       overwrite = TRUE)

# 7. automate narratives --------------------------------------------------

# 8. render markdowns ------------------------------------------------------

rmarkdown::render("hrt-narrative.Rmd",
                  output_format = "html_document",
                  output_file = "outputs/hrt.html")

rmarkdown::render("hrt-narrative.Rmd",
                  output_format = "word_document",
                  output_file = "outputs/hrt.docx")

rmarkdown::render("hrt-background.Rmd",
                  output_format = "html_document",
                  output_file = "outputs/hrt-background-info-methodology-v001.html")

rmarkdown::render("hrt-background.Rmd",
                  output_format = "word_document",
                  output_file = "outputs/hrt-background-info-methodology-v001.docx")



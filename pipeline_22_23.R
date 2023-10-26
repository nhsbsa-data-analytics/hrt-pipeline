# pipeline.R --------------------------------------------------------------
# This script is used to run the RAP for the HRT publication


#clear environment
rm(list = ls())


#source functions
source("./functions/functions.R")

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
    "readxl",
    "DT",
    "kableExtra"
  )

utils::install.packages(req_pkgs, dependencies = TRUE)

#
# reinstall hrtR if needed
#
#devtools::install_github("nhsbsa-data-analytics/nhsbsaR")
#
# devtools::install_github("nhsbsa-data-analytics/hrtR",
#                          auth_token = Sys.getenv("GITHUB_PAT"))
#


# #unload and reinstall hrtR if needed
devtools::install_github("nhsbsa-data-analytics/hrtR",
                         auth_token = Sys.getenv("GITHUB_PAT"), force = TRUE)
 detach("package:hrtR", unload = TRUE)

invisible(lapply(c(req_pkgs,  "nhsbsaR", "hrtR"), library, character.only = TRUE))

# 2. set options ----------------------------------------------------------
hrt_options()

# 3. build fact table if new data available -------------------
con <- nhsbsaR::con_nhsbsa(dsn = "FBS_8192k",
                           driver = "Oracle in OraClient19Home1",
                           "DWCP")

# load config
config <- yaml::yaml.load_file("config.yml")

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
  dplyr::select(YEAR_MONTH,
                FINANCIAL_YEAR)  %>%
  # add month counts for financial quarters and financial years to latest
  # complete periods
  dplyr::mutate(FY_COUNT = dbplyr::win_over(
    expr = dplyr::sql("count(distinct YEAR_MONTH)"),
    partition = "FINANCIAL_YEAR",
    con = con
  ))

# extract latest available month of data
#set max month needed
max_month <- as.numeric(202306L)
ltst_month <- max_month

# use ltst_month to automate tidy date
ltst_month_tidy <- as.character(paste0(ltst_month, "01"))
ltst_month_tidy <- base::as.Date(ltst_month_tidy, format = "%Y%m%d")
ltst_month_tidy <- format(ltst_month_tidy, "%B %Y")

#calculate date 24 months back for filtering charts
lt_st_month_min <-
  as.POSIXlt(base::as.Date(as.character(paste0(ltst_month, "01")), format = "%Y%m%d"))
lt_st_month_min$mon <- lt_st_month_min$mon - 23
lt_st_month_min <- as.numeric(format(lt_st_month_min, "%Y%m"))

#calculate date 15 months back for filtering chart 11
lt_st_month_min_v2 <-
  as.POSIXlt(base::as.Date(as.character(paste0(ltst_month, "01")), format = "%Y%m%d"))
lt_st_month_min_v2$mon <- lt_st_month_min_v2$mon - 14
lt_st_month_min_v2 <- as.numeric(format(lt_st_month_min_v2, "%Y%m"))

# extract latest available full financial year
ltst_year <- ym_dim %>%
  dplyr::filter(FY_COUNT == 12) %>%
  dplyr::select(FINANCIAL_YEAR) %>%
  dplyr::filter(FINANCIAL_YEAR == max(FINANCIAL_YEAR, na.rm = TRUE)) %>%
  dplyr::distinct() %>%
  dplyr::pull(FINANCIAL_YEAR)

# 4. extract data tables from fact table -----------------------------------------
raw_data <- list()

#patient identification rates
raw_data$pi_table_annual <- pi_extract(con = con,
                                       schema = "KIGRA",
                                       table = "HRT_FACT_202310", time_frame = "FY")

raw_data$pi_table_monthly <- pi_extract(con = con,
                                        schema = "KIGRA",
                                        table = "HRT_FACT_202310", time_frame = "Monthly")

raw_data$pi_excel_annual <- pi_extract_excel(con = con,
                                             schema = "KIGRA",
                                             table = "HRT_FACT_202310", time_frame = "FY")

raw_data$pi_excel_monthly <-
  pi_extract_excel(con = con,
                   schema = "KIGRA",
                   table = "HRT_FACT_202310", time_frame =  "Monthly")

## national level data
raw_data$national_annual <- national_extract(con = con,
                                             schema = "KIGRA",
                                             table = "HRT_FACT_202310", time_frame = "FY")

raw_data$national_monthly <-
  national_extract(con = con,
                   schema = "KIGRA",
                   table = "HRT_FACT_202310", time_frame = "Monthly")

# data by bnf paragraph
raw_data$national_par_annual <-
  paragraph_extract(con = con,
                    schema = "KIGRA",
                    table = "HRT_FACT_202310", time_frame = "FY")

raw_data$national_par_monthly <-
  paragraph_extract(con = con,
                    schema = "KIGRA",
                    table = "HRT_FACT_202310", time_frame = "Monthly")

# data by chemical substance paragraph
raw_data$chem_sub_annual <- chem_sub_extract(con = con,
                                             schema = "KIGRA",
                                             table = "HRT_FACT_202310", time_frame = "FY")

raw_data$chem_sub_monthly <-
  chem_sub_extract(con = con,
                   schema = "KIGRA",
                   table = "HRT_FACT_202310", time_frame = "Monthly")

# data by presentation
raw_data$presentation_annual <-
  presentation_extract(con = con,
                       schema = "KIGRA",
                       table = "HRT_FACT_202310", time_frame = "FY")

raw_data$presentation_monthly <-
  presentation_extract(con = con,
                       schema = "KIGRA",
                       table = "HRT_FACT_202310", time_frame = "Monthly")

# data by presentation with ssp flag
raw_data$ssp_annual <- 
  ssp_extract(con = con,
              schema = "KIGRA",
              table = "HRT_FACT_202310", time_frame = "FY")

raw_data$ssp_monthly <- 
  ssp_extract(con = con,
              schema = "KIGRA",
              table = "HRT_FACT_202310", time_frame = "Monthly")

# icb level data
raw_data$icb_annual <- icb_extract(con = con,
                                   schema = "KIGRA",
                                   table = "HRT_FACT_202310", time_frame = "FY")

raw_data$icb_monthly <- icb_extract(con = con,
                                    schema = "KIGRA",
                                    table = "HRT_FACT_202310", time_frame = "Monthly")

# # sex level data
# raw_data$gender_annual <- gender_extract(con = con,
#                                          schema = "KIGRA",
#                                          table = "HRT_FACT_202310", time_frame = "FY")
# 
# raw_data$gender_monthly <-
#   gender_extract(con = con,
#                  schema = "KIGRA",
#                  table = "HRT_FACT_202310", time_frame = "Monthly")

#age level data
raw_data$ageband_annual <- ageband_extract(con = con,
                                           schema = "KIGRA",
                                           table = "HRT_FACT_202310", time_frame = "FY")

raw_data$ageband_monthly <-
  ageband_extract(con = con,
                  schema = "KIGRA",
                  table = "HRT_FACT_202310", time_frame = "Monthly")

#imd quintile
raw_data$quintile_annual <- quintile_extract(con = con,
                                             schema = "KIGRA",
                                             table = "HRT_FACT_202310", time_frame = "FY")

raw_data$quintile_monthly <-
  quintile_extract(con = con,
                   schema = "KIGRA",
                   table = "HRT_FACT_202310", time_frame = "Monthly")

# quintile age band data
raw_data$quintile_age_annual <-
  quintile_age_extract(con = con,
                       schema = "KIGRA",
                       table = "HRT_FACT_202310", time_frame = "FY")

raw_data$quintile_age_monthly <-
  quintile_age_extract(con = con,
                       schema = "KIGRA",
                       table = "HRT_FACT_202310", time_frame = "Monthly")

#exemption category data
raw_data$exempt_annual <- exemption_extract(con = con,
                                            schema = "KIGRA",
                                            table = "HRT_FACT_202310", time_frame = "FY")

raw_data$exempt_monthly <- exemption_extract(con = con,
                                             schema = "KIGRA",
                                             table = "HRT_FACT_202310", time_frame = "Monthly")
#charge category data
raw_data$charge_annual <- charge_extract(con = con,
                                            schema = "KIGRA",
                                            table = "HRT_FACT_202310", time_frame = "FY")

raw_data$charge_monthly <- charge_extract(con = con,
                                             schema = "KIGRA",
                                             table = "HRT_FACT_202310", time_frame = "Monthly")

# disconnect from DWH
DBI::dbDisconnect(con)

# 5. data manipulation ----------------------------------------------------

# get icb population using external data package
icb_lsoa_lookup <- nhsbsaExternalData::icb_lsoa_lookup()
lsoa_population <- nhsbsaExternalData::lsoa_population()

# build ibc population lookup
icb_pop <- icb_lsoa_lookup |>
  dplyr::left_join(lsoa_population,
                   by = c("LSOA_CODE" = "LSOA_CODE")) |>
  dplyr::group_by(ICB_CODE, ICB_NAME, ICB_LONG_CODE) |>
  dplyr::summarise(POP = sum(POP, na.rm = TRUE),
                   .groups = "drop")

# imd pop data
imd_population_age_gender <- imd_population()
imd_population_age <- imd_population_age_gender %>%
  group_by(IMD_QUINTILE,
           AGE_BAND) %>%
  summarise(POPULATION = sum(POPULATION, na.rm = T))
imd_population <- imd_population_age_gender %>%
  group_by(IMD_QUINTILE) %>%
  summarise(POPULATION = sum(POPULATION, na.rm = T))

# annual
pi_data_annual <- raw_data$pi_excel_annual %>%  dplyr::mutate(
    FINANCIAL_YEAR = case_when(
      FINANCIAL_YEAR == max(FINANCIAL_YEAR) ~ paste0(FINANCIAL_YEAR, " (YTD ", ltst_month_tidy, ")"),
      TRUE ~ FINANCIAL_YEAR
    )
  ) %>%
  select(FINANCIAL_YEAR,
         RATE) %>%
  rename("Financial Year" = 1,
         "Identified Patient Rate" = 2)


national_data <- raw_data$national_annual %>%
  select(FINANCIAL_YEAR,
         PATIENT_IDENTIFIED,
         PATIENT_COUNT,
         ITEM_COUNT,
         ITEM_PAY_DR_NIC) %>%
  #apply_sdc() %>%
  dplyr::mutate(
    FINANCIAL_YEAR = case_when(
      FINANCIAL_YEAR == max(FINANCIAL_YEAR) ~ paste0(FINANCIAL_YEAR, " (YTD ", ltst_month_tidy, ")"),
      TRUE ~ FINANCIAL_YEAR
    )
  ) %>%
  #select(1, 2, 6, 7, 8) %>%
  dplyr::rename(
    "Financial Year" = 1,
    "Identified Patient Flag" = 2,
    "Total Identified Patients" = 3,
    "Total Items" = 4,
    "Total Net Ingredient Cost (GBP)" = 5
  )

nat_pop <- ons_national_pop(year = 2015:2021, area = "ENPOP")

nat_pop_data <- national_data %>%
  filter(`Total Identified Patients` != 0) %>%
  mutate(`Mid-year Population Year` = as.numeric(substr(`Financial Year`, 1, 4))) %>%
  select(`Financial Year`,
         `Mid-year Population Year`,
         `Total Identified Patients`) %>%
  left_join(nat_pop,
            by = c("Mid-year Population Year" = "YEAR")) %>%
  # mutate(ENPOP = case_when(`Financial Year` == "2021/2022" ~ as.numeric(56489800),
  #                                               TRUE ~ as.numeric(ENPOP))) %>%
  mutate(`Patients per 1,000 Population` = `Total Identified Patients` / ENPOP * 1000) %>%
  rename("Mid-year Population Estimate" = 4)

paragraph_annual <- raw_data$national_par_annual %>%
  #apply_sdc() %>%
  #select(1, 2, 3, 4, 5, 6, 10, 11, 12) %>%
  dplyr::mutate(
    FINANCIAL_YEAR = case_when(
      FINANCIAL_YEAR == max(FINANCIAL_YEAR) ~ paste0(FINANCIAL_YEAR, " (YTD ", ltst_month_tidy, ")"),
      TRUE ~ FINANCIAL_YEAR
    )
  ) %>%
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
  #apply_sdc() %>%
  #select(1, 2, 3, 4, 5, 6, 7, 8, 12, 13, 14) %>%
  dplyr::mutate(
    FINANCIAL_YEAR = case_when(
      FINANCIAL_YEAR == max(FINANCIAL_YEAR) ~ paste0(FINANCIAL_YEAR, " (YTD ", ltst_month_tidy, ")"),
      TRUE ~ FINANCIAL_YEAR
    )
  ) %>%
  rename(
    "Financial Year" = 1,
    "BNF Section Name" = 2,
    "BNF Section Code" = 3,
    "BNF Paragraph Name" = 4,
    "BNF Paragraph Code" = 5,
    "Chemical Subtance" = 6,
    "Chemical Substance Code" = 7,
    "Identified Patient Flag" = 8,
    "Total Identified Patients" = 9,
    "Total Items" = 10,
    "Total Net Ingredient Cost (GBP)" = 11
  )

presentation_annual_raw <- raw_data$presentation_annual %>%
  mutate(
    VMPP_UOM = case_when(
      PRESENTATION_BNF == "0702010F0AAAEAE" ~ "gram",
      PRESENTATION_BNF == "0702010G0AAAEAE" ~ "device",
      TRUE ~ VMPP_UOM
    )
  )

#build UOM lookup to impute missing values
lookup <- presentation_annual_raw %>%
  filter(VMPP_UOM != "UNKNOWN") %>%
  select(PRESENTATION_BNF, VMPP_UOM) %>%
  unique() %>%
  rename("IMP_UOM" = 2)


presentation_annual <- presentation_annual_raw %>%
  dplyr::mutate(
    FINANCIAL_YEAR = case_when(
      FINANCIAL_YEAR == max(FINANCIAL_YEAR) ~ paste0(FINANCIAL_YEAR, " (YTD ", ltst_month_tidy, ")"),
      TRUE ~ FINANCIAL_YEAR
    )
  ) %>%
  left_join(
    lookup, by = c("PRESENTATION_BNF" = "PRESENTATION_BNF")
  ) %>%
  mutate(
    VMPP_UOM = case_when(
      VMPP_UOM == "UNKNOWN" ~ IMP_UOM,
      TRUE ~ VMPP_UOM
    )
  ) %>%
  select(-IMP_UOM) %>%
  group_by(
    FINANCIAL_YEAR, 
    SECTION_DESCR,
    BNF_SECTION,
    PARAGRAPH_DESCR,
    BNF_PARAGRAPH,
    CHEMICAL_SUBSTANCE_BNF_DESCR,
    BNF_CHEMICAL_SUBSTANCE,
    PRESENTATION_BNF,
    PRESENTATION_BNF_DESCR,
    GENERIC_BNF_CODE,
    GEN_PRESENTATION_BNF_DESCR,
    VMPP_UOM
  ) %>%
  summarise(
    TOTAL_QTY = sum(TOTAL_QTY, na.rm = T),
    ITEM_COUNT = sum(ITEM_COUNT, na.rm = T),
    ITEM_PAY_DR_NIC = sum(ITEM_PAY_DR_NIC, na.rm = T)
  ) %>%
  ungroup() %>%
  rename(
    "Financial Year" = 1,
    "BNF Section Name" = 2,
    "BNF Section Code" = 3,
    "BNF Paragraph Name" = 4,
    "BNF Paragraph Code" = 5,
    "Chemical Subtance" = 6,
    "Chemical Substance Code" = 7,
    "BNF Presentation Code" = 8,
    "BNF Presentation Name" = 9,
    "Generic BNF Presentation Code" = 10,
    "Generic BNF Presentation Name" = 11,
    "Unit of Measure" = 12,
    "Total Quantity" = 13,
    "Total Items" = 14,
    "Total Net Ingredient Cost (GBP)" = 15
  ) %>%
  mutate(
    `Cost Per Item (GBP)` = `Total Net Ingredient Cost (GBP)` / `Total Items`,
    `Cost Per Quantity (GBP)` = `Total Net Ingredient Cost (GBP)` / `Total Quantity`,
    `Quantity Per Item` = `Total Quantity` / `Total Items`
  )
# select distinct drugs or appendix and to compare to webpage
hrt_drug <- raw_data$presentation_annual %>%
  select(8,9) %>% 
  rename("BNF Presentation Code" = 1,
"BNF Presentation Name" = 2) %>% 
  distinct(`BNF Presentation Code`,
           `BNF Presentation Name`) %>% 
  arrange(`BNF Presentation Code`)

ssp_annual<- raw_data$ssp_annual %>%
  #apply_sdc() %>%
  #select(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 18, 19, 20) %>%
  rename(
    "Financial Year" = 1,
    "BNF Section Name" = 2,
    "BNF Section Code" = 3,
    "BNF Paragraph Name" = 4,
    "BNF Paragraph Code" = 5,
    "Chemical Subtance" = 6,
    "Chemical Substance Code" = 7,
    "BNF Presentation Code" = 8,
    "BNF Presentation Name" = 9,
    "Generic BNF Presentation Code" = 10,
    "Generic BNF Presentation Name" = 11,
    "Unit of Measure" = 12,
    "Total Quantity" = 13,
    "Total Items" = 14,
    "Total Net Ingredient Cost (GBP)" = 15
  ) %>%
  mutate(
    `Cost Per Item (GBP)` = `Total Net Ingredient Cost (GBP)` / `Total Items`,
    `Cost Per Quantity (GBP)` = `Total Net Ingredient Cost (GBP)` / `Total Quantity`,
    `Quantity Per Item` = `Total Quantity` / `Total Items`
  )

icb_annual <- raw_data$icb_annual %>%
  #apply_sdc() %>%
  #select(1, 2, 3, 4, 5, 6, 10, 11, 12) %>%
  dplyr::mutate(
    FINANCIAL_YEAR = case_when(
      FINANCIAL_YEAR == max(FINANCIAL_YEAR) ~ paste0(FINANCIAL_YEAR, " (YTD ", ltst_month_tidy, ")"),
      TRUE ~ FINANCIAL_YEAR
    )
  ) %>%
  rename(
    "Financial Year" = 1,
    "ICB Name" = 2,
    "ICB Code" = 3,
    "Identified Patient Flag" = 4,
    "Total Identified Patients" = 5,
    "Total Items" = 6,
    "Total Net Ingredient Cost (GBP)" = 7
  ) %>%
  mutate(
    `ICB Name` = case_when(
      `ICB Name` == "UNKNOWN STP" ~ "UNKNOWN ICB",
      TRUE ~ `ICB Name`
    )
  )
# 
# gender_annual <- raw_data$gender_annual %>%
#   #apply_sdc() %>%
#   #select(1, 2, 3, 7, 8, 9) %>%
#   dplyr::mutate(
#     FINANCIAL_YEAR = case_when(
#       FINANCIAL_YEAR == max(FINANCIAL_YEAR) ~ paste0(FINANCIAL_YEAR, " (YTD ", ltst_month_tidy, ")"),
#       TRUE ~ FINANCIAL_YEAR
#     )
#   ) %>%
#   rename(
#     "Financial Year" = 1,
#     "Sex" = 2,
#     "Identified Patient Flag" = 3,
#     "Total Identified Patients" = 4,
#     "Total Items" = 5,
#     "Total Net Ingredient Cost (GBP)" = 6
#   )

ageband_annual <- raw_data$ageband_annual %>%
  #apply_sdc() %>%
 # select(1, 2, 3, 7, 8, 9) %>%
  dplyr::mutate(
    FINANCIAL_YEAR = case_when(
      FINANCIAL_YEAR == max(FINANCIAL_YEAR) ~ paste0(FINANCIAL_YEAR, " (YTD ", ltst_month_tidy, ")"),
      TRUE ~ FINANCIAL_YEAR
    )
  ) %>%
  rename(
    "Financial Year" = 1,
    "Age Band" = 2,
    "Identified Patient Flag" = 3,
    "Total Identified Patients" = 4,
    "Total Items" = 5,
    "Total Net Ingredient Cost (GBP)" = 6
  )

quintile_annual <- raw_data$quintile_annual %>%
  #apply_sdc() %>%
  #select(1, 2, 3, 8, 9, 10) %>%
  filter(FINANCIAL_YEAR >= "2019/2020") %>%
  dplyr::mutate(
    FINANCIAL_YEAR = case_when(
      FINANCIAL_YEAR == max(FINANCIAL_YEAR) ~ paste0(FINANCIAL_YEAR, " (YTD ", ltst_month_tidy, ")"),
      TRUE ~ FINANCIAL_YEAR
    )
  ) %>%
  left_join(imd_population,
            by = c("IMD_QUINTILE" = "IMD_QUINTILE")) %>%
  mutate(`Patients per 1,000 Population` = PATIENT_COUNT / POPULATION * 1000) %>%
  mutate(
    IMD_QUINTILE = case_when(
      is.na(IMD_QUINTILE) ~ as.character("Unknown"),
      IMD_QUINTILE == 1 ~ as.character("1 - Most deprived"),
      IMD_QUINTILE == 5 ~ as.character("5 - Least deprived"),
      TRUE ~ as.character(IMD_QUINTILE)
    )
  ) %>%
  select(1, 2, 6, 3, 4, 5, 7) %>%
  rename(
    "Financial Year" = 1,
    "IMD Quintile" = 2,
    "Population" = 3,
    "Total Identified Patients" = 4,
    "Total Items" = 5,
    "Total Net Ingredient Cost (GBP)" = 6,
    "Patients per 1,000 Population" = 7
  )

quintile_age_annual <- raw_data$quintile_age_annual %>%
  #apply_sdc() %>%
  #select(1, 2, 3, 4, 9, 10, 11) %>%
  filter(FINANCIAL_YEAR >= "2019/2020") %>%
  dplyr::mutate(
    FINANCIAL_YEAR = case_when(
      FINANCIAL_YEAR == max(FINANCIAL_YEAR) ~ paste0(FINANCIAL_YEAR, " (YTD ", ltst_month_tidy, ")"),
      TRUE ~ FINANCIAL_YEAR
    )
  ) %>%
  left_join(imd_population_age,
            by = c("IMD_QUINTILE" = "IMD_QUINTILE",
                   "AGE_BAND" = "AGE_BAND")) %>%
  mutate(`Patients per 1,000 Population` = PATIENT_COUNT / POPULATION * 1000) %>%
  mutate(
    IMD_QUINTILE = case_when(
      is.na(IMD_QUINTILE) ~ as.character("Unknown"),
      IMD_QUINTILE == 1 ~ as.character("1 - Most deprived"),
      IMD_QUINTILE == 5 ~ as.character("5 - Least deprived"),
      TRUE ~ as.character(IMD_QUINTILE)
    )
  ) %>%
  select(1, 2, 3, 7, 4, 5, 6, 8) %>%
  rename(
    "Financial Year" = 1,
    "Age Band" = 2,
    "IMD Quintile" = 3,
    "Population" = 4,
    "Total Identified Patients" = 5,
    "Total Items" = 6,
    "Total Net Ingredient Cost (GBP)" = 7
  )
charge_annual <- raw_data$charge_annual %>%
  #apply_sdc() %>%
  #select(1,2,3,6,7,8,9) %>%
  dplyr::mutate(
    FINANCIAL_YEAR = case_when(
      FINANCIAL_YEAR == max(FINANCIAL_YEAR) ~ paste0(FINANCIAL_YEAR, " (YTD ", ltst_month_tidy, ")"),
      TRUE ~ FINANCIAL_YEAR
    )
  ) %>%
  mutate(
    CHARGE_STATUS = case_when(
      CHARGE_STATUS == "Null Charge Status" ~ "Unknown",
      TRUE ~ CHARGE_STATUS
    )
  ) %>%
  rename(
    "Financial Year" = 1,
    "Charge Status Code" = 2,
    "Charge Status" = 3,
    "Identified Patient Flag" = 4,
    "Total Identified Patients" = 5,
    "Total Items" = 6,
    "Total Net Ingredient Cost (GBP)" = 7
  )
exemption_annual <- raw_data$exempt_annual %>%
  #apply_sdc() %>%
  #select(1,2,3,4,5,6,10,11,12) %>%
  dplyr::mutate(
    FINANCIAL_YEAR = case_when(
      FINANCIAL_YEAR == max(FINANCIAL_YEAR) ~ paste0(FINANCIAL_YEAR, " (YTD ", ltst_month_tidy, ")"),
      TRUE ~ FINANCIAL_YEAR
    )
  ) %>%
  mutate(
    CHARGE_STATUS = case_when(
      CHARGE_STATUS == "Null Charge Status" ~ "Unknown",
      TRUE ~ CHARGE_STATUS
    ),
    EXEMPT_CAT = case_when(
      CHARGE_STATUS %in% c("Chargeable at Current Rate", "Chargeable at Previous Rate") ~ "None",
      is.na(EXEMPT_CAT) ~ "Unknown",
      TRUE ~ EXEMPT_CAT
    )
  ) %>%
  rename(
    "Financial Year" = 1,
    "Charge Status Code" = 2,
    "Charge Status" = 3,
    "Exemption Category Code" = 4,
    "Exemption Category" = 5,
    "Identified Patient Flag" = 6,
    "Total Identified Patients" = 7,
    "Total Items" = 8,
    "Total Net Ingredient Cost (GBP)" = 9
  )

# monthly
pi_data_monthly <- raw_data$pi_excel_monthly %>%
  #apply_sdc() %>%
  select(FINANCIAL_YEAR,
         YEAR_MONTH,
         RATE) %>%
  rename(
    "Financial Year" = 1,
    "Year Month" = 2,
    "Identified Patient Rate" = 3
  )

national_data_monthly <- raw_data$national_monthly %>%
  select(
    FINANCIAL_YEAR,
    YEAR_MONTH,
    PATIENT_IDENTIFIED,
    PATIENT_COUNT,
    ITEM_COUNT,
    ITEM_PAY_DR_NIC
  ) %>%
  #apply_sdc() %>%
  #select(1, 2, 3, 8, 9, 10) %>%
  dplyr::rename(
    "Financial Year" = 1,
    "Year Month" = 2,
    "Identified Patient Flag" = 3,
    "Total Identified Patients" = 4,
    "Total Items" = 5,
    "Total Net Ingredient Cost (GBP)" = 6
  )

paragraph_monthly <- raw_data$national_par_monthly %>%
  #apply_sdc() %>%
  #select(1, 2, 3, 4, 5, 6, 7, 12, 13, 14) %>%
  rename(
    "Financial Year" = 1,
    "Year Month" = 2,
    "BNF Section Name" = 3,
    "BNF Section Code" = 4,
    "BNF Paragraph Name" = 5,
    "BNF Paragraph Code" = 6,
    "Identified Patient Flag" = 7,
    "Total Identified Patients" = 8,
    "Total Items" = 9,
    "Total Net Ingredient Cost (GBP)" = 10
  )

chem_sub_monthly <- raw_data$chem_sub_monthly %>%
 # apply_sdc() %>%
  #select(1, 2, 3, 4, 5, 6, 7, 8, 9, 14, 15, 16) %>%
  rename(
    "Financial Year" = 1,
    "Year Month" = 2,
    "BNF Section Name" = 3,
    "BNF Section Code" = 4,
    "BNF Paragraph Name" = 5,
    "BNF Paragraph Code" = 6,
    "Chemical Subtance" = 7,
    "Chemical Substance Code" = 8,
    "Identified Patient Flag" = 9,
    "Total Identified Patients" = 10,
    "Total Items" = 11,
    "Total Net Ingredient Cost (GBP)" = 12
  )


presentation_monthly_raw <- raw_data$presentation_monthly %>%
  mutate(
    VMPP_UOM = case_when(
      PRESENTATION_BNF == "0702010F0AAAEAE" ~ "gram",
      PRESENTATION_BNF == "0702010G0AAAEAE" ~ "device",
      TRUE ~ VMPP_UOM
    )
  )

#build UOM lookup to impute missing values
lookup <- presentation_annual_raw %>%
  filter(VMPP_UOM != "UNKNOWN") %>%
  select(PRESENTATION_BNF, VMPP_UOM) %>%
  unique() %>%
  rename("IMP_UOM" = 2)


presentation_monthly <- presentation_monthly_raw %>%
  left_join(
    lookup, by = c("PRESENTATION_BNF" = "PRESENTATION_BNF")
  ) %>%
  mutate(
    VMPP_UOM = case_when(
      VMPP_UOM == "UNKNOWN" ~ IMP_UOM,
      TRUE ~ VMPP_UOM
    )
  ) %>%
  select(-IMP_UOM) %>%
  group_by(
    FINANCIAL_YEAR, 
    YEAR_MONTH,
    SECTION_DESCR,
    BNF_SECTION,
    PARAGRAPH_DESCR,
    BNF_PARAGRAPH,
    CHEMICAL_SUBSTANCE_BNF_DESCR,
    BNF_CHEMICAL_SUBSTANCE,
    PRESENTATION_BNF,
    PRESENTATION_BNF_DESCR,
    GENERIC_BNF_CODE,
    GEN_PRESENTATION_BNF_DESCR,
    VMPP_UOM
  ) %>%
  summarise(
    TOTAL_QTY = sum(TOTAL_QTY, na.rm = T),
    ITEM_COUNT = sum(ITEM_COUNT, na.rm = T),
    ITEM_PAY_DR_NIC = sum(ITEM_PAY_DR_NIC, na.rm = T)
  ) %>%
  ungroup() %>%
  rename(
    "Financial Year" = 1,
    "Year Month" = 2,
    "BNF Section Name" = 3,
    "BNF Section Code" = 4,
    "BNF Paragraph Name" = 5,
    "BNF Paragraph Code" = 6,
    "Chemical Subtance" = 7,
    "Chemical Substance Code" = 8,
    "BNF Presentation Code" = 9,
    "BNF Presentation Name" = 10,
    "Generic BNF Presentation Code" = 11,
    "Generic BNF Presentation Name" = 12,
    "Unit of Measure" = 13,
    "Total Quantity" = 14,
    "Total Items" = 15,
    "Total Net Ingredient Cost (GBP)" = 16
  ) %>%
  mutate(
    `Cost Per Item (GBP)` = `Total Net Ingredient Cost (GBP)` / `Total Items`,
    `Cost Per Quantity (GBP)` = `Total Net Ingredient Cost (GBP)` / `Total Quantity`,
    `Quantity Per Item` = `Total Quantity` / `Total Items`
  )


ssp_monthly <- raw_data$ssp_monthly %>%
  #apply_sdc() %>%
  #select(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 18, 19, 20) %>%
  rename(
    "Financial Year" = 1,
    "Year Month" = 2,
    "BNF Section Name" = 3,
    "BNF Section Code" = 4,
    "BNF Paragraph Name" = 5,
    "BNF Paragraph Code" = 6,
    "Chemical Subtance" = 7,
    "Chemical Substance Code" = 8,
    "BNF Presentation Code" = 9,
    "BNF Presentation Name" = 10,
    "Generic BNF Presentation Code" = 11,
    "Generic BNF Presentation Name" = 12,
    "Unit of Measure" = 13,
    "Total Quantity" = 14,
    "Total Items" = 15,
    "Total Net Ingredient Cost (GBP)" = 16
  ) %>%
  mutate(
    `Cost Per Item (GBP)` = `Total Net Ingredient Cost (GBP)` / `Total Items`,
    `Cost Per Quantity (GBP)` = `Total Net Ingredient Cost (GBP)` / `Total Quantity`,
    `Quantity Per Item` = `Total Quantity` / `Total Items`
  )

icb_monthly <- raw_data$icb_monthly %>%
  #apply_sdc() %>%
  #select(1, 2, 3, 4, 5, 6, 7, 12, 13, 14) %>%
  rename(
    "Financial Year" = 1,
    "Year Month" = 2,
    "ICB Name" = 3,
    "ICB Code" = 4,
    "Identified Patient Flag" = 5,
    "Total Identified Patients" = 6,
    "Total Items" = 7,
    "Total Net Ingredient Cost (GBP)" = 8
  )  %>%
  mutate(
    `ICB Name` = case_when(
      `ICB Name` == "UNKNOWN STP" ~ "UNKNOWN ICB",
      TRUE ~ `ICB Name`
    )
  )

# gender_monthly <- raw_data$gender_monthly %>%
#   #apply_sdc() %>%
#   #select(1, 2, 3, 4, 9, 10, 11) %>%
#   rename(
#     "Financial Year" = 1,
#     "Year Month" = 2,
#     "Sex" = 3,
#     "Identified Patient Flag" = 4,
#     "Total Identified Patients" = 5,
#     "Total Items" = 6,
#     "Total Net Ingredient Cost (GBP)" = 7
#   )

ageband_monthly <- raw_data$ageband_monthly %>%
  #apply_sdc() %>%
  #select(1, 2, 3, 4, 9, 10, 11) %>%
  rename(
    "Financial Year" = 1,
    "Year Month" = 2,
    "Age Band" = 3,
    "Identified Patient Flag" = 4,
    "Total Identified Patients" = 5,
    "Total Items" = 6,
    "Total Net Ingredient Cost (GBP)" = 7
  )

quintile_monthly <- raw_data$quintile_monthly %>%
  #apply_sdc() %>%
  #select(1, 2, 3, 4, 10, 11, 12) %>%
  filter(FINANCIAL_YEAR >= "2019/2020") %>%
  mutate(
    IMD_QUINTILE = case_when(
      is.na(IMD_QUINTILE) ~ as.character("Unknown"),
      IMD_QUINTILE == 1 ~ as.character("1 - Most deprived"),
      IMD_QUINTILE == 5 ~ as.character("5 - Least deprived"),
      TRUE ~ as.character(IMD_QUINTILE)
    )
  ) %>%
  #select(1, 2, 4, 3, 5, 6, 7) %>%
  rename(
    "Financial Year" = 1,
    "Year Month" = 2,
    #"Identified Patient Flag" = 4,
    "IMD Quintile" = 3,
    "Total Identified Patients" = 4,
    "Total Items" = 5,
    "Total Net Ingredient Cost (GBP)" = 6
  )

quintile_age_monthly <- raw_data$quintile_age_monthly %>%
  #apply_sdc() %>%
  #select(1, 2, 4, 5, 3, 11, 12, 13) %>%
  filter(FINANCIAL_YEAR >= "2019/2020") %>%
  mutate(
    IMD_QUINTILE = case_when(
      is.na(IMD_QUINTILE) ~ as.character("Unknown"),
      IMD_QUINTILE == 1 ~ as.character("1 - Most deprived"),
      IMD_QUINTILE == 5 ~ as.character("5 - Least deprived"),
      TRUE ~ as.character(IMD_QUINTILE)
    )
  ) %>%
  rename(
    "Financial Year" = 1,
    "Year Month" = 2,
    #"Identified Patient Flag" = 5,
    "Age Band" = 3,
    "IMD Quintile" = 4,
    "Total Identified Patients" = 5,
    "Total Items" = 6,
    "Total Net Ingredient Cost (GBP)" = 7
  )

exemption_monthly <- raw_data$exempt_monthly %>%
  #apply_sdc() %>%
  #select(1,2,3,4,5,6,7,12,13,14) %>%
  mutate(
    CHARGE_STATUS = case_when(
      CHARGE_STATUS == "Null Charge Status" ~ "Unknown",
      TRUE ~ CHARGE_STATUS
    ),
    EXEMPT_CAT = case_when(
      CHARGE_STATUS %in% c("Chargeable at Current Rate", "Chargeable at Previous Rate") ~ "None",
      is.na(EXEMPT_CAT) ~ "Unknown",
      TRUE ~ EXEMPT_CAT
    )
  ) %>%
  rename(
    "Financial Year" = 1,
    "Year Month" = 2,
    "Charge Status Code" = 3,
    "Charge Status" = 4,
    "Exemption Category Code" = 5,
    "Exemption Category" = 6,
    "Identified Patient Flag" = 7,
    "Total Identified Patients" = 8,
    "Total Items" = 9,
    "Total Net Ingredient Cost (GBP)" = 10
  )

charge_monthly <- raw_data$charge_monthly %>%
  #apply_sdc() %>%
  #select(1,2,3,6,7,8,9) %>%
  mutate(
    CHARGE_STATUS = case_when(
      CHARGE_STATUS == "Null Charge Status" ~ "Unknown",
      TRUE ~ CHARGE_STATUS
    )
  ) %>%
  rename(
    "Financial Year" = 1,
    "Year Month" = 2,
    "Charge Status Code" = 3,
    "Charge Status" = 4,
    "Identified Patient Flag" = 5,
    "Total Identified Patients" = 6,
    "Total Items" = 7,
    "Total Net Ingredient Cost (GBP)" = 8
  )

# 6. write data to .xlsx --------------------------------------------------

# build max fy ytd for sheet titles
ltst_year_ytd <- paste0(max(raw_data$national_annual$FINANCIAL_YEAR), " (YTD ", ltst_month_tidy, ")")

# FY Excel
# create wb object
# create list of sheetnames needed (overview and metadata created automatically)
sheetNames <- c(
  "Patient_Identification",
  "National_Total",
  "National_Population",
  "Chemical_Substance",
  "Presentations",
  "SSP",
  "ICB",
  #"Sex",
  "Age_Band",
  "IMD_Quintile",
  "IMD_Quintile_Age",
  "Charge_Status"
)

wb <- accessibleTables::create_wb(sheetNames)

#create metadata tab (will need to open file and auto row heights once ran)
meta_fields <- c(
  "BNF Section Name",
  "BNF Section Code",
  "Identified Patient Flag",
  "Total Identified Patients",
  "Total Items",
  "Total Net Ingredient Cost (GBP)",
  "Financial Year",
  "Year Month",
  "BNF Paragraph Name",
  "BNF Paragraph Code",
  "ICB Name",
  "ICB Code",
  "BNF Chemical Substance Name",
  "BNF Chemical Substance Code",
  "Age Band",
  "IMD Quintile"
)

meta_descs <-
  c(
    "The name given to a British National Formulary (BNF) section. This is the next broadest grouping of The BNF Therapeutical classification system after chapter.",
    "The unique code used to refer to the British National Formulary (BNF) section.",
    "This shows where an item has been attributed to an NHS number that has been verified by the Personal Demographics Service.",
    "Where patients are identified via the flag, the number of patients that the data corresponds to. This will always be 0 where 'Identified Patient' = N.",
    "The number of prescription items dispensed. 'Items' is the number of times a product appears on a prescription form. Prescription forms include both paper prescriptions and electronic messages.",
    "Total Net Ingredient Cost is the amount that would be paid using the basic price of the prescribed drug or appliance and the quantity prescribed. Sometimes called the 'Net Ingredient Cost' (NIC). The basic price is given either in the Drug Tariff or is determined from prices published by manufacturers, wholesalers or suppliers. Basic price is set out in Parts 8 and 9 of the Drug Tariff. For any drugs or appliances not in Part 8, the price is usually taken from the manufacturer, wholesaler or supplier of the product. This is given in GBP (£).",
    "The financial year to which the data belongs.",
    "The year and month to which the data belongs, denoted in YYYYMM format.",
    "The name given to the British National Formular (BNF) paragraph. This level of grouping of the BNF Therapeutical classification system sits below BNF section.",
    "The unique code used to refer to the British National Formulary (BNF) paragraph.",
    "The name given to the Integrated Care Board (ICB) that a prescribing organisation belongs to. This is based upon NHSBSA administrative records, not geographical boundaries and more closely reflect the operational organisation of practices than other geographical data sources.",
    "The unique code used to refer to an Integrated Care Board (ICB).",
    "The name of the main active ingredient in a drug. Appliances do not hold a chemical substance, but instead inherit the corresponding BNF section. Determined by the British National Formulatory (BNF) for drugs, or the NHS BSA for appliances. For example, Amoxicillin.",
    "The unique code used to refer to the British National Formulary (BNF) chemical substance.",
    "The age band of the patient as of the 30th September of the corresponding financial year the drug was prescribed.",
    "The IMD quintile of the patient, based on the patient's postcode, where '1' is the 20% of areas with the highest deprivation score in the Index of Multiple Deprivation (IMD) from the English Indices of Deprivation 2019, and '5' is the 20% of areas with the lowest IMD deprivation score. The IMD quintile has been recorded as 'Unknown' where the items are attributed to an unidentified patient, or where we have been unable to match the patient postcode to a postcode in the National Statistics Postcode Lookup (NSPL)."
  )

accessibleTables::create_metadata(wb,
                                  meta_fields,
                                  meta_descs)

## Patient identification

# write data to sheet

#### Patient identification
# write data to sheet
accessibleTables::write_sheet(
  wb,
  "Patient_Identification",
  paste0(
    "Hormone replacement therapy - England - 2015/2016 to ",
    ltst_year_ytd,
    " - Proportion of items for which an NHS number was recorded (%)"
  ),
  c(
    "1. Field definitions can be found on the 'Metadata' tab.",
    "2. The figures in this table relate to prescribing of HRT medications in England that are subsequently dispensed in the community in England, Scotland, Wales, Isle of Man or the Channel Islands by a pharmacy, appliance contractor, dispensing doctor, or have been personally administered by a GP practice. They do not include data on medicines used in secondary care, prisons, or issued by a private prescriber.",
    "3. The below proportions reflect the percentage of prescription items where a NHS number was recorded."
  ),
  pi_data_annual,
  30
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
accessibleTables::write_sheet(
  wb,
  "National_Total",
  paste0(
    "Hormone replacement therapy - England - 2015/2016 to ",
    ltst_year_ytd,
    " - Yearly totals split by identified patients"
  ),
  c("1. Field definitions can be found on the 'Metadata' tab.",
    "2. The figures in this table relate to prescribing of HRT medications in England that are subsequently dispensed in the community in England, Scotland, Wales, Isle of Man or the Channel Islands by a pharmacy, appliance contractor, dispensing doctor, or have been personally administered by a GP practice. They do not include data on medicines used in secondary care, prisons, or issued by a private prescriber."),
  national_data,
  30
)

#left align columns A
accessibleTables::format_data(wb,
            "National_Total",
            c("A", "B"),
            "left",
            "")

#right align columns and round to 2 DP
accessibleTables::format_data(wb,
            "National_Total",
            c("C", "D"),
            "right",
            "#,##0")

accessibleTables::format_data(wb,
            "National_Total",
            c("E"),
            "right",
            "#,##0.00")

#### National population
# write data to sheet
accessibleTables::write_sheet(
  wb,
  "National_Population",
  paste0(
    "Hormone replacement therapy - England - 2015/2016 to ",
    ltst_year_ytd,
    " - Population totals split by financial year"
  ),
  c(
    "1. Field definitions can be found on the 'Metadata' tab.",
    "2. The figures in this table relate to prescribing of HRT medications in England that are subsequently dispensed in the community in England, Scotland, Wales, Isle of Man or the Channel Islands by a pharmacy, appliance contractor, dispensing doctor, or have been personally administered by a GP practice. They do not include data on medicines used in secondary care, prisons, or issued by a private prescriber.",
    "3. Some cells in this table are empty because ONS population estimates for 2022/2023 were not available prior to publication.",
    "4. ONS population estimates taken from https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates."
   
  ),
  nat_pop_data,
  30
)

#left align columns A
accessibleTables::format_data(wb,
            "National_Population",
            c("A", "B"),
            "left",
            "")

#right align columns and round to 2 DP
accessibleTables::format_data(wb,
            "National_Population",
            c("C", "D"),
            "right",
            "#,##0")

accessibleTables::format_data(wb,
            "National_Population",
            c("E"),
            "right",
            "0.00")

# #### Paragraph annual
# # write data to sheet
# accessibleTables::write_sheet(
#   wb,
#   "BNF_Paragraph",
#   paste0(
#     "Hormone replacement therapy - England - 2015/2016 to ",
#     ltst_year_ytd,
#     " - Yearly totals split by BNF paragraph and identified patients"
#   ),
#   c(
#     "1. Field definitions can be found on the 'Metadata' tab.",
#     "2. The figures in this table relate to prescribing of HRT medications in England that are subsequently dispensed in the community in England, Scotland, Wales, Isle of Man or the Channel Islands by a pharmacy, appliance contractor, dispensing doctor, or have been personally administered by a GP practice. They do not include data on medicines used in secondary care, prisons, or issued by a private prescriber."  ),
#   paragraph_annual,
#   30
# )
# 
# accessibleTables::format_data(wb,
#             "BNF_Paragraph",
#             c("A", "B", "C", "D", "E", "F"),
#             "left",
#             "")
# 
# accessibleTables::format_data(wb,
#             "BNF_Paragraph",
#             c("G", "H"),
#             "right",
#             "#,##0")
# 
# accessibleTables::format_data(wb,
#             "BNF_Paragraph",
#             c("I"),
#             "right",
#             "#,##0.00")
# 
#### Chemical substance annual
# write data to sheet
accessibleTables::write_sheet(
  wb,
  "Chemical_Substance",
  paste0(
    "Hormone replacement therapy - England - 2015/2016 to ",
    ltst_year_ytd,
    " - Yearly totals split by chemical substance and identified patients"
  ),
  c(
    "1. Field definitions can be found on the 'Metadata' tab.",
    "2. The figures in this table relate to prescribing of HRT medications in England that are subsequently dispensed in the community in England, Scotland, Wales, Isle of Man or the Channel Islands by a pharmacy, appliance contractor, dispensing doctor, or have been personally administered by a GP practice. They do not include data on medicines used in secondary care, prisons, or issued by a private prescriber."  ),
  chem_sub_annual,
  30
)

accessibleTables::format_data(wb,
            "Chemical_Substance",
            c("A", "B", "C", "D", "E", "F", "G", "H"),
            "left",
            "")

accessibleTables::format_data(wb,
            "Chemical_Substance",
            c("I", "J"),
            "right",
            "#,##0")

accessibleTables::format_data(wb,
            "Chemical_Substance",
            c("K"),
            "right",
            "#,##0.00")

#### presentations annual
# write data to sheet
accessibleTables::write_sheet(
  wb,
  "Presentations",
  paste0(
    "Hormone replacement therapy - England - 2015/2016 to ",
    ltst_year_ytd,
    " - Yearly totals split by presentation"
  ),
  c(
    "1. Field definitions can be found on the 'Metadata' tab.",
    "2. The figures in this table relate to prescribing of HRT medications in England that are subsequently dispensed in the community in England, Scotland, Wales, Isle of Man or the Channel Islands by a pharmacy, appliance contractor, dispensing doctor, or have been personally administered by a GP practice. They do not include data on medicines used in secondary care, prisons, or issued by a private prescriber.",
    "3. For medications which are dispensed in Scotland no unit of measure is captured, this has been imputed from the data on medicines dispensed elsewhere"),
  presentation_annual,
  30
)

accessibleTables::format_data(
  wb,
  "Presentations",
  c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L"),
  "left",
  ""
)

accessibleTables::format_data(wb,
            "Presentations",
            c("M", "N"),
            "right",
            "#,##0")

accessibleTables::format_data(wb,
            "Presentations",
            c("O", "P", "Q", "R"),
            "right",
            "#,##0.00")

#### ssp substance annual
# write data to sheet
accessibleTables::write_sheet(
  wb,
  "SSP",
  paste0(
    "Hormone replacement therapy - England - 2015/2016 to ",
    ltst_year_ytd,
    " - Yearly totals for precribing which has been issued under Serious Shortage Protocols (SSP) split by presentation"
  ),
  c(
    "1. Field definitions can be found on the 'Metadata' tab.",
    "2. The figures in this table relate to prescribing of HRT medications in England that are subsequently dispensed in the community in England, Scotland, Wales, Isle of Man or the Channel Islands by a pharmacy, appliance contractor, dispensing doctor, or have been personally administered by a GP practice. They do not include data on medicines used in secondary care, prisons, or issued by a private prescriber.",
    "3. These figures will be included as part of the totals on the 'Presentations' tab."
  ),
  ssp_annual,
  30
)

accessibleTables::format_data(
  wb,
  "SSP",
  c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L"),
  "left",
  ""
)

accessibleTables::format_data(wb,
            "SSP",
            c("M", "N"),
            "right",
            "#,##0")

accessibleTables::format_data(wb,
            "SSP",
            c("O", "P", "Q", "R"),
            "right",
            "#,##0.00")

#### ICB annual
# write data to sheet
accessibleTables::write_sheet(
  wb,
  "ICB",
  paste0(
    "Hormone replacement therapy - England - 2015/2016 to ",
    ltst_year_ytd,
    " - Yearly totals split by ICB"
  ),
  c(
    "1. Field definitions can be found on the 'Metadata' tab.",
    "2. The figures in this table relate to prescribing of HRT medications in England that are subsequently dispensed in the community in England, Scotland, Wales, Isle of Man or the Channel Islands by a pharmacy, appliance contractor, dispensing doctor, or have been personally administered by a GP practice. They do not include data on medicines used in secondary care, prisons, or issued by a private prescriber."  ),
  icb_annual,
  30
)

accessibleTables::format_data(wb,
            "ICB",
            c("A", "B", "C", "D"),
            "left",
            "")

accessibleTables::format_data(wb,
            "ICB",
            c("E", "F"),
            "right",
            "#,##0")

accessibleTables::format_data(wb,
            "ICB",
            c("G"),
            "right",
            "#,##0.00")

#### Sex annual
# write data to sheet
# write_sheet(
#   wb,
#   "Sex",
#   paste0(
#     "Hormone replacement therapy - England - 2015/2016 to ",
#     ltst_year,
#     " - Totals by sex"
#   ),
#   c(
#     "1. Field definitions can be found on the 'Metadata' tab.",
#     "2. Statistical disclosure control has been applied to cells containing 5 or fewer patients or items. These cells will appear blank.",
#     "3. It is possible for a patient to be codified with gender 'unknown' or 'indeterminate'. Due to the low number of patients that these two groups contain the NHSBSA has decided to group these classifications together."
#   ),
#   gender_annual,
#   14
# )

# format_data(wb,
#             "Sex",
#             c("A", "B", "C"),
#             "left",
#             "")

# format_data(wb,
#             "Sex",
#             c("D", "E"),
#             "right",
#             "#,##0")

# format_data(wb,
#             "Sex",
#             c("F"),
#             "right",
#             "#,##0.00")

#### Age annual
# write data to sheet
accessibleTables::write_sheet(
  wb,
  "Age_Band",
  paste0(
    "Hormone replacement therapy - England - 2015/2016 to ",
    ltst_year_ytd,
    " - Totals by age band"
  ),
  c(
    "1. Field definitions can be found on the 'Metadata' tab.",
    "2. The figures in this table relate to prescribing of HRT medications in England that are subsequently dispensed in the community in England, Scotland, Wales, Isle of Man or the Channel Islands by a pharmacy, appliance contractor, dispensing doctor, or have been personally administered by a GP practice. They do not include data on medicines used in secondary care, prisons, or issued by a private prescriber."  ),
  ageband_annual,
  30
)

accessibleTables::format_data(wb,
            "Age_Band",
            c("A", "B", "C"),
            "left",
            "")

accessibleTables::format_data(wb,
            "Age_Band",
            c("D", "E"),
            "right",
            "#,##0")

accessibleTables::format_data(wb,
            "Age_Band",
            c("F"),
            "right",
            "#,##0.00")

#### IMD quintile annual
# write data to sheet
accessibleTables::write_sheet(
  wb,
  "IMD_Quintile",
  paste0(
    "Hormone replacement therapy - England - 2015/2016 to ",
    ltst_year_ytd,
    " - Totals by IMD quintile"
  ),
  c(
    "1. Field definitions can be found on the 'Metadata' tab.",
    "2. The figures in this table relate to prescribing of HRT medications in England that are subsequently dispensed in the community in England, Scotland, Wales, Isle of Man or the Channel Islands by a pharmacy, appliance contractor, dispensing doctor, or have been personally administered by a GP practice. They do not include data on medicines used in secondary care, prisons, or issued by a private prescriber.",
    "3. Where a patient's lower-layer super output areas (LSOA) has not been able to to be matched, is not available, or the patient has not been identified the records are reported as 'unknown' IMD decile.",
    "4. Patient LSOA is only available for prescriptions issued via the Electronic Prescription Service (EPS). As this service was not broadly utilised until 2019/20 figures prior to this have been omitted from this table.",
    "5. ONS population estimates taken from https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/populationandmigration/populationestimates/adhocs/13773populationsbyindexofmultipledeprivationimddecileenglandandwales2020/populationbyimdenglandandwales2020.xlsx",
    "6. Figures in this table are only for patients where an NHS number that has been verified by the Personal Demographics Service (PDS) "
  ),
  quintile_annual,
  30
)

accessibleTables::format_data(wb,
            "IMD_Quintile",
            c("A", "B"),
            "left",
            "")

accessibleTables::format_data(wb,
            "IMD_Quintile",
            c("C", "D", "E"),
            "right",
            "#,##0")

accessibleTables::format_data(wb,
            "IMD_Quintile",
            c("F", "G"),
            "right",
            "#,##0.00")

#### IMD quintile/age annual
# write data to sheet
accessibleTables::write_sheet(
  wb,
  "IMD_Quintile_Age",
  paste0(
    "Hormone replacement therapy - England - 2015/2016 to ",
    ltst_year_ytd,
    " - Totals by IMD quintile and age band"
  ),
  c(
    "1. Field definitions can be found on the 'Metadata' tab.",
    "2. The figures in this table relate to prescribing of HRT medications in England that are subsequently dispensed in the community in England, Scotland, Wales, Isle of Man or the Channel Islands by a pharmacy, appliance contractor, dispensing doctor, or have been personally administered by a GP practice. They do not include data on medicines used in secondary care, prisons, or issued by a private prescriber.",
    "3. Where a patient's lower-layer super output areas (LSOA) has not been able to to be matched, is not available, or the patient has not been identified the records are reported as 'unknown' IMD decile.",
    "4. Patient LSOA is only available for prescriptions issued via the Electronic Prescription Service (EPS). As this service was not broadly utilised until 2019/20 figures prior to this have been omitted from this table.",
    "5. ONS population estimates taken from https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/populationandmigration/populationestimates/adhocs/13773populationsbyindexofmultipledeprivationimddecileenglandandwales2020/populationbyimdenglandandwales2020.xlsx",
    "6. Figures in this table are only for patients where an NHS number that has been verified by the Personal Demographics Service (PDS) "
  ),
  quintile_age_annual,
  30
)

accessibleTables::format_data(wb,
            "IMD_Quintile_Age",
            c("A", "B", "C"),
            "left",
            "")

accessibleTables::format_data(wb,
            "IMD_Quintile_Age",
            c("D", "E", "F"),
            "right",
            "#,##0")

accessibleTables::format_data(wb,
            "IMD_Quintile_Age",
            c("G", "H"),
            "right",
            "#,##0.00")

#### charge status annual
# write data to sheet
accessibleTables::write_sheet(
  wb,
  "Charge_Status",
  paste0(
    "Hormone replacement therapy - England - 2015/2016 to ",
    ltst_year_ytd,
    " - Totals by exemption category"
  ),
  c(
    "1. Field definitions can be found on the 'Metadata' tab.",
    "2. The figures in this table relate to prescribing of HRT medications in England that are subsequently dispensed in the community in England, Scotland, Wales, Isle of Man or the Channel Islands by a pharmacy, appliance contractor, dispensing doctor, or have been personally administered by a GP practice. They do not include data on medicines used in secondary care, prisons, or issued by a private prescriber.",
    "3. A charge status is 'Unknown' when an item prescribed in England but has been dispensed in Scotland"),
  charge_annual,
  30
)

accessibleTables::format_data(wb,
            "Charge_Status",
            c("A", "B", "C", "D"),
            "left",
            "")

accessibleTables::format_data(wb,
            "Charge_Status",
            c( "E", "F"),
            "right",
            "#,##0")

format_data(wb,
            "Charge_Status",
            c("G"),
            "right",
            "#,##0.00")

#create cover sheet
accessibleTables::makeCoverSheet(
  "Hormone Replacement Therapy - England April 2015 to June 2023",
  "England - Financial Year Summary Statistics",
  "Publication Date: 26 October 2023",
  wb,
  sheetNames,
  c(
    "Metadata",
    "Table 1: Patient Identification Rates",
    "Table 2: National Total",
    "Table 3: National Population",
    "Table 4: Chemical Substance",
    "Table 5: Presentations",
    "Table 6: SSP",
    "Table 7: ICB",
    "Table 8: Age Band",
    "Table 9: Indices of Deprivation (IMD) Quintile",
    "Table 10: Indices of Deprivation (IMD) Quintile Age",
    "Table 11: Charge Status"
    
  ),
  c("Metadata", sheetNames)
)



#save file into outputs folder
openxlsx::saveWorkbook(wb,
                       paste0(
                         "outputs/hrt_financial_year_",
                         paste0(
                           gsub("/", "_", max(raw_data$national_annual$FINANCIAL_YEAR)),
                           "_YTD_", 
                           gsub(" ", "_", ltst_month_tidy)
                         ),
                         "_v001.xlsx"
                       ),
                       overwrite = TRUE)

# Monthly Excel
# create wb object
# create list of sheetnames needed (overview and metadata created automatically)
sheetNames <- c(
  "Patient_Identification",
  "National_Total",
  "Chemical_Substance",
  "Presentations",
  "SSP",
  "ICB",
  #"Sex",
  "Age_Band",
  "IMD_Quintile",
  "IMD_Quintile_Age",
  "Charge_Status"
)

wb <- accessibleTables::create_wb(sheetNames)

#create metadata tab (will need to open file and auto row heights once ran)
meta_fields <- c(
  "BNF Section Name",
  "BNF Section Code",
  "Identified Patient Flag",
  "Total Identified Patients",
  "Total Items",
  "Total Net Ingredient Cost (GBP)",
  "Financial Year",
  "Year Month",
  "BNF Paragraph Name",
  "BNF Paragraph Code",
  "ICB Name",
  "ICB Code",
  "BNF Chemical Substance Name",
  "BNF Chemical Substance Code",
  "Age Band",
  "IMD Quintile"
)

meta_descs <-
  c(
    "The name given to a British National Formulary (BNF) section. This is the next broadest grouping of The BNF Therapeutical classification system after chapter.",
    "The unique code used to refer to the British National Formulary (BNF) section.",
    "This shows where an item has been attributed to an NHS number that has been verified by the Personal Demographics Service.",
    "Where patients are identified via the flag, the number of patients that the data corresponds to. This will always be 0 where 'Identified Patient' = N.",
    "The number of prescription items dispensed. 'Items' is the number of times a product appears on a prescription form. Prescription forms include both paper prescriptions and electronic messages.",
    "Total Net Ingredient Cost is the amount that would be paid using the basic price of the prescribed drug or appliance and the quantity prescribed. Sometimes called the 'Net Ingredient Cost' (NIC). The basic price is given either in the Drug Tariff or is determined from prices published by manufacturers, wholesalers or suppliers. Basic price is set out in Parts 8 and 9 of the Drug Tariff. For any drugs or appliances not in Part 8, the price is usually taken from the manufacturer, wholesaler or supplier of the product. This is given in GBP (£).",
    "The financial year to which the data belongs.",
    "The year and month to which the data belongs, denoted in YYYYMM format.",
    "The name given to the British National Formular (BNF) paragraph. This level of grouping of the BNF Therapeutical classification system sits below BNF section.",
    "The unique code used to refer to the British National Formulary (BNF) paragraph.",
    "The name given to the Integrated Care Board (ICB) that a prescribing organisation belongs to. This is based upon NHSBSA administrative records, not geographical boundaries and more closely reflect the operational organisation of practices than other geographical data sources.",
    "The unique code used to refer to an Integrated Care Board (ICB).",
    "The name of the main active ingredient in a drug. Appliances do not hold a chemical substance, but instead inherit the corresponding BNF section. Determined by the British National Formulatory (BNF) for drugs, or the NHS BSA for appliances. For example, Amoxicillin.",
    "The unique code used to refer to the British National Formulary (BNF) chemical substance.",
    "The age band of the patient as of the 30th September of the corresponding financial year the drug was prescribed.",
    "The IMD quintile of the patient, based on the patient's postcode, where '1' is the 20% of areas with the highest deprivation score in the Index of Multiple Deprivation (IMD) from the English Indices of Deprivation 2019, and '5' is the 20% of areas with the lowest IMD deprivation score. The IMD quintile has been recorded as 'Unknown' where the items are attributed to an unidentified patient, or where we have been unable to match the patient postcode to a postcode in the National Statistics Postcode Lookup (NSPL)."
  )

accessibleTables::create_metadata(wb,
                                  meta_fields,
                                  meta_descs)

#### Patient identification
# write data to sheet
accessibleTables::write_sheet(
  wb,
  "Patient_Identification",
  paste0(
    "Hormone replacement therapy - England - April 2015 to ",
    ltst_month_tidy,
    " - Proportion of items for which an NHS number was recorded (%)"
  ),
  c(
    "1. Field definitions can be found on the 'Metadata' tab.",
    "2. The figures in this table relate to prescribing of HRT medications in England that are subsequently dispensed in the community in England, Scotland, Wales, Isle of Man or the Channel Islands by a pharmacy, appliance contractor, dispensing doctor, or have been personally administered by a GP practice. They do not include data on medicines used in secondary care, prisons, or issued by a private prescriber.",
    "3. The below proportions reflect the percentage of prescription items where a NHS number was recorded."
  ),
  pi_data_monthly,
  14
)

#left align columns A
format_data(wb,
            "Patient_Identification",
            c("A", "B"),
            "left",
            "")

#right align columns and round to 2 DP
accessibleTables::format_data(wb,
            "Patient_Identification",
            c("c"),
            "right",
            "0.00")

#### National data
# write data to sheet
accessibleTables::write_sheet(
  wb,
  "National_Total",
  paste0(
    "Hormone replacement therapy - England - April 2015 to ",
    ltst_month_tidy,
    " - Monthly totals split by identified patients"
  ),
  c("1. Field definitions can be found on the 'Metadata' tab.",
    "2. The figures in this table relate to prescribing of HRT medications in England that are subsequently dispensed in the community in England, Scotland, Wales, Isle of Man or the Channel Islands by a pharmacy, appliance contractor, dispensing doctor, or have been personally administered by a GP practice. They do not include data on medicines used in secondary care, prisons, or issued by a private prescriber."),
  national_data_monthly,
  14
)

#left align columns A
accessibleTables::format_data(wb,
            "National_Total",
            c("A", "B", "C"),
            "left",
            "")

#right align columns and round to 2 DP
accessibleTables::format_data(wb,
            "National_Total",
            c("D", "E"),
            "right",
            "#,##0")

accessibleTables::format_data(wb,
            "National_Total",
            c("F"),
            "right",
            "#,##0.00")

# #### Paragraph monthly
# # write data to sheet
# accessibleTables::write_sheet(
#   wb,
#   "BNF_Paragraph",
#   paste0(
#     "Hormone replacement therapy - England - April 2015 to ",
#     ltst_month_tidy,
#     " - Monthly totals split by BNF paragraph and identified patients"
#   ),
#   c(
#     "1. Field definitions can be found on the 'Metadata' tab.",
#     "2. The figures in this table relate to prescribing of HRT medications in England that are subsequently dispensed in the community in England, Scotland, Wales, Isle of Man or the Channel Islands by a pharmacy, appliance contractor, dispensing doctor, or have been personally administered by a GP practice. They do not include data on medicines used in secondary care, prisons, or issued by a private prescriber."  ),
#   paragraph_monthly,
#   14
# )
# 
# accessibleTables::format_data(wb,
#             "BNF_Paragraph",
#             c("A", "B", "C", "D", "E", "F", "G"),
#             "left",
#             "")
# 
# accessibleTables::format_data(wb,
#             "BNF_Paragraph",
#             c("H", "I"),
#             "right",
#             "#,##0")
# 
# accessibleTables::format_data(wb,
#             "BNF_Paragraph",
#             c("J"),
#             "right",
#             "#,##0.00")

#### Chemical substance monthly
# write data to sheet
accessibleTables::write_sheet(
  wb,
  "Chemical_Substance",
  paste0(
    "Hormone replacement therapy - England - April 2015 to ",
    ltst_month_tidy,
    " - Monthly totals split by chemical substance and identified patients"
  ),
  c(
    "1. Field definitions can be found on the 'Metadata' tab.",
    "2. The figures in this table relate to prescribing of HRT medications in England that are subsequently dispensed in the community in England, Scotland, Wales, Isle of Man or the Channel Islands by a pharmacy, appliance contractor, dispensing doctor, or have been personally administered by a GP practice. They do not include data on medicines used in secondary care, prisons, or issued by a private prescriber."  ),
  chem_sub_monthly,
  14
)

accessibleTables::format_data(wb,
            "Chemical_Substance",
            c("A", "B", "C", "D", "E", "F", "G", "H", "I"),
            "left",
            "")

accessibleTables::format_data(wb,
            "Chemical_Substance",
            c("J", "K"),
            "right",
            "#,##0")

accessibleTables::format_data(wb,
            "Chemical_Substance",
            c("L"),
            "right",
            "#,##0.00")

#### presentations monthly
# write data to sheet
accessibleTables::write_sheet(
  wb,
  "Presentations",
  paste0(
    "Hormone replacement therapy - England - April 2015 to ",
    ltst_month_tidy,
    " - Monthly totals split by presentation"
  ),
  c(
    "1. Field definitions can be found on the 'Metadata' tab.",
    "2. The figures in this table relate to prescribing of HRT medications in England that are subsequently dispensed in the community in England, Scotland, Wales, Isle of Man or the Channel Islands by a pharmacy, appliance contractor, dispensing doctor, or have been personally administered by a GP practice. They do not include data on medicines used in secondary care, prisons, or issued by a private prescriber.",
    "3. For medications which are dispensed in Scotland no unit of measure is captured, this has been imputed from the data on medicines dispensed elsewhere"),
  presentation_monthly,
  14
)

accessibleTables::format_data(
  wb,
  "Presentations",
  c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M"),
  "left",
  ""
)

accessibleTables::format_data(wb,
            "Presentations",
            c("N", "O"),
            "right",
            "#,##0")

accessibleTables::format_data(wb,
            "Presentations",
            c("P", "Q", "R", "S"),
            "right",
            "#,##0.00")

#### ssp monthly
# write data to sheet
accessibleTables::write_sheet(
  wb,
  "SSP",
  paste0(
    "Hormone replacement therapy - England - April 2015 to ",
    ltst_month_tidy,
    " - Monthly totals for precribing which has been issued under Serious Shortage Protocols (SSP) split by presentation"
  ),
  c(
    "1. Field definitions can be found on the 'Metadata' tab.",
    "2. The figures in this table relate to prescribing of HRT medications in England that are subsequently dispensed in the community in England, Scotland, Wales, Isle of Man or the Channel Islands by a pharmacy, appliance contractor, dispensing doctor, or have been personally administered by a GP practice. They do not include data on medicines used in secondary care, prisons, or issued by a private prescriber.",
    "3. These figures will be included as part of the totals on the 'Presentations' tab."
  ),
  ssp_monthly,
  14
)

accessibleTables::format_data(
  wb,
  "SSP",
  c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M"),
  "left",
  ""
)

accessibleTables::format_data(wb,
            "SSP",
            c("N", "O"),
            "right",
            "#,##0")

accessibleTables::format_data(wb,
            "SSP",
            c("P", "Q", "R", "S"),
            "right",
            "#,##0.00")

#### ICB monthly
# write data to sheet
accessibleTables::write_sheet(
  wb,
  "ICB",
  paste0(
    "Hormone replacement therapy - England - April 2015 to ",
    ltst_month_tidy,
    " - Monthly totals split by ICB"
  ),
  c(
    "1. Field definitions can be found on the 'Metadata' tab.",
    "2. The figures in this table relate to prescribing of HRT medications in England that are subsequently dispensed in the community in England, Scotland, Wales, Isle of Man or the Channel Islands by a pharmacy, appliance contractor, dispensing doctor, or have been personally administered by a GP practice. They do not include data on medicines used in secondary care, prisons, or issued by a private prescriber."  ),
  icb_monthly,
  14
)

accessibleTables::format_data(wb,
            "ICB",
            c("A", "B", "C", "D", "E"),
            "left",
            "")

accessibleTables::format_data(wb,
            "ICB",
            c("F", "G"),
            "right",
            "#,##0")

accessibleTables::format_data(wb,
            "ICB",
            c("H"),
            "right",
            "#,##0.00")

#### Sex monthly
# write data to sheet
# write_sheet(
#   wb,
#   "Sex",
#   paste0(
#     "Hormone replacement therapy - England - April 2015 to ",
#     ltst_month_tidy,
#     " - Monthly totals by sex"
#   ),
#   c(
#     "1. Field definitions can be found on the 'Metadata' tab.",
#     "2. Statistical disclosure control has been applied to cells containing 5 or fewer patients or items. These cells will appear blank.",
#     "3. It is possible for a patient to be codified with gender 'unknown' or 'indeterminate'. Due to the low number of patients that these two groups contain the NHSBSA has decided to group these classifications together."
#   ),
#   gender_monthly,
#   14
# )

# format_data(wb,
#             "Sex",
#             c("A", "B", "C", "D"),
#             "left",
#             "")

# format_data(wb,
#             "Sex",
#             c("E", "F"),
#             "right",
#             "#,##0")

# format_data(wb,
#             "Sex",
#             c("G"),
#             "right",
#             "#,##0.00")

#### Age monthly
# write data to sheet
accessibleTables::write_sheet(
  wb,
  "Age_Band",
  paste0(
    "Hormone replacement therapy - England - April to ",
    ltst_month_tidy,
    " - Monthly totals by age band"
  ),
  c(
    "1. Field definitions can be found on the 'Metadata' tab.",
    "2. The figures in this table relate to prescribing of HRT medications in England that are subsequently dispensed in the community in England, Scotland, Wales, Isle of Man or the Channel Islands by a pharmacy, appliance contractor, dispensing doctor, or have been personally administered by a GP practice. They do not include data on medicines used in secondary care, prisons, or issued by a private prescriber."  ),
  ageband_monthly,
  14
)

accessibleTables::format_data(wb,
            "Age_Band",
            c("A", "B", "C", "D"),
            "left",
            "")

accessibleTables::format_data(wb,
            "Age_Band",
            c("E", "F"),
            "right",
            "#,##0")

accessibleTables::format_data(wb,
            "Age_Band",
            c("G"),
            "right",
            "#,##0.00")

#### IMD quintile monthly
# write data to sheet
accessibleTables::write_sheet(
  wb,
  "IMD_Quintile",
  paste0(
    "Hormone replacement therapy - England - April 2015 to ",
    ltst_month_tidy,
    " - Monthly totals by IMD quintile"
  ),
  c(
    "1. Field definitions can be found on the 'Metadata' tab.",
    "2. The figures in this table relate to prescribing of HRT medications in England that are subsequently dispensed in the community in England, Scotland, Wales, Isle of Man or the Channel Islands by a pharmacy, appliance contractor, dispensing doctor, or have been personally administered by a GP practice. They do not include data on medicines used in secondary care, prisons, or issued by a private prescriber.",
    "3. Where a patient's lower-layer super output areas (LSOA) has not been able to to be matched, is not available, or the patient has not been identified the records are reported as 'unknown' IMD decile.",
    "4. Patient LSOA is only available for prescriptions issued via the Electronic Prescription Service (EPS). As this service was not broadly utilised until 2019/20 figures prior to this have been omitted from this table.",
    "5. Figures in this table are only for patients where an NHS number that has been verified by the Personal Demographics Service (PDS) "
  ),
  quintile_monthly,
  14
)

accessibleTables::format_data(wb,
            "IMD_Quintile",
            c("A", "B", "C"),
            "left",
            "")

accessibleTables::format_data(wb,
            "IMD_Quintile",
            c("D", "E"),
            "right",
            "#,##0")

accessibleTables::format_data(wb,
            "IMD_Quintile",
            c("F"),
            "right",
            "#,##0.00")

#### IMD quintile/age monthly
# write data to sheet
accessibleTables::write_sheet(
  wb,
  "IMD_Quintile_Age",
  paste0(
    "Hormone replacement therapy - England - April 2015 to ",
    ltst_month_tidy,
    " - Monthly totals by IMD quintile and age band"
  ),
  c(
    "1. Field definitions can be found on the 'Metadata' tab.",
    "2. The figures in this table relate to prescribing of HRT medications in England that are subsequently dispensed in the community in England, Scotland, Wales, Isle of Man or the Channel Islands by a pharmacy, appliance contractor, dispensing doctor, or have been personally administered by a GP practice. They do not include data on medicines used in secondary care, prisons, or issued by a private prescriber.",
    "3. Where a patient's lower-layer super output areas (LSOA) has not been able to to be matched, is not available, or the patient has not been identified the records are reported as 'unknown' IMD decile.",
    "4. Patient LSOA is only available for prescriptions issued via the Electronic Prescription Service (EPS). As this service was not broadly utilised until 2019/20 figures prior to this have been omitted from this table.",
    "5. Figures in this table are only for patients where an NHS number that has been verified by the Personal Demographics Service (PDS) "
  ),
  quintile_age_monthly,
  14
)

accessibleTables::format_data(wb,
            "IMD_Quintile_Age",
            c("A", "B", "C", "D"),
            "left",
            "")

accessibleTables::format_data(wb,
            "IMD_Quintile_Age",
            c("E", "F"),
            "right",
            "#,##0")

accessibleTables::format_data(wb,
            "IMD_Quintile_Age",
            c("G"),
            "right",
            "#,##0.00")

#### Charge monthly
# write data to sheet
accessibleTables::write_sheet(
  wb,
  "Charge_Status",
  paste0(
    "Hormone replacement therapy - England - 2015/2016 to ",
    ltst_year,
    " - Totals by exemption category"
  ),
  c(
    "1. Field definitions can be found on the 'Metadata' tab.",
    "2. The figures in this table relate to prescribing of HRT medications in England that are subsequently dispensed in the community in England, Scotland, Wales, Isle of Man or the Channel Islands by a pharmacy, appliance contractor, dispensing doctor, or have been personally administered by a GP practice. They do not include data on medicines used in secondary care, prisons, or issued by a private prescriber.",
    "5. A charge status is 'Unknown' when an item prescribed in England but has been dispensed in Scotland"),
  charge_monthly,
  14
)

accessibleTables::format_data(wb,
            "Charge_Status",
            c("A", "B", "C", "D", "E"),
            "left",
            "")

accessibleTables::format_data(wb,
            "Charge_Status",
            c( "F", "G" ),
            "right",
            "#,##0")

accessibleTables::format_data(wb,
            "Charge_Status",
            c("H"),
            "right",
            "#,##0.00")

#create cover sheet
accessibleTables::makeCoverSheet(
  "Hormone Replacement Therapy - England April 2015 to June 2023",
  "England - Monthly Summary Statistics",
  "Publication Date: 26 October 2023",
  wb,
  sheetNames,
  c(
    "Metadata",
    "Table 1: Patient Identification Rates",
    "Table 2: National Total",
    "Table 3: Chemical Substance",
    "Table 4: Presentations",
    "Table 5: SSP",
    "Table 6: ICB",
    "Table 7: Age Band",
    "Table 8: Indices of Deprivation (IMD) Quintile",
    "Table 9: Indices of Deprivation (IMD) Quintile Age",
    "Table 10: Charge Status"
    
  ),
  c("Metadata", sheetNames)
)  
#save file into outputs folder
openxlsx::saveWorkbook(wb,
                       paste0(
                         "outputs/hrt_monthly_",
                         gsub(" ", "_", ltst_month_tidy),
                         "_v001.xlsx"
                       ),
                       overwrite = TRUE)
#Management Information Exemption Excel

# FY Excel
# create wb object
# create list of sheetnames needed (overview and metadata created automatically)
sheetNames <- c(
  "Exemption_Categories",
  "Exemption_Categories_Monthly"
)

wb <- accessibleTables::create_wb(sheetNames)

#create metadata tab (will need to open file and auto row heights once ran)
meta_fields <- c(
  "Financial Year",
  "Year Month",
  "Exemption category code",
  "Exemption category",
  "Identified Patient Flag",
  "Total Identified Patients",
  "Total Items",
  "Total Net Ingredient Cost (GBP)"
  
  
)

#Appendix Exemption Excel


# create wb object
# create list of sheetnames needed (overview and metadata created automatically)
sheetNames <- c(
  "HRT_Drug_list"
)

wb <- accessibleTables::create_wb(sheetNames)

#create metadata tab (will need to open file and auto row heights once ran)
meta_fields <- c(
  "BNF Presentation Code",
  "BNF Presentation Name"
  
  
)
meta_descs <-
  c(
    "	The unique code used to refer to the British National Formulary (BNF) presentation.",
  	"The name given to the specific type, strength, and formulation of a drug; or, the specific type of an appliance. For example, Paracetamol 500mg tablets."  )

accessibleTables::create_metadata(wb,
                                  meta_fields,
                                  meta_descs)


#### Appendix Exemption
# write data to sheet
accessibleTables::write_sheet(
  wb,
  "HRT_Drug_list",
  paste0(
    "Hormone replacement therapy - England - 2015/2016 to ",
    ltst_year_ytd,
    " - Appendix A - included drugs"
  ),
  c(
    "1. Field definitions can be found on the 'Metadata' tab.",
    "2. The presentations in this table are those covered by the HRT PPC as licensed to treat menopause in the UK."
),
  hrt_drug,
  30
)



#create cover sheet
accessibleTables::makeCoverSheet(
  "Hormone Replacement Therapy - England April 2015 to June 2023",
  "Management Information Tables - Appendix A - included drugs",
  "Publication Date: 26 October 2023",
  wb,
  sheetNames,
  c(
    "Metadata",
    "Table 1: HRT Drug list"
   
   
    
  ),
  c("Metadata", sheetNames)
)  
#save file into outputs folder
openxlsx::saveWorkbook(wb,
                       paste0(
                         "outputs/hrt_appendix_a_",
                         gsub(" ", "_", ltst_month_tidy),
                         "_v001.xlsx"
                       ),
                       overwrite = TRUE)

# 7. chart data for narrative-----------------------------------------------

### Figure 1: Number of prescribed items and estimated identified patients by financial year 

figure_1_data <- raw_data$national_annual %>%
  dplyr::mutate(
    FINANCIAL_YEAR = case_when(
      FINANCIAL_YEAR == max(FINANCIAL_YEAR) ~ paste0(FINANCIAL_YEAR, "<br>(YTD ", ltst_month_tidy, ")"),
      TRUE ~ FINANCIAL_YEAR
    )
  ) %>%
  dplyr::group_by(FINANCIAL_YEAR) %>% 
  dplyr::summarise(`Prescribed items` = sum(ITEM_COUNT),
                   `Identified patients` = sum(PATIENT_COUNT),
                   .groups = "drop") %>% 
  tidyr::pivot_longer(cols = c(`Identified patients`,`Prescribed items`),
                      names_to = "measure",
                      values_to = "value") %>% 
  dplyr::mutate(value = signif(value, 3)) %>% 
  dplyr::arrange(desc(measure))


figure_1 <-  figure_1_data  %>%
  nhsbsaVis::group_chart_hc(
    x = FINANCIAL_YEAR,
    y = value,
    group = measure,
    type = "line",
    marker = FALSE,
    dlOn = FALSE,
    xLab = "Financial year",
    yLab = "Number of prescription items/identified patients",
    title = ""
  ) %>% 
  hc_tooltip(enabled = TRUE,
             shared = TRUE,
             sort = TRUE) %>% 
  hc_legend(enabled = TRUE)%>%
  hc_xAxis(
    plotLines = list(
      list(
        value = 7,
        color = "grey",
        width = 1,
        dashStyle = "dash",
        label = list(
          rotation = 0,
          text = "<b>Note:</b> The figures <br>beyond this point <br>represent<br>prescribing in <br>April 2023 to <br>June 2023 only",
          style = list(
            fontSize = "10px"
          )
        )
      ))
  )



### Figure 2: Number of prescribed items and estimated identified patients by month {.toc-ignore}

figure_2_data <-  raw_data$national_monthly %>%
  filter(YEAR_MONTH >= lt_st_month_min) %>%
  dplyr::group_by(YEAR_MONTH) %>% 
  dplyr::summarise(`Prescribed items` = sum(ITEM_COUNT),
                   `Identified patients` = sum(PATIENT_COUNT),
                   .groups = "drop") %>% 
  tidyr::pivot_longer(cols = c(`Identified patients`,`Prescribed items`),
                      names_to = "measure",
                      values_to = "value") %>% 
  dplyr::mutate(value = signif(value, 3)) %>% 
  dplyr::arrange(desc(measure)) %>%
  mutate(
    YEAR_MONTH = base::as.Date(as.character(paste0(YEAR_MONTH,"01")), format = "%Y%m%d")
  ) 

figure_2 <- figure_2_data |>    nhsbsaVis::group_chart_hc(
  x = YEAR_MONTH,
  y = value,
  group = measure,
  type = "line",
  marker = FALSE,
  dlOn = FALSE,
  xLab = "Month",
  yLab = "Number of prescription items/identified patients",
  title = ""
) %>% 
  hc_tooltip(enabled = TRUE,
             shared = TRUE,
             sort = TRUE) %>% 
  hc_legend(enabled = TRUE)%>%
  hc_xAxis(type = "datetime") %>%
  hc_xAxis(
    plotLines = list(
      list(
        value = datetime_to_timestamp(as.Date('2023-04-01', tz = 'UTC')),
        color = "grey",
        width = 1,
        dashStyle = "dash",
        label = list(
          rotation = 0,
          text = "<b>Note:</b><br> The HRT <br>PPC was <br>introduced.",
          style = list(
            fontSize = "8px"
          )
        )
      ))
  )
figure_2

### Figure 3: Number of prescribed items and estimated identified patients by financial year and BNF paragraph {.toc-ignore}
figure_3_data <- raw_data$national_par_annual %>%
  dplyr::mutate(
    FINANCIAL_YEAR = case_when(
      FINANCIAL_YEAR == max(FINANCIAL_YEAR) ~ paste0(FINANCIAL_YEAR, "<br>(YTD ", ltst_month_tidy, ")"),
      TRUE ~ FINANCIAL_YEAR
    )
  ) %>%
  filter(BNF_PARAGRAPH %in% c("060401","070201"),FINANCIAL_YEAR %!in% c("2015/2016", "2016/2017","2017/2018")) %>%
  group_by(FINANCIAL_YEAR,BNF_PARAGRAPH) %>%
  dplyr::summarise(`Prescribed items` = sum(ITEM_COUNT),
                   `Identified patients` = sum(PATIENT_COUNT),
                   .groups = "drop") %>% 
  tidyr::pivot_longer(cols = c(`Identified patients`,`Prescribed items`),
                      names_to = "measure",
                      values_to = "value") %>% 
  dplyr::mutate(value = signif(value, 3)) %>% 
  dplyr::arrange(desc(measure))

### Part 1


figure_3_pt_1_data <- raw_data$national_par_annual %>%
  dplyr::mutate(
    FINANCIAL_YEAR = case_when(
      FINANCIAL_YEAR == max(FINANCIAL_YEAR) ~ paste0(FINANCIAL_YEAR, "<br>(YTD ", ltst_month_tidy, ")"),
      TRUE ~ FINANCIAL_YEAR
    )
  ) %>%
  filter(BNF_PARAGRAPH == "060401",FINANCIAL_YEAR %!in% c("2015/2016", "2016/2017","2017/2018")) %>%
  group_by(FINANCIAL_YEAR) %>%
  dplyr::summarise(`Prescribed items` = sum(ITEM_COUNT),
                   `Identified patients` = sum(PATIENT_COUNT),
                   .groups = "drop") %>% 
  tidyr::pivot_longer(cols = c(`Identified patients`,`Prescribed items`),
                      names_to = "measure",
                      values_to = "value") %>% 
  dplyr::mutate(value = signif(value, 3)) %>% 
  dplyr::arrange(desc(measure))
#,FINANCIAL_YEAR %!in% c("2015/2016", "2016/2017","2017/2018")
figure_3_pt_1 <- figure_3_pt_1_data %>%
   nhsbsaVis::group_chart_hc(
    x = FINANCIAL_YEAR,
    y = value,
    group = measure,
    type = "line",
    marker = FALSE,
    dlOn = FALSE,
    xLab = "Financial year",
    yLab = "Number of prescription items/identified patients",
    title = "Female sex hormones and their modulators"
  ) %>% 
  hc_tooltip(enabled = TRUE,
             shared = TRUE,
             sort = TRUE) %>% 
  hc_legend(enabled = TRUE) %>%
  hc_title(align = "left") %>%
  hc_yAxis(max = max(figure_3_pt_1_data$value)) %>%
  hc_xAxis(
    plotLines = list(
      list(
        value = 4,
        color = "grey",
        width = 1,
        dashStyle = "dash",
        label = list(
          rotation = 0,
          text = "<b>Note:</b><br> The figures <br>beyond this <br>point <br>represent<br>prescribing in <br>April 2023 to <br>June 2023<br> only",
          style = list(
            fontSize = "8px"
          )
        )
      ))
  )


### Part 2


figure_3_pt_2_data <- raw_data$national_par_annual %>%
  dplyr::mutate(
    FINANCIAL_YEAR = case_when(
      FINANCIAL_YEAR == max(FINANCIAL_YEAR) ~ paste0(FINANCIAL_YEAR, "<br>(YTD ", ltst_month_tidy, ")"),
      TRUE ~ FINANCIAL_YEAR
    )
  ) %>%
  filter(BNF_PARAGRAPH == "070201",FINANCIAL_YEAR %!in% c("2015/2016", "2016/2017","2017/2018")) %>%
  group_by(FINANCIAL_YEAR) %>%
  dplyr::summarise(`Prescribed items` = sum(ITEM_COUNT),
                   `Identified patients` = sum(PATIENT_COUNT),
                   .groups = "drop") %>% 
  tidyr::pivot_longer(cols = c(`Identified patients`,`Prescribed items`),
                      names_to = "measure",
                      values_to = "value") %>% 
  dplyr::mutate(value = signif(value, 3)) %>% 
  dplyr::arrange(desc(measure))

figure_3_pt_2 <- figure_3_pt_2_data %>%
   nhsbsaVis::group_chart_hc(
    x = FINANCIAL_YEAR,
    y = value,
    group = measure,
    type = "line",
    marker = FALSE,
    dlOn = FALSE,
    xLab = "Financial year",
    yLab = "Number of prescription items/identified patients",
    title = "Preparations for vaginal and vulval change"
  ) %>% 
  hc_tooltip(enabled = TRUE,
             shared = TRUE,
             sort = TRUE) %>% 
  hc_legend(enabled = TRUE)%>%
  hc_title(align = "left") %>%
  hc_yAxis(max = max(figure_3_pt_1_data$value)) %>%
  hc_xAxis(
    plotLines = list(
      list(
        value = 4,
        color = "grey",
        width = 1,
        dashStyle = "dash",
        label = list(
          rotation = 0,
          text = "<b>Note:</b><br> The figures <br>beyond this <br>point  <br>represent<br>prescribing in <br>April 2023 to <br>June 2023<br> only",
          style = list(
            fontSize = "8px"
          )
        )
      ))
  )


#part 3
figure_3_pt_3_data <- raw_data$national_par_annual %>%
  dplyr::mutate(
    FINANCIAL_YEAR = case_when(
      FINANCIAL_YEAR == max(FINANCIAL_YEAR) ~ paste0(FINANCIAL_YEAR, "<br>(YTD ", ltst_month_tidy, ")"),
      TRUE ~ FINANCIAL_YEAR
    )
  ) %>%
  filter(BNF_PARAGRAPH == "060403") %>%
  group_by(FINANCIAL_YEAR) %>%
  dplyr::summarise(`Prescribed items` = sum(ITEM_COUNT),
                   `Identified patients` = sum(PATIENT_COUNT),
                   .groups = "drop") %>% 
  tidyr::pivot_longer(cols = c(`Identified patients`,`Prescribed items`),
                      names_to = "measure",
                      values_to = "value") %>% 
  dplyr::mutate(value = signif(value, 3)) %>% 
  dplyr::arrange(desc(measure))

figure_3_pt_3 <- figure_3_pt_3_data %>%
   nhsbsaVis::group_chart_hc(
    x = FINANCIAL_YEAR,
    y = value,
    group = measure,
    type = "line",
    marker = FALSE,
    dlOn = FALSE,
    xLab = "Financial year",
    yLab = "Number of prescription items/identified patients",
    title = "BNF Paragraph - 060403"
  ) %>% 
  hc_tooltip(enabled = TRUE,
             shared = TRUE,
             sort = TRUE) %>% 
  hc_legend(enabled = TRUE) %>%
  hc_title(align = "left") %>%
  hc_yAxis(max = max(figure_3_pt_3_data$value)) %>%
  hc_xAxis(
    plotLines = list(
      list(
        value = 3,
        color = "grey",
        width = 1,
        dashStyle = "dash",
        label = list(
          rotation = 0,
          text = "<b>Note:</b> The figures <br>beyond this point <br>represent<br>prescribing in <br>April 2023 to <br>June 2023 only",
          style = list(
            fontSize = "8px"
          )
        )
      ))
  )

#part 4
figure_3_pt_4_data <- raw_data$national_par_annual %>%
  dplyr::mutate(
    FINANCIAL_YEAR = case_when(
      FINANCIAL_YEAR == max(FINANCIAL_YEAR) ~ paste0(FINANCIAL_YEAR, "<br>(YTD ", ltst_month_tidy, ")"),
      TRUE ~ FINANCIAL_YEAR
    )
  ) %>%
  filter(BNF_PARAGRAPH == "070302") %>%
  group_by(FINANCIAL_YEAR) %>%
  dplyr::summarise(`Prescribed items` = sum(ITEM_COUNT),
                   `Identified patients` = sum(PATIENT_COUNT),
                   .groups = "drop") %>% 
  tidyr::pivot_longer(cols = c(`Identified patients`,`Prescribed items`),
                      names_to = "measure",
                      values_to = "value") %>% 
  dplyr::mutate(value = signif(value, 3)) %>% 
  dplyr::arrange(desc(measure))

figure_3_pt_4 <- figure_3_pt_4_data %>%
   nhsbsaVis::group_chart_hc(
    x = FINANCIAL_YEAR,
    y = value,
    group = measure,
    type = "line",
    marker = FALSE,
    dlOn = FALSE,
    xLab = "Financial year",
    yLab = "Number of prescription items/identified patients",
    title = "BNF Paragraph - 070302"
  ) %>% 
  hc_tooltip(enabled = TRUE,
             shared = TRUE,
             sort = TRUE) %>% 
  hc_legend(enabled = TRUE) %>%
  hc_title(align = "left") %>%
  hc_yAxis(max = max(figure_3_pt_4_data$value)) %>%
  hc_xAxis(
    plotLines = list(
      list(
        value = 7,
        color = "grey",
        width = 1,
        dashStyle = "dash",
        label = list(
          rotation = 0,
          text = "<b>Note:</b> The figures <br>beyond this point <br>represent<br>prescribing in <br>April 2023 to <br>June 2023 only",
          style = list(
            fontSize = "8px"
          )
        )
      ))
  )


### Figure 4: Estimated Number of identified patients by financial year for selected BNF chemical substances {.toc-ignore}

figure_4_data <- raw_data$chem_sub_annual %>%
  dplyr::mutate(
    FINANCIAL_YEAR = case_when(
      FINANCIAL_YEAR == max(FINANCIAL_YEAR) ~ paste0(FINANCIAL_YEAR, "<br>(YTD ", ltst_month_tidy, ")"),
      TRUE ~ FINANCIAL_YEAR
    )
  ) %>%
  filter(
    CHEMICAL_SUBSTANCE_BNF_DESCR %in% c("Estradiol", "Estriol", "Estradiol with progestogen", "Progesterone", "Norethisterone")
  ) %>%
  mutate(CHEM_SUB = paste0(CHEMICAL_SUBSTANCE_BNF_DESCR, " - ", BNF_CHEMICAL_SUBSTANCE)) %>%
  select(1, 12, 9) %>%
  filter(
    PATIENT_COUNT > 0
  ) %>%
  mutate(value = signif(PATIENT_COUNT, 3))

figure_4 <- figure_4_data %>%
   nhsbsaVis::group_chart_hc(
    x = FINANCIAL_YEAR,
    y = value,
    group = CHEM_SUB,
    type = "line",
    marker = FALSE,
    dlOn = FALSE,
    xLab = "Financial year",
    yLab = "Number of identified patients",
    title = ""
  ) %>% 
  hc_tooltip(enabled = TRUE,
             shared = TRUE,
             sort = TRUE) %>% 
  hc_legend(enabled = TRUE)%>%
  hc_title(align = "left") %>%
  hc_xAxis(
    plotLines = list(
      list(
        value = 7,
        color = "grey",
        width = 1,
        dashStyle = "dash",
        label = list(
          rotation = 0,
          text = "<b>Note:</b> The figures <br>beyond this point <br>represent<br>prescribing in <br>April 2023 to <br>June 2023 only",
          style = list(
            fontSize = "10px"
          )
        )
      )))
  


### Figure 5: Number of prescribed items by financial year for selected BNF chemical substances {.toc-ignore}

figure_5_data <- raw_data$chem_sub_annual %>%
  dplyr::mutate(
    FINANCIAL_YEAR = case_when(
      FINANCIAL_YEAR == max(FINANCIAL_YEAR) ~ paste0(FINANCIAL_YEAR, "<br>(YTD ", ltst_month_tidy, ")"),
      TRUE ~ FINANCIAL_YEAR
    )
  ) %>%
  filter(
    CHEMICAL_SUBSTANCE_BNF_DESCR %in% c("Estradiol", "Estriol", "Estradiol with progestogen", "Progesterone", "Norethisterone")
  ) %>%
  mutate(CHEM_SUB = paste0(CHEMICAL_SUBSTANCE_BNF_DESCR, " - ", BNF_CHEMICAL_SUBSTANCE)) %>%
  filter(CHEM_SUB != "Estriol - 0604011M0") %>%
  select(1, 12, 10) %>%
  group_by(FINANCIAL_YEAR, CHEM_SUB) %>%
  summarise(total = sum(ITEM_COUNT, na.rm = T))%>%
  mutate(total = signif(total, 3))



figure_5 <- figure_5_data %>%
   nhsbsaVis::group_chart_hc(
    x = FINANCIAL_YEAR,
    y = total,
    group = CHEM_SUB,
    type = "line",
    marker = FALSE,
    dlOn = FALSE,
    xLab = "Financial year",
    yLab = "Number of prescription items",
    title = ""
  ) %>% 
  hc_tooltip(enabled = TRUE,
             shared = TRUE,
             sort = TRUE) %>% 
  hc_legend(enabled = TRUE)%>%
  hc_title(align = "left") %>%
  hc_xAxis(
    plotLines = list(
      list(
        value = 7,
        color = "grey",
        width = 1,
        dashStyle = "dash",
        label = list(
          rotation = 0,
          text = "<b>Note:</b> The figures <br>beyond this point <br>represent<br>prescribing in <br>April 2023 to <br>June 2023 only",
          style = list(
            fontSize = "10px"
          )
        )
      ))
  )

### Figure 6: Estimated number of identified patients per 1,000 population by Integrated Care Board (ICB) in `r ltst_year` {.toc-ignore}


figure_6_data <- raw_data$icb_annual %>%
  filter(
    FINANCIAL_YEAR == ltst_year,
    PATIENT_COUNT != 0
  ) %>%
  left_join(
    icb_pop,
    by = c("ICB_CODE" = "ICB_CODE")
  ) %>%
  #select(1, 4,5,7,12) %>%
  mutate(
    PER_1000 = round(PATIENT_COUNT / POP * 1000, 1)
  ) %>%
  rename(ICB_NAME = ICB_NAME.x) %>% 
  arrange(desc(PER_1000)) %>%
  filter(
    ICB_NAME != "UNKNOWN ICB"
  )%>%
  select(-c(ICB_NAME.y, ICB_LONG_CODE))

figure_6 <-   nhsbsaVis::basic_chart_hc(
  figure_6_data,
  x = ICB_CODE,
  y = PER_1000,
  type = "column",
  xLab = "ICB code",
  yLab = "Patients per 1,000 population",
  title = ""
) %>%
  hc_tooltip(enabled = T,
             useHTML = TRUE,
             formatter = JS("function(){
                            var result = this.point.ICB_NAME + '<br><b>Patients per 1,000 population:</b> ' + this.point.PER_1000.toFixed(0)
                            return result
             }")) %>%
  hc_yAxis(
    labels = list(
      enabled = T
    )
  )
figure_6$x$hc_opts$series[[1]]$dataLabels$enabled  <- FALSE

### Figure 7: Estimated number of identified patients by age band in `r ltst_year` {.toc-ignore}

figure_7_data <- raw_data$ageband_annual %>%
  filter(
    FINANCIAL_YEAR == ltst_year
  ) %>%
  filter(PATIENT_COUNT != 0)


figure_7 <-  nhsbsaVis::basic_chart_hc(
  figure_7_data,
  x = AGE_BAND,
  y = PATIENT_COUNT,
  type = "column",
  xLab = "Age band",
  yLab = "Patients",
  title = ""
) %>%
  hc_tooltip(enabled = F)


figure_7$x$hc_opts$series[[1]]$dataLabels$allowOverlap <- TRUE
figure_7

### Figure 8: Estimated number of identified patients by IMD quintile in `r ltst_year` {.toc-ignore}


figure_8_data <- raw_data$quintile_annual %>%
  filter(
    FINANCIAL_YEAR == ltst_year
  ) %>%
  filter(PATIENT_COUNT != 0,
         !is.na(IMD_QUINTILE)) %>%
  mutate(
    IMD_QUINTILE = case_when(
      IMD_QUINTILE == 1 ~ as.character("1 - most deprived"),
      IMD_QUINTILE == 5 ~ as.character("5 - least deprived"),
      TRUE ~ as.character(IMD_QUINTILE)
    )
  )


figure_8 <-  nhsbsaVis::basic_chart_hc(
  figure_8_data,
  x = IMD_QUINTILE,
  y = PATIENT_COUNT,
  type = "column",
  xLab = "IMD quintile",
  yLab = "Patients",
  title = ""
) %>%
  hc_tooltip(enabled = F)


figure_8$x$hc_opts$series[[1]]$dataLabels$allowOverlap <- TRUE
figure_8

### Figure 9: Proportion of patients under/over 60 by IMD quintile in `r ltst_year` {.toc-ignore}


figure_9_data <- raw_data$quintile_age_annual %>%
  filter(
    FINANCIAL_YEAR == ltst_year
  ) %>%
  filter(PATIENT_COUNT != 0,
         !is.na(IMD_QUINTILE),
         AGE_BAND != "Unknown") %>%
  mutate(
    GROUP = case_when(
      AGE_BAND <= "55-59" ~ "Under 60",
      TRUE ~ "Over 60"
    )
  ) %>%
  group_by(
    GROUP,
    IMD_QUINTILE
  ) %>%
  summarise(
    PATIENT_COUNT = sum(PATIENT_COUNT),
    .groups = "drop"
  ) %>%
  group_by(
    IMD_QUINTILE
  ) %>%
  mutate(
    PROP = PATIENT_COUNT / sum(PATIENT_COUNT) * 100
  ) %>%
  ungroup() %>%
  mutate(
    IMD_QUINTILE = case_when(
      IMD_QUINTILE == 1 ~ as.character("1 - most deprived"),
      IMD_QUINTILE == 5 ~ as.character("5 - least deprived"),
      TRUE ~ as.character(IMD_QUINTILE)
    )
  ) %>%
  arrange(
    desc(GROUP)
  )


figure_9 <-  nhsbsaVis::group_chart_hc(
  figure_9_data,
  x = IMD_QUINTILE,
  y = PROP,
  group = GROUP,
  type = "column",
  marker = FALSE,
  dlOn = FALSE,
  xLab = "IMD quintile",
  yLab = "Proportion (%)",
  title = ""
) %>% 
  hc_tooltip(enabled = TRUE,
             shared = TRUE,
             sort = TRUE) %>% 
  hc_legend(enabled = TRUE) %>%
  hc_tooltip(
    valueDecimals = 1,
    valueSuffix = "%"
  )%>% 
  hc_plotOptions(
    column = list(
      stacking = "normal"
    )
  ) %>%
  hc_colors(c("#ED8B00", "#005EB8"))



### Figure 10: Proportion of chargeable items by financial year {.toc-ignore}

# figure_10_data <- raw_data$exempt_annual %>%
#   dplyr::mutate(
#     FINANCIAL_YEAR = case_when(
#       FINANCIAL_YEAR == max(FINANCIAL_YEAR) ~ paste0(FINANCIAL_YEAR, "<br>(YTD ", ltst_month_tidy, ")"),
#       TRUE ~ FINANCIAL_YEAR
#     )
#   ) %>%
#   filter(
#     CHARGE_STATUS != "Null Charge Status"
#   ) %>%
#   mutate(
#     CHARGE_STATUS = case_when(
#       EXEMPT_CAT == "Pre-Payment Certificate" ~ "Used PPC",
#       EXEMPT_CAT == "HRT Pre-payment Certificate" ~ "Used HRT PPC",
#       CHARGE_STATUS %in% c("Chargeable at Current Rate", "Chargeable at Previous Rate") ~ "Charged",
#       TRUE ~ CHARGE_STATUS
#     )
#   ) %>%
#   group_by(
#     FINANCIAL_YEAR,
#     CHARGE_STATUS
#   ) %>%
#   summarise(
#     ITEM_COUNT = sum(ITEM_COUNT, na.rm = T),
#     .groups = "drop"
#   ) %>%
#   group_by(
#     FINANCIAL_YEAR
#   ) %>%
#   mutate(
#     PROP = ITEM_COUNT / sum(ITEM_COUNT, na.rm = T) * 100
#   ) %>%
#   ungroup()
# 
# figure_10<-  nhsbsaVis::group_chart_hc(
#   figure_10_data,
#   x = FINANCIAL_YEAR,
#   y = PROP,
#   group = CHARGE_STATUS,
#   type = "column",
#   marker = FALSE,
#   dlOn = FALSE,
#   xLab = "Financial year",
#   yLab = "Proportion (%)",
#   title = ""
# ) %>% 
#   hc_tooltip(enabled = TRUE,
#              shared = TRUE,
#              sort = TRUE) %>% 
#   hc_legend(enabled = TRUE) %>%
#   hc_tooltip(
#     valueDecimals = 1,
#     valueSuffix = "%"
#   ) %>%
#   hc_plotOptions(
#     column = list(
#       stacking = "normal"
#     )
#   )%>%
#   hc_colors(c("#ED8B00", "#003087", "#41B6E6","#CAF0F8"))%>%
#   hc_yAxis(gridLineColor = "#e6e6e6")
figure_10_data <- raw_data$exempt_annual %>%
  dplyr::mutate(
    FINANCIAL_YEAR = case_when(
      FINANCIAL_YEAR == max(FINANCIAL_YEAR) ~ paste0(FINANCIAL_YEAR, "<br>(YTD ", ltst_month_tidy, ")"),
      TRUE ~ FINANCIAL_YEAR
    )
  ) %>%
  filter(
    CHARGE_STATUS != "Null Charge Status"
  ) %>%
  mutate(
    CHARGE_STATUS = case_when(
      CHARGE_STATUS == "Exempt" ~ "Exempt",
      CHARGE_STATUS %in% c("Chargeable at Current Rate", "Chargeable at Previous Rate") ~ "Charged",
      TRUE ~ CHARGE_STATUS
    )
  ) %>%
  group_by(
    FINANCIAL_YEAR,
    CHARGE_STATUS
  ) %>%
  summarise(
    ITEM_COUNT = sum(ITEM_COUNT, na.rm = T),
    .groups = "drop"
  ) %>%
  group_by(
    FINANCIAL_YEAR
  ) %>%
  mutate(
    PROP = ITEM_COUNT / sum(ITEM_COUNT, na.rm = T) * 100
  ) %>%
  ungroup()

figure_10 <-  nhsbsaVis::group_chart_hc(
  figure_10_data,
  x = FINANCIAL_YEAR,
  y = PROP,
  group = CHARGE_STATUS,
  type = "column",
  marker = FALSE,
  dlOn = FALSE,
  xLab = "Financial year",
  yLab = "Proportion (%)",
  title = ""
) %>% 
  hc_tooltip(enabled = TRUE,
             shared = TRUE,
             sort = TRUE) %>% 
  hc_legend(enabled = TRUE) %>%
  hc_tooltip(
    valueDecimals = 1,
    valueSuffix = "%"
  ) %>%
  hc_plotOptions(
    column = list(
      stacking = "normal"
    )
  )%>%
  hc_colors(c("#ED8B00", "#005EB8"))





### Figure 11: Proportion of chargeable items by month July 2020 to June 2023 {.toc-ignore}

# figure_11_data <- raw_data$exempt_monthly %>%
#   filter(
#     YEAR_MONTH >= lt_st_month_min
#   ) %>%
#   filter(
#     CHARGE_STATUS != "Null Charge Status"
#   ) %>%
#   mutate(
#     CHARGE_STATUS = case_when(
#       EXEMPT_CAT == "Pre-Payment Certificate" ~ "Used PPC",
#       EXEMPT_CAT == "HRT Pre-payment Certificate" ~ "Used HRT PPC",
#       CHARGE_STATUS %in% c("Chargeable at Current Rate", "Chargeable at Previous Rate") ~ "Charged",
#       TRUE ~ CHARGE_STATUS
#     )
#   ) %>%
#   group_by(
#     YEAR_MONTH,
#     CHARGE_STATUS
#   ) %>%
#   summarise(
#     ITEM_COUNT = sum(ITEM_COUNT, na.rm = T),
#     .groups = "drop"
#   ) %>%
#   group_by(
#     YEAR_MONTH
#   ) %>%
#   mutate(
#     PROP = ITEM_COUNT / sum(ITEM_COUNT, na.rm = T) * 100
#   ) %>%
#   ungroup() %>%
#   mutate(
#     YEAR_MONTH = base::as.Date(as.character(paste0(YEAR_MONTH,"01")), format = "%Y%m%d")
#   )
# 
# figure_11<- nhsbsaVis::group_chart_hc(
#   figure_11_data,
#   x = YEAR_MONTH,
#   y = PROP,
#   group = CHARGE_STATUS,
#   type = "column",
#   marker = FALSE,
#   dlOn = FALSE,
#   xLab = "Month",
#   yLab = "Proportion (%)",
#   title = ""
# ) %>% 
#   hc_tooltip(enabled = TRUE,
#              shared = TRUE,
#              sort = TRUE) %>% 
#   hc_legend(enabled = TRUE) %>%
#   hc_tooltip(
#     valueDecimals = 1,
#     valueSuffix = "%"
#   ) %>%
#   hc_plotOptions(
#     column = list(
#       stacking = "normal"
#     )
#   ) %>%
#   hc_xAxis(
#     type = "datetime"
#   ) %>%
#   hc_colors(c("#ED8B00", "#003087", "#41B6E6","#CAF0F8"))%>%
#   hc_yAxis(gridLineColor = "#e6e6e6")
figure_11_data <- raw_data$exempt_monthly %>%
  filter(
    YEAR_MONTH >= lt_st_month_min_v2
  ) %>%
  filter(
    CHARGE_STATUS != "Null Charge Status"
  ) %>%
  mutate(
    CHARGE_STATUS = case_when(
      CHARGE_STATUS == "Exempt" ~ "Exempt",
      CHARGE_STATUS %in% c("Chargeable at Current Rate", "Chargeable at Previous Rate") ~ "Charged",
      TRUE ~ CHARGE_STATUS
    )
  ) %>%
  group_by(
    YEAR_MONTH,
    CHARGE_STATUS
  ) %>%
  summarise(
    ITEM_COUNT = sum(ITEM_COUNT, na.rm = T),
    .groups = "drop"
  ) %>%
  group_by(
    YEAR_MONTH
  ) %>%
  mutate(
    PROP = ITEM_COUNT / sum(ITEM_COUNT, na.rm = T) * 100
  ) %>%
  ungroup() %>%
  mutate(
    YEAR_MONTH = base::as.Date(as.character(paste0(YEAR_MONTH,"01")), format = "%Y%m%d")
  )

figure_11<- nhsbsaVis::group_chart_hc(
  figure_11_data,
  x = YEAR_MONTH,
  y = PROP,
  group = CHARGE_STATUS,
  type = "column",
  marker = FALSE,
  dlOn = FALSE,
  xLab = "Month",
  yLab = "Proportion (%)",
  title = ""
) %>% 
  hc_tooltip(enabled = TRUE,
             shared = TRUE,
             sort = TRUE) %>% 
  hc_legend(enabled = TRUE) %>%
  hc_tooltip(
    valueDecimals = 1,
    valueSuffix = "%"
  ) %>%
  hc_plotOptions(
    column = list(
      stacking = "normal"
    )
  ) %>%
  hc_yAxis(max = 120) %>%
  hc_xAxis(
    type = "datetime"
  ) %>%
  hc_colors(c("#ED8B00", "#005EB8"))%>%
  hc_xAxis(
    plotLines = list(
      list(
        zIndex=1000,
        value = datetime_to_timestamp(as.Date('2023-04-01', tz = 'UTC')),
        color = "grey",
        width = 1,
        dashStyle = "dash",
        label = list(
          rotation = 0,
          text = "<b>Note:</b><br> The HRT <br>PPC was <br>introduced.",
          style = list(
            fontSize = "8px"
          )
        )
      ))
  )
figure_11

# %>%
#   hc_yAxis(gridLineColor = "#e6e6e6")


# 8. Table code 

#table 1 pi data
max_col <- ncol(raw_data$pi_table_annual)
#get this number plus previous 4
seq <- seq(max_col - 4, max_col)
#get number of columns
cols <- length(seq) + 

names(raw_data$pi_table_annual)[max_col] <- paste0(
  substr(names(raw_data$pi_table_annual)[max_col],1,9), " (YTD ", ltst_month_tidy,")"
)
table_1_data <- raw_data$pi_table_annual %>%
  select(1, unlist(as.numeric(seq))) %>% 
  rename("Drug Group" = 1) %>% 
  mutate_if(is.numeric, signif, 3) %>%
  mutate_if(is.numeric, paste0, "%") %>% 
  rename_with(~ gsub(" ", "_", toupper(gsub(
    "[^[:alnum:] ]", "", .
  ))), everything())

table_1 <- raw_data$pi_table_annual %>%
  select(1, unlist(as.numeric(seq))) %>%
  rename("Drug Group" = 1) %>% 
  mutate(across(where(is.numeric), round, 1)) %>% 
  mutate(across(where(is.numeric), format, nsmall = 1)) %>% 
  mutate(across(contains("20"), ~ paste0(.x, "%"))) %>% 
  DT::datatable(rownames = FALSE,
                options = list(dom = "t",
                               columnDefs = list(
                                 list(orderable = FALSE,
                                      targets = "_all"),
                                 list(className = "dt-left", targets = 0:0),
                                 list(className = "dt-right", targets = 1:5)
                               )))          


# table 2

table_2_data_use <- presentation_annual %>%
  filter(`Financial Year` == ltst_year) %>%
  top_n(10, `Total Items`) %>%
  arrange(desc(`Total Items`)) %>%
  select(9, 12, 13, 14) %>%
  rename(
    "BNF Presentation Name" = 1,
    "Unit of Measure" = 2,
    "Total Quantity" = 3,
    "Total Items" = 4
  ) %>%
  mutate(
    `Total Quantity` = formatC(
      signif(`Total Quantity`, 3),
      big.mark = ",",
      format = "d"
    ),
    `Total Items` = formatC(
      signif(`Total Items`, 3),
      big.mark = ",",
      format = "d"
    )
  ) 
table_2 <- table_2_data_use %>% 
  DT::datatable(rownames = FALSE,
                options = list(dom = "t",
                               columnDefs = list(
                                 list(orderable = FALSE,
                                      targets = "_all"),
                                 list(className = "dt-left", targets = 0:1),
                                 list(className = "dt-right",
                                      targets = 2:3)
                               )))
table_2_data <- presentation_annual %>%
  filter(`Financial Year` == ltst_year) %>%
  top_n(10, `Total Items`) %>%
  arrange(desc(`Total Items`)) %>%
  select(9, 12, 13, 14) %>%
  rename(
    "BNF Presentation Name" = 1,
    "Unit of Measure" = 2,
    "Total Quantity" = 3,
    "Total Items" = 4
  ) %>%
  mutate(
    `Total Quantity` = formatC(
      signif(`Total Quantity`, 3),
      big.mark = ",",
      format = "d"
    ),
    `Total Items` = formatC(
      signif(`Total Items`, 3),
      big.mark = ",",
      format = "d"
    )
  ) %>% 
  rename_with(~ gsub(" ", "_", toupper(gsub(
    "[^[:alnum:] ]", "", .
  ))), everything())

#table 3 top 10 for june

table_3_data_use <- presentation_monthly %>%
  filter(`Year Month` == ltst_month) %>%
  top_n(10, `Total Items`) %>%
  arrange(desc(`Total Items`)) %>%
  select(10, 13, 14, 15) %>%
  rename(
    "BNF Presentation Name" = 1,
    "Unit of Measure" = 2,
    "Total Quantity" = 3,
    "Total Items" = 4
  ) %>%
  mutate(
    `Total Quantity` = formatC(
      signif(`Total Quantity`, 3),
      big.mark = ",",
      format = "d"
    ),
    `Total Items` = formatC(
      signif(`Total Items`, 3),
      big.mark = ",",
      format = "d"
    )
  ) 
table_3 <- table_3_data_use %>% 
  DT::datatable(rownames = FALSE,
                options = list(dom = "t",
                               columnDefs = list(
                                 list(orderable = FALSE,
                                      targets = "_all"),
                                 list(className = "dt-left", targets = 0:1),
                                 list(className = "dt-right",
                                      targets = 2:3)
                               )))
  
  table_3_data <- presentation_monthly %>%
    filter(`Year Month` == ltst_month) %>%
    top_n(10, `Total Items`) %>%
    arrange(desc(`Total Items`)) %>%
    select(10, 13, 14, 15) %>%
    rename(
      "BNF Presentation Name" = 1,
      "Unit of Measure" = 2,
      "Total Quantity" = 3,
      "Total Items" = 4
    ) %>%
    mutate(
      `Total Quantity` = formatC(
        signif(`Total Quantity`, 3),
        big.mark = ",",
        format = "d"
      ),
      `Total Items` = formatC(
        signif(`Total Items`, 3),
        big.mark = ",",
        format = "d"
      )
    ) %>% 
    rename_with(~ gsub(" ", "_", toupper(gsub(
      "[^[:alnum:] ]", "", .
    ))), everything())

# 9. render markdowns ------------------------------------------------------

rmarkdown::render("hrt-narrative.Rmd",
                  output_format = "html_document",
                  output_file = paste0("outputs/hrt_",
                                       gsub(" ", "_", ltst_month_tidy),
                                       "_v001.html"))


rmarkdown::render("hrt-narrative.Rmd",
                  output_format = "word_document",
                  output_file = paste0("outputs/hrt_",
                                       gsub(" ", "_", ltst_month_tidy),
                                       "_v001.docx"))

rmarkdown::render("hrt-background.Rmd",
                  output_format = "html_document",
                  output_file = "outputs/hrt-background-info-methodology-v002.html")

rmarkdown::render("hrt-background.Rmd",
                  output_format = "word_document",
                  output_file = "outputs/hrt-background-info-methodology-v002.docx")


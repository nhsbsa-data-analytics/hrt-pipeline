# pipeline.R --------------------------------------------------------------
# This script is used to run the RAP for the HRT publication

# 1. install required packages --------------------------------------------
req_pkgs <- c("dplyr", "stringr", "data.table", "yaml", "openxlsx","rmarkdown",
              "logr", "highcharter", "lubridate", "dbplyr")

#  utils::install.packages(req_pkgs, dependencies = TRUE)
# # #
#   devtools::install_github(
#     "nhsbsa-data-analytics/hrtR",
#     auth_token = Sys.getenv("GITHUB_PAT")
#     )
# #
#  devtools::install_github("nhsbsa-data-analytics/nhsbsaR")

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


# extract latest available full financial year
ltst_year <- ym_dim %>%
  dplyr::filter(FY_COUNT == 12) %>%
  dplyr::select(FINANCIAL_YEAR) %>%
  dplyr::filter(
    FINANCIAL_YEAR == max(FINANCIAL_YEAR, na.rm = TRUE)
  ) %>%
  dplyr::distinct() %>%
  dplyr::pull(FINANCIAL_YEAR)

# 4. extract data tables from fact table -----------------------------------------
raw_data <- list()

#patient identification rates
patient_identification_extract <- function(
  con,
  table = "HRT_FACT_DIM"
) {
fact_pi_overall <- dplyr::tbl(con,
                      from = "HRT_FACT_DIM") %>% 
  filter(
    FINANCIAL_YEAR <= ltst_year
  ) %>%
  dplyr::group_by(FINANCIAL_YEAR,
                  `BNF paragraph name` = PARAGRAPH_NAME,
                  `BNF paragraph code` = PARAGRAPH_CODE,
                  PATIENT_IDENTIFIED) %>% 
  dplyr::summarise(ITEM_COUNT = sum(ITEM_COUNT, na.rm = T)) %>%
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

#national annual table
  fact <- dplyr::tbl(con,
                     from = "HRT_FACT_DIM") %>%
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
  
  fact_national_annual <- fact %>%
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
  
  #national monthly table
  fact <- dplyr::tbl(con,
                     from = "HRT_FACT_DIM") %>%
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
  
  fact_national_monthly <- fact %>%
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
    
  

DBI::dbDisconnect(con)

# 5. data manipulation ----------------------------------------------------

# 6. write data to .xlsx --------------------------------------------------

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



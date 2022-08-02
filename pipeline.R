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

#utils::install.packages(req_pkgs, dependencies = TRUE)
#
# devtools::install_github("nhsbsa-data-analytics/hrtR",
#                          auth_token = Sys.getenv("GITHUB_PAT"))
#
#devtools::install_github("nhsbsa-data-analytics/nhsbsaR")

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
ltst_month <- ym_dim %>%
  dplyr::filter(YEAR_MONTH == max(YEAR_MONTH, na.rm = TRUE)) %>%
  dplyr::pull(YEAR_MONTH)

# use ltst_month to automate tidy date
ltst_month_tidy <- as.character(paste0(ltst_month, "01"))
ltst_month_tidy <- base::as.Date(ltst_month_tidy, format = "%Y%m%d")
ltst_month_tidy <- format(ltst_month_tidy, "%B %Y")

#calculate date 24 months back for filtering charts
lt_st_month_min <-
  as.POSIXlt(base::as.Date(as.character(paste0(ltst_month, "01")), format = "%Y%m%d"))
lt_st_month_min$mon <- lt_st_month_min$mon - 23
lt_st_month_min <- as.numeric(format(lt_st_month_min, "%Y%m"))

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
raw_data$pi_table_annual <- pi_extract(con, time_frame = "FY")

raw_data$pi_table_monthly <- pi_extract(con, time_frame = "Monthly")

raw_data$pi_excel_annual <- pi_extract_excel(con, time_frame = "FY")

raw_data$pi_excel_monthly <-
  pi_extract_excel(con, time_frame = "Monthly")

## national level data
raw_data$national_annual <- national_extract(con, time_frame = "FY")

raw_data$national_monthly <-
  national_extract(con, time_frame = "Monthly")

# data by bnf paragraph
raw_data$national_par_annual <-
  paragraph_extract(con, time_frame = "FY")

raw_data$national_par_monthly <-
  paragraph_extract(con, time_frame = "Monthly")

# data by chemical substance paragraph
raw_data$chem_sub_annual <- chem_sub_extract(con, time_frame = "FY")

raw_data$chem_sub_monthly <-
  chem_sub_extract(con, time_frame = "Monthly")

# data by presentation
raw_data$presentation_annual <-
  presentation_extract(con, time_frame = "FY")

raw_data$presentation_monthly <-
  presentation_extract(con, time_frame = "Monthly")

# icb level data
raw_data$icb_annual <- icb_extract(con, time_frame = "FY")

raw_data$icb_monthly <- icb_extract(con, time_frame = "Monthly")

# sex level data
raw_data$gender_annual <- gender_extract(con, time_frame = "FY")

raw_data$gender_monthly <-
  gender_extract(con, time_frame = "Monthly")

#age level data
raw_data$ageband_annual <- ageband_extract(con, time_frame = "FY")

raw_data$ageband_monthly <-
  ageband_extract(con, time_frame = "Monthly")

#imd quintile
raw_data$quintile_annual <- quintile_extract(con, time_frame = "FY")

raw_data$quintile_monthly <-
  quintile_extract(con, time_frame = "Monthly")

# quintile age band data
raw_data$quintile_age_annual <-
  quintile_age_extract(con, time_frame = "FY")

raw_data$quintile_age_monthly <-
  quintile_age_extract(con, time_frame = "Monthly")

# disconnect from DWH
DBI::dbDisconnect(con)

# 5. data manipulation ----------------------------------------------------
#basic hcart function
basic_chart_hc <- function(
    data,
    x, 
    y,
    type = "line",
    xLab = NULL,
    yLab = NULL,
    title = NULL,
    seriesName = "Series 1",
    color = "#005EB8",
    dlOn = TRUE,
    currency = FALSE,
    alt_text = NULL
) {
  
  `%>%` <- magrittr::`%>%`
  
  x <- rlang::enexpr(x)
  y <- rlang::enexpr(y)
  
  font <- "Arial"
  
  if(currency == TRUE) {
    
    dlFormatter <- highcharter::JS(
      paste0(
        "function () {
        
        var ynum = this.point.", y," ;
      
        if(ynum >= 1000000000) {
      
        result = ynum.toLocaleString('en-GB', {maximumSignificantDigits: 4, minimumSignificantDigits: 4, style: 'currency', currency: 'GBP'});
      
          } else {
      
        result = ynum.toLocaleString('en-GB', {maximumSignificantDigits: 3, minimumSignificantDigits: 3, style: 'currency', currency: 'GBP'});
      
          }
      
      return result
    
        }"
      )
    )
    
  } else {
    
    dlFormatter <- highcharter::JS(
      paste0(
        "function () {
      var ynum = this.point.", y," ;
      
      if(ynum >= 1000000000) {
      
      result = ynum.toLocaleString('en-GB', {maximumSignificantDigits: 4, minimumSignificantDigits: 4});
      
      } else {
      
       result = ynum.toLocaleString('en-GB', {maximumSignificantDigits: 3, minimumSignificantDigits: 3});
      
      }
      
      return result
      
    }"
      )
    )
    
  }
  
  # check chart type to set grid lines
  gridlineColor <- ifelse(type == "line", "#e6e6e6", "transparent")
  
  # check chart type to turn on y axis labels
  yLabels <- ifelse(type == "line", TRUE, FALSE)
  
  chart <- highcharter::highchart() %>% 
    highcharter::hc_chart(style = list(fontFamily = font)) %>% 
    # add only series
    highcharter::hc_add_series(data = data,
                               name = seriesName,
                               color = color,
                               type = type,
                               highcharter::hcaes(x = !!x,
                                                  y = !!y),
                               groupPadding = 0.1,
                               pointPadding = 0.05,
                               dataLabels = list(enabled = dlOn,
                                                 formatter = dlFormatter,
                                                 style = list(textOutline = "none"))) %>% 
    highcharter::hc_xAxis(type = "category",
                          title = list(text = xLab)) %>% 
    # turn off y axis and grid lines
    highcharter::hc_yAxis(title = list(text = yLab),
                          labels = list(enabled = yLabels),
                          gridLineColor = gridlineColor,
                          min = 0) %>% 
    highcharter::hc_title(text = title,
                          style = list(fontSize = "16px",
                                       fontWeight = "bold")) %>% 
    highcharter::hc_legend(enabled = FALSE) %>% 
    highcharter::hc_tooltip(enabled = FALSE) %>% 
    highcharter::hc_credits(enabled = TRUE) %>% 
    highcharter::hc_caption(text = alt_text)
  
  return(chart)
  
}

#icb pop data
icb_population <- function(){
temp <- tempfile()
icb_url <-
  utils::download.file(url = "https://www.england.nhs.uk/wp-content/uploads/2022/04/j-overall-weighted-populations-22-23.xlsx",
                       temp,
                       mode = "wb")

df <- readxl::read_xlsx(temp,
                        sheet = 5,
                        range = "A4:U63",
                        col_names = TRUE)

data <- df %>%
  filter(
    !is.na(R22)
  ) %>%
  select(2,4) %>%
  rename(
    "ICB Code" = 1
  )

return(data)
}

icb_population <- icb_population()
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
pi_data_annual <- raw_data$pi_excel_annual %>%
  apply_sdc() %>%
  select(FINANCIAL_YEAR,
         sdc_RATE) %>%
  rename("Financial Year" = 1,
         "Identified Patient Rate" = 2)

national_data <- raw_data$national_annual %>%
  select(FINANCIAL_YEAR,
         PATIENT_IDENTIFIED,
         PATIENT_COUNT,
         ITEM_COUNT,
         ITEM_PAY_DR_NIC) %>%
  apply_sdc() %>%
  select(1, 2, 6, 7, 8) %>%
  dplyr::rename(
    "Financial Year" = 1,
    "Identified Patient Flag" = 2,
    "Total Identified Patients" = 3,
    "Total Items" = 4,
    "Total Net Ingredient Cost (GBP)" = 5
  )

nat_pop <- ons_national_pop(year = 2015:2020, area = "ENPOP")

nat_pop_data <- national_data %>%
  filter(`Total Identified Patients` != 0) %>%
  mutate(`Mid-year Population Year` = as.numeric(substr(`Financial Year`, 1, 4))) %>%
  select(`Financial Year`,
         `Mid-year Population Year`,
         `Total Identified Patients`) %>%
  left_join(nat_pop,
            by = c("Mid-year Population Year" = "YEAR")) %>%
  mutate(`Patients per 1,000 Population` = `Total Identified Patients` / ENPOP * 1000) %>%
  rename("Mid-year Population Estimate" = 4)

paragraph_annual <- raw_data$national_par_annual %>%
  apply_sdc() %>%
  select(1, 2, 3, 4, 5, 6, 10, 11, 12) %>%
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
  select(1, 2, 3, 4, 5, 6, 7, 8, 12, 13, 14) %>%
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

presentation_annual <- raw_data$presentation_annual %>%
  apply_sdc() %>%
  select(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 16, 17, 18) %>%
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
  apply_sdc() %>%
  select(1, 2, 3, 4, 5, 6, 10, 11, 12) %>%
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
  select(1, 2, 3, 7, 8, 9) %>%
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
  select(1, 2, 3, 7, 8, 9) %>%
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
  select(1, 2, 3, 8, 9, 10) %>%
  left_join(imd_population,
            by = c("IMD_QUINTILE" = "IMD_QUINTILE")) %>%
  mutate(`Patients per 1,000 Population` = sdc_PATIENT_COUNT / POPULATION * 1000) %>%
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
  select(1, 2, 3, 4, 9, 10, 11) %>%
  left_join(imd_population_age,
            by = c("IMD_QUINTILE" = "IMD_QUINTILE",
                   "AGE_BAND" = "AGE_BAND")) %>%
  mutate(`Patients per 1,000 Population` = sdc_PATIENT_COUNT / POPULATION * 1000) %>%
  mutate(
    IMD_QUINTILE = case_when(
      is.na(IMD_QUINTILE) ~ as.character("Unknown"),
      IMD_QUINTILE == 1 ~ as.character("1 - Most deprived"),
      IMD_QUINTILE == 5 ~ as.character("5 - Least deprived"),
      TRUE ~ as.character(IMD_QUINTILE)
    )
  ) %>%
  select(1, 2, 3, 4, 8, 5, 6, 7, 9) %>%
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

# monthly
pi_data_monthly <- raw_data$pi_excel_monthly %>%
  apply_sdc() %>%
  select(FINANCIAL_YEAR,
         YEAR_MONTH,
         sdc_RATE) %>%
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
  apply_sdc() %>%
  select(1, 2, 3, 8, 9, 10) %>%
  dplyr::rename(
    "Financial Year" = 1,
    "Year Month" = 2,
    "Identified Patient Flag" = 3,
    "Total Identified Patients" = 4,
    "Total Items" = 5,
    "Total Net Ingredient Cost (GBP)" = 6
  )

paragraph_monthly <- raw_data$national_par_monthly %>%
  apply_sdc() %>%
  select(1, 2, 3, 4, 5, 6, 7, 12, 13, 14) %>%
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
  apply_sdc() %>%
  select(1, 2, 3, 4, 5, 6, 7, 8, 9, 14, 15, 16) %>%
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

presentation_monthly <- raw_data$presentation_monthly %>%
  apply_sdc() %>%
  select(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 18, 19, 20) %>%
  rename(
    "Financial Year" = 1,
    "Year Month" = 2,
    "BNF Section Name" = 3,
    "BNF Section Code" = 4,
    "BNF Paragraph Name" = 5,
    "BNF Paragraph Code" = 7,
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
  apply_sdc() %>%
  select(1, 2, 3, 4, 5, 6, 7, 12, 13, 14) %>%
  rename(
    "Financial Year" = 1,
    "Year Month" = 2,
    "NHS England Region Name" = 3,
    "NHS England Region Code" = 4,
    "ICB Name" = 5,
    "ICB Code" = 6,
    "Identified Patient Flag" = 7,
    "Total Identified Patients" = 8,
    "Total Items" = 9,
    "Total Net Ingredient Cost (GBP)" = 10
  )

gender_monthly <- raw_data$gender_monthly %>%
  apply_sdc() %>%
  select(1, 2, 3, 4, 9, 10, 11) %>%
  rename(
    "Financial Year" = 1,
    "Year Month" = 2,
    "Sex" = 3,
    "Identified Patient Flag" = 4,
    "Total Identified Patients" = 5,
    "Total Items" = 6,
    "Total Net Ingredient Cost (GBP)" = 7
  )

ageband_monthly <- raw_data$ageband_monthly %>%
  apply_sdc() %>%
  select(1, 2, 3, 4, 9, 10, 11) %>%
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
  apply_sdc() %>%
  select(1, 2, 3, 4, 10, 11, 12) %>%
  mutate(
    IMD_QUINTILE = case_when(
      is.na(IMD_QUINTILE) ~ as.character("Unknown"),
      IMD_QUINTILE == 1 ~ as.character("1 - Most deprived"),
      IMD_QUINTILE == 5 ~ as.character("5 - Least deprived"),
      TRUE ~ as.character(IMD_QUINTILE)
    )
  ) %>%
  select(1, 2, 4, 3, 5, 6, 7) %>%
  rename(
    "Financial Year" = 1,
    "Year Month" = 2,
    "Identified Patient Flag" = 4,
    "IMD Quintile" = 3,
    "Total Identified Patients" = 5,
    "Total Items" = 6,
    "Total Net Ingredient Cost (GBP)" = 7
  )

quintile_age_monthly <- raw_data$quintile_age_monthly %>%
  apply_sdc() %>%
  select(1, 2, 4, 5, 3, 11, 12, 13) %>%
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
    "Identified Patient Flag" = 5,
    "Age Band" = 3,
    "IMD Quintile" = 4,
    "Total Identified Patients" = 6,
    "Total Items" = 7,
    "Total Net Ingredient Cost (GBP)" = 8
  )
# 6. write data to .xlsx --------------------------------------------------

# FY Excel
# create wb object
# create list of sheetnames needed (overview and metadata created automatically)
sheetNames <- c(
  "Patient_Identification",
  "National_Total",
  "National_Population",
  "BNF_Paragraph",
  "Chemical_Substance",
  "Presentations",
  "ICB",
  "Sex",
  "Age_Band",
  "IMD_Quintile",
  "IMD_Quintile_Age"
)

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
                meta_descs)

#### Patient identification
# write data to sheet
write_sheet(
  wb,
  "Patient_Identification",
  paste0(
    "Hormone replacement therapy - England - 2015/2016 to ",
    ltst_year,
    " - Proportion of items for which an NHS number was recorded (%)"
  ),
  c(
    "1. The below proportions reflect the percentage of prescription items where a NHS number was recorded."
  ),
  pi_data_annual,
  14
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
  paste0(
    "Hormone replacement therapy - England - 2015/2016 to ",
    ltst_year,
    " - Yearly totals split by identified patients"
  ),
  c("1. Field definitions can be found on the 'Metadata' tab."),
  national_data,
  14
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
  paste0(
    "Hormone replacement therapy - England - 2015/2016 to ",
    ltst_year,
    " - Population totals split by financial year"
  ),
  c(
    "1. Some cells in this table are empty because ONS population estimates for 2021/2022 were not available prior to publication.",
    "2. ONS population estimates taken from https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates."
  ),
  nat_pop_data,
  14
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
  paste0(
    "Hormone replacement therapy - England - 2015/2016 to ",
    ltst_year,
    " - Yearly totals split by BNF paragraph and identified patients"
  ),
  c(
    "1. Field definitions can be found on the 'Metadata' tab.",
    "2. Statistical disclosure control has been applied to cells containing 5 or fewer patients or items. These cells will appear blank."
  ),
  paragraph_annual,
  14
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
  paste0(
    "Hormone replacement therapy - England - 2015/2016 to ",
    ltst_year,
    " - Yearly totals split by chemical substance and identified patients"
  ),
  c(
    "1. Field definitions can be found on the 'Metadata' tab.",
    "2. Statistical disclosure control has been applied to cells containing 5 or fewer patients or items. These cells will appear blank."
  ),
  chem_sub_annual,
  14
)

format_data(wb,
            "Chemical_Substance",
            c("A", "B", "C", "D", "E", "F", "G", "H"),
            "left",
            "")

format_data(wb,
            "Chemical_Substance",
            c("I", "J"),
            "right",
            "#,##0")

format_data(wb,
            "Chemical_Substance",
            c("K"),
            "right",
            "#,##0.00")

#### presentations substance annual
# write data to sheet
write_sheet(
  wb,
  "Presentations",
  paste0(
    "Hormone replacement therapy - England - 2015/2016 to ",
    ltst_year,
    " - Yearly totals split by presentation"
  ),
  c(
    "1. Field definitions can be found on the 'Metadata' tab.",
    "2. Statistical disclosure control has been applied to cells containing 5 or fewer items. These cells will appear blank."
  ),
  presentation_annual,
  14
)

format_data(
  wb,
  "Presentations",
  c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L"),
  "left",
  ""
)

format_data(wb,
            "Presentations",
            c("M", "N"),
            "right",
            "#,##0")

format_data(wb,
            "Presentations",
            c("O", "P", "Q", "R"),
            "right",
            "#,##0.00")

#### ICB annual
# write data to sheet
write_sheet(
  wb,
  "ICB",
  paste0(
    "Hormone replacement therapy - England - 2015/2016 to ",
    ltst_year,
    " - Yearly totals split by ICB, BNF Paragraph and identified patients"
  ),
  c(
    "1. Field definitions can be found on the 'Metadata' tab.",
    "2. Statistical disclosure control has been applied to cells containing 5 or fewer patients or items. These cells will appear blank."
  ),
  icb_annual,
  14
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
  paste0(
    "Hormone replacement therapy - England - 2015/2016 to ",
    ltst_year,
    " - Totals by sex"
  ),
  c(
    "1. Field definitions can be found on the 'Metadata' tab.",
    "2. Statistical disclosure control has been applied to cells containing 5 or fewer patients or items. These cells will appear blank.",
    "3. It is possible for a patient to be codified with gender 'unknown' or 'indeterminate'. Due to the low number of patients that these two groups contain the NHSBSA has decided to group these classifications together."
  ),
  gender_annual,
  14
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
  paste0(
    "Hormone replacement therapy - England - 2015/2016 to ",
    ltst_year,
    " - Totals by age band"
  ),
  c(
    "1. Field definitions can be found on the 'Metadata' tab.",
    "2. Statistical disclosure control has been applied to cells containing 5 or fewer patients or items. These cells will appear blank."
  ),
  ageband_annual,
  14
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
  paste0(
    "Hormone replacement therapy - England - 2015/2016 to ",
    ltst_year,
    " - Totals by IMD quintile"
  ),
  c(
    "1. Field definitions can be found on the 'Metadata' tab.",
    "2. Statistical disclosure control has been applied to cells containing 5 or fewer patients or items. These cells will appear blank.",
    "3. Where a patient's postcode has not been able to to be matched to NSPL or the patient has not been identified the records are reported as 'unknown' IMD decile.",
    "4. ONS population estimates taken from https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/populationandmigration/populationestimates/adhocs/13773populationsbyindexofmultipledeprivationimddecileenglandandwales2020/populationbyimdenglandandwales2020.xlsx"
  ),
  quintile_annual,
  14
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
            c("G", "H"),
            "right",
            "#,##0.00")

#### IMD quintile/age annual
# write data to sheet
write_sheet(
  wb,
  "IMD_Quintile_Age",
  paste0(
    "Hormone replacement therapy - England - 2015/2016 to ",
    ltst_year,
    " - Totals by IMD quintile and age band"
  ),
  c(
    "1. Field definitions can be found on the 'Metadata' tab.",
    "2. Statistical disclosure control has been applied to cells containing 5 or fewer patients or items. These cells will appear blank.",
    "3. Where a patient's postcode has not been able to to be matched to NSPL or the patient has not been identified the records are reported as 'unknown' IMD decile.",
    "4. ONS population estimates taken from https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/populationandmigration/populationestimates/adhocs/13773populationsbyindexofmultipledeprivationimddecileenglandandwales2020/populationbyimdenglandandwales2020.xlsx"
  ),
  quintile_age_annual,
  14
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
                       paste0(
                         "outputs/hrt_financial_year_",
                         gsub("/", "_", ltst_year),
                         "_v001.xlsx"
                       ),
                       overwrite = TRUE)

# Monthly Excel
# create wb object
# create list of sheetnames needed (overview and metadata created automatically)
sheetNames <- c(
  "Patient_Identification",
  "National_Total",
  "BNF_Paragraph",
  "Chemical_Substance",
  "Presentations",
  "ICB",
  "Sex",
  "Age_Band",
  "IMD_Quintile",
  "IMD_Quintile_Age"
)

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
                meta_descs)

#### Patient identification
# write data to sheet
write_sheet(
  wb,
  "Patient_Identification",
  paste0(
    "Hormone replacement therapy - England - April 2015 to ",
    ltst_month_tidy,
    " - Proportion of items for which an NHS number was recorded (%)"
  ),
  c(
    "1. The below proportions reflect the percentage of prescription items where a NHS number was recorded."
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
format_data(wb,
            "Patient_Identification",
            c("c"),
            "right",
            "0.00")

#### National data
# write data to sheet
write_sheet(
  wb,
  "National_Total",
  paste0(
    "Hormone replacement therapy - England - April 2015 to ",
    ltst_month_tidy,
    " - Monthly totals split by identified patients"
  ),
  c("1. Field definitions can be found on the 'Metadata' tab."),
  national_data_monthly,
  14
)

#left align columns A
format_data(wb,
            "National_Total",
            c("A", "B", "C"),
            "left",
            "")

#right align columns and round to 2 DP
format_data(wb,
            "National_Total",
            c("D", "E"),
            "right",
            "#,##0")

format_data(wb,
            "National_Total",
            c("F"),
            "right",
            "#,##0.00")

#### Paragraph monthly
# write data to sheet
write_sheet(
  wb,
  "BNF_Paragraph",
  paste0(
    "Hormone replacement therapy - England - April 2015 to ",
    ltst_month_tidy,
    " - Monthly totals split by BNF paragraph and identified patients"
  ),
  c(
    "1. Field definitions can be found on the 'Metadata' tab.",
    "2. Statistical disclosure control has been applied to cells containing 5 or fewer patients or items. These cells will appear blank."
  ),
  paragraph_monthly,
  14
)

format_data(wb,
            "BNF_Paragraph",
            c("A", "B", "C", "D", "E", "F", "G"),
            "left",
            "")

format_data(wb,
            "BNF_Paragraph",
            c("H", "I"),
            "right",
            "#,##0")

format_data(wb,
            "BNF_Paragraph",
            c("J"),
            "right",
            "#,##0.00")

#### Chemical substance monthly
# write data to sheet
write_sheet(
  wb,
  "Chemical_Substance",
  paste0(
    "Hormone replacement therapy - England - April 2015 to ",
    ltst_month_tidy,
    " - Monthly totals split by chemical substance and identified patients"
  ),
  c(
    "1. Field definitions can be found on the 'Metadata' tab.",
    "2. Statistical disclosure control has been applied to cells containing 5 or fewer patients or items. These cells will appear blank."
  ),
  chem_sub_monthly,
  14
)

format_data(wb,
            "Chemical_Substance",
            c("A", "B", "C", "D", "E", "F", "G", "H", "I"),
            "left",
            "")

format_data(wb,
            "Chemical_Substance",
            c("J", "K"),
            "right",
            "#,##0")

format_data(wb,
            "Chemical_Substance",
            c("L"),
            "right",
            "#,##0.00")

#### presentations substance monthly
# write data to sheet
write_sheet(
  wb,
  "Presentations",
  paste0(
    "Hormone replacement therapy - England - April 2015 to ",
    ltst_month_tidy,
    " - Monthly totals split by presentation"
  ),
  c(
    "1. Field definitions can be found on the 'Metadata' tab.",
    "2. Statistical disclosure control has been applied to cells containing 5 or fewer items. These cells will appear blank."
  ),
  presentation_monthly,
  14
)

format_data(
  wb,
  "Presentations",
  c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M"),
  "left",
  ""
)

format_data(wb,
            "Presentations",
            c("N", "O"),
            "right",
            "#,##0")

format_data(wb,
            "Presentations",
            c("P", "Q", "R", "S"),
            "right",
            "#,##0.00")

#### ICB monthly
# write data to sheet
write_sheet(
  wb,
  "ICB",
  paste0(
    "Hormone replacement therapy - England - April 2015 to ",
    ltst_month_tidy,
    " - Monthly totals split by ICB, BNF Paragraph and identified patients"
  ),
  c(
    "1. Field definitions can be found on the 'Metadata' tab.",
    "2. Statistical disclosure control has been applied to cells containing 5 or fewer patients or items. These cells will appear blank."
  ),
  icb_monthly,
  14
)

format_data(wb,
            "ICB",
            c("A", "B", "C", "D", "E", "F", "G"),
            "left",
            "")

format_data(wb,
            "ICB",
            c("H", "I"),
            "right",
            "#,##0")

format_data(wb,
            "ICB",
            c("J"),
            "right",
            "#,##0.00")

#### Sex monthly
# write data to sheet
write_sheet(
  wb,
  "Sex",
  paste0(
    "Hormone replacement therapy - England - April 2015 to ",
    ltst_month_tidy,
    " - Monthly totals by sex"
  ),
  c(
    "1. Field definitions can be found on the 'Metadata' tab.",
    "2. Statistical disclosure control has been applied to cells containing 5 or fewer patients or items. These cells will appear blank.",
    "3. It is possible for a patient to be codified with gender 'unknown' or 'indeterminate'. Due to the low number of patients that these two groups contain the NHSBSA has decided to group these classifications together."
  ),
  gender_monthly,
  14
)

format_data(wb,
            "Sex",
            c("A", "B", "C", "D"),
            "left",
            "")

format_data(wb,
            "Sex",
            c("E", "F"),
            "right",
            "#,##0")

format_data(wb,
            "Sex",
            c("G"),
            "right",
            "#,##0.00")

#### Age monthly
# write data to sheet
write_sheet(
  wb,
  "Age_Band",
  paste0(
    "Hormone replacement therapy - England - April to ",
    ltst_month_tidy,
    " - Monthly totals by age band"
  ),
  c(
    "1. Field definitions can be found on the 'Metadata' tab.",
    "2. Statistical disclosure control has been applied to cells containing 5 or fewer patients or items. These cells will appear blank."
  ),
  ageband_monthly,
  14
)

format_data(wb,
            "Age_Band",
            c("A", "B", "C", "D"),
            "left",
            "")

format_data(wb,
            "Age_Band",
            c("E", "F"),
            "right",
            "#,##0")

format_data(wb,
            "Age_Band",
            c("G"),
            "right",
            "#,##0.00")

#### IMD quintile monthly
# write data to sheet
write_sheet(
  wb,
  "IMD_Quintile",
  paste0(
    "Hormone replacement therapy - England - April 2015 to ",
    ltst_month_tidy,
    " - Monthly totals by IMD quintile"
  ),
  c(
    "1. Field definitions can be found on the 'Metadata' tab.",
    "2. Statistical disclosure control has been applied to cells containing 5 or fewer patients or items. These cells will appear blank.",
    "3. Where a patient's postcode has not been able to to be matched to NSPL or the patient has not been identified the records are reported as 'unknown' IMD decile."
  ),
  quintile_monthly,
  14
)

format_data(wb,
            "IMD_Quintile",
            c("A", "B", "C", "D"),
            "left",
            "")

format_data(wb,
            "IMD_Quintile",
            c("E", "F"),
            "right",
            "#,##0")

format_data(wb,
            "IMD_Quintile",
            c("G"),
            "right",
            "#,##0.00")

#### IMD quintile/age monthly
# write data to sheet
write_sheet(
  wb,
  "IMD_Quintile_Age",
  paste0(
    "Hormone replacement therapy - England - April 2015 to ",
    ltst_month_tidy,
    " - Monthly totals by IMD quintile and age band"
  ),
  c(
    "1. Field definitions can be found on the 'Metadata' tab.",
    "2. Statistical disclosure control has been applied to cells containing 5 or fewer patients or items. These cells will appear blank.",
    "3. Where a patient's postcode has not been able to to be matched to NSPL or the patient has not been identified the records are reported as 'unknown' IMD decile."
  ),
  quintile_age_monthly,
  14
)

format_data(wb,
            "IMD_Quintile_Age",
            c("A", "B", "C", "D", "E"),
            "left",
            "")

format_data(wb,
            "IMD_Quintile_Age",
            c("F", "G"),
            "right",
            "#,##0")

format_data(wb,
            "IMD_Quintile_Age",
            c("H"),
            "right",
            "#,##0.00")

#save file into outputs folder
openxlsx::saveWorkbook(wb,
                       paste0("outputs/hrt_monthly_", gsub(" ", "_", ltst_month_tidy), "_v001.xlsx"),
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

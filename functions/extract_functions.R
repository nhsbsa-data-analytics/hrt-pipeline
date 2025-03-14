ageband_extract <- function(con,
                            schema,
                            table,
                            time_frame = c("FY", "Monthly")) {
  time_frame <- match.arg(time_frame)
  
  if (time_frame == "FY") {
    fact <- tbl(src = con,
                dbplyr::in_schema(schema, table))  %>%
      dplyr::mutate(PATIENT_COUNT = case_when(PATIENT_IDENTIFIED == "Y" ~ 1,
                                              TRUE ~ 0),
                    #apply fix to age of 169
                    CALC_AGE = case_when(
                      CALC_AGE == 169 ~ 69,
                      TRUE ~ CALC_AGE
                    )) |>
      dplyr::group_by(FINANCIAL_YEAR,
                      IDENTIFIED_PATIENT_ID,
                      PATIENT_IDENTIFIED,
                      CALC_AGE,
                      PATIENT_COUNT) %>%
      dplyr::summarise(
        ITEM_COUNT = sum(ITEM_COUNT, na.rm = T),
        ITEM_PAY_DR_NIC = sum(ITEM_PAY_DR_NIC, na.rm = T)
      )
    
    fact_age <- fact %>%
      dplyr::inner_join(
        dplyr::tbl(con,
                   from = dbplyr::in_schema("DIM", "AGE_DIM")),
        by = c(
          "CALC_AGE" = "AGE"
        )
      ) %>%
      dplyr::mutate(AGE_BAND = dplyr::case_when(is.na(DALL_5YR_BAND) ~ "Unknown",
                                                TRUE ~ DALL_5YR_BAND)) %>%
      dplyr::group_by(FINANCIAL_YEAR,
                      AGE_BAND,
                      PATIENT_IDENTIFIED) %>%
      dplyr::summarise(
        PATIENT_COUNT = sum(PATIENT_COUNT, na.rm = T),
        ITEM_COUNT = sum(ITEM_COUNT, na.rm = T),
        ITEM_PAY_DR_NIC = sum(ITEM_PAY_DR_NIC, na.rm = T) / 100,
        .groups = "drop"
      ) %>%
      dplyr::arrange(FINANCIAL_YEAR,
                     AGE_BAND,
                     desc(PATIENT_IDENTIFIED)) %>%
      collect()
  } else {
    fact <- tbl(src = con,
                dbplyr::in_schema(schema, table))  %>%
      dplyr::mutate(PATIENT_COUNT = case_when(PATIENT_IDENTIFIED == "Y" ~ 1,
                                              TRUE ~ 0),
                    #apply fix to age of 169
                    CALC_AGE = case_when(
                      CALC_AGE == 169 ~ 69,
                      TRUE ~ CALC_AGE
                    )) |>
      dplyr::group_by(
        FINANCIAL_YEAR,
        YEAR_MONTH,
        IDENTIFIED_PATIENT_ID,
        PATIENT_IDENTIFIED,
        CALC_AGE,
        PATIENT_COUNT
      ) %>%
      dplyr::summarise(
        ITEM_COUNT = sum(ITEM_COUNT, na.rm = T),
        ITEM_PAY_DR_NIC = sum(ITEM_PAY_DR_NIC, na.rm = T)
      )
    
    fact_age <- fact %>%
      dplyr::inner_join(
        dplyr::tbl(con,
                   from = dbplyr::in_schema("DIM", "AGE_DIM")),
        by = c(
          "CALC_AGE" = "AGE"
        )
      ) %>%
      dplyr::mutate(AGE_BAND = dplyr::case_when(is.na(DALL_5YR_BAND) ~ "Unknown",
                                                TRUE ~ DALL_5YR_BAND)) %>%
      dplyr::group_by(FINANCIAL_YEAR,
                      YEAR_MONTH,
                      AGE_BAND,
                      PATIENT_IDENTIFIED) %>%
      dplyr::summarise(
        PATIENT_COUNT = sum(PATIENT_COUNT, na.rm = T),
        ITEM_COUNT = sum(ITEM_COUNT, na.rm = T),
        ITEM_PAY_DR_NIC = sum(ITEM_PAY_DR_NIC, na.rm = T) / 100,
        .groups = "drop"
      ) %>%
      dplyr::arrange(FINANCIAL_YEAR,
                     YEAR_MONTH,
                     AGE_BAND,
                     desc(PATIENT_IDENTIFIED)) %>%
      collect()
  }
  
  return(fact_age)
  
}

### Figure 1: Number of prescribed items and estimated identified patients by financial year 
{
  figure_one_data <- raw_data$national_annual %>%
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
  dplyr::arrange(desc(measure)) %>%
  group_chart_hc(
    x = FINANCIAL_YEAR,
    y = value,
    group = measure,
    type = "line",
    marker = FALSE,
    dlOn = FALSE,
    xLab = "Financial year",
    yLab = "Volume",
    title = ""
  ) 


  figure_one_data |>  
  hc_tooltip(enabled = TRUE,
             shared = TRUE,
             sort = TRUE) %>% 
  hc_legend(enabled = TRUE)%>%
  hc_xAxis(
    plotLines = list(
      list(
        value = 6,
        color = "grey",
        width = 1,
        dashStyle = "dash",
        label = list(
          rotation = 0,
          text = "<b>Note:</b> The figures <br>beyond this point <br>represent<br>prescribing in <br>April 2022 to <br>June 2022 only",
          style = list(
            fontSize = "10px"
          )
        )
      ))
  )
}


### Figure 2: Number of prescribed items and estimated identified patients by month {.toc-ignore}
{
  figure_two_data <-  raw_data$national_monthly %>%
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
  
  figure_two_data |>   group_chart_hc(
    x = YEAR_MONTH,
    y = value,
    group = measure,
    type = "line",
    marker = FALSE,
    dlOn = FALSE,
    xLab = "Month",
    yLab = "Volume",
    title = ""
  ) %>% 
  hc_tooltip(enabled = TRUE,
             shared = TRUE,
             sort = TRUE) %>% 
  hc_legend(enabled = TRUE) %>%
  hc_xAxis(type = "datetime")
}


### Figure 3: Number of prescribed items and estimated identified patients by financial year and BNF paragraph {.toc-ignore}
### Part 1

{
  figure_three_pt_1_data <- raw_data$national_par_annual %>%
  dplyr::mutate(
    FINANCIAL_YEAR = case_when(
      FINANCIAL_YEAR == max(FINANCIAL_YEAR) ~ paste0(FINANCIAL_YEAR, "<br>(YTD ", ltst_month_tidy, ")"),
      TRUE ~ FINANCIAL_YEAR
    )
  ) %>%
  filter(PARAGRAPH_CODE == "060401") %>%
  group_by(FINANCIAL_YEAR) %>%
  dplyr::summarise(`Prescribed items` = sum(ITEM_COUNT),
                   `Identified patients` = sum(PATIENT_COUNT),
                   .groups = "drop") %>% 
  tidyr::pivot_longer(cols = c(`Identified patients`,`Prescribed items`),
                      names_to = "measure",
                      values_to = "value") %>% 
  dplyr::mutate(value = signif(value, 3)) %>% 
  dplyr::arrange(desc(measure))

  figure_three_figure_pt1_data <- chart_three_pt_1_data%>%
  group_chart_hc(
    x = FINANCIAL_YEAR,
    y = value,
    group = measure,
    type = "line",
    marker = FALSE,
    dlOn = FALSE,
    xLab = "Financial year",
    yLab = "Volume",
    title = "BNF Paragraph - 060401"
  ) %>% 
  hc_tooltip(enabled = TRUE,
             shared = TRUE,
             sort = TRUE) %>% 
  hc_legend(enabled = TRUE) %>%
  hc_title(align = "left") %>%
  hc_yAxis(max = max(chart_1_data$value)) %>%
  hc_xAxis(
    plotLines = list(
      list(
        value = 6,
        color = "grey",
        width = 1,
        dashStyle = "dash",
        label = list(
          rotation = 0,
          text = "<b>Note:</b> The figures <br>beyond this point <br>represent<br>prescribing in <br>April 2022 to <br>June 2022 only",
          style = list(
            fontSize = "8px"
          )
        )
      ))
  )
}

### Part 2 and hw_grid function
{

  figure_three_pt_2_data <- raw_data$national_par_annual %>%
  dplyr::mutate(
    FINANCIAL_YEAR = case_when(
      FINANCIAL_YEAR == max(FINANCIAL_YEAR) ~ paste0(FINANCIAL_YEAR, "<br>(YTD ", ltst_month_tidy, ")"),
      TRUE ~ FINANCIAL_YEAR
    )
  ) %>%
  filter(PARAGRAPH_CODE == "070201") %>%
  group_by(FINANCIAL_YEAR) %>%
  dplyr::summarise(`Prescribed items` = sum(ITEM_COUNT),
                   `Identified patients` = sum(PATIENT_COUNT),
                   .groups = "drop") %>% 
  tidyr::pivot_longer(cols = c(`Identified patients`,`Prescribed items`),
                      names_to = "measure",
                      values_to = "value") %>% 
  dplyr::mutate(value = signif(value, 3)) %>% 
  dplyr::arrange(desc(measure))

  figure_three_figure_pt_2_data <- chart_three_pt_2_data %>%
  group_chart_hc(
    x = FINANCIAL_YEAR,
    y = value,
    group = measure,
    type = "line",
    marker = FALSE,
    dlOn = FALSE,
    xLab = "Financial year",
    yLab = "Volume",
    title = "BNF Paragraph - 070201"
  ) %>% 
  hc_tooltip(enabled = TRUE,
             shared = TRUE,
             sort = TRUE) %>% 
  hc_legend(enabled = TRUE)%>%
  hc_title(align = "left") %>%
  hc_yAxis(max = max(chart_1_data$value)) %>%
  hc_xAxis(
    plotLines = list(
      list(
        value = 6,
        color = "grey",
        width = 1,
        dashStyle = "dash",
        label = list(
          rotation = 0,
          text = "<b>Note:</b> The figures <br>beyond this point <br>represent<br>prescribing in <br>April 2022 to <br>June 2022 only",
          style = list(
            fontSize = "8px"
          )
        )
      ))
  )


hw_grid(figure_three_figure_pt1_data, figure_three_figure_pt_2_data)
}


### Figure 4: Estimated Number of identified patients by financial year for selected BNF chemical substances {.toc-ignore}
{
  figure_four_data <- raw_data$chem_sub_annual %>%
  dplyr::mutate(
    FINANCIAL_YEAR = case_when(
      FINANCIAL_YEAR == max(FINANCIAL_YEAR) ~ paste0(FINANCIAL_YEAR, "<br>(YTD ", ltst_month_tidy, ")"),
      TRUE ~ FINANCIAL_YEAR
    )
  ) %>%
  filter(
    CHEM_SUB_NAME %in% c("Estradiol", "Estriol", "Estradiol with progestogen", "Progesterone", "Norethisterone")
  ) %>%
  mutate(CHEM_SUB = paste0(CHEM_SUB_NAME, " - ", CHEM_SUB_CODE)) %>%
  select(1, 12, 9) %>%
  filter(
    PATIENT_COUNT > 0
  ) %>%
  mutate(value = signif(PATIENT_COUNT, 3))

  figure_four_data %>%
  group_chart_hc(
    x = FINANCIAL_YEAR,
    y = value,
    group = CHEM_SUB,
    type = "line",
    marker = FALSE,
    dlOn = FALSE,
    xLab = "Financial year",
    yLab = "Volume",
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
        value = 6,
        color = "grey",
        width = 1,
        dashStyle = "dash",
        label = list(
          rotation = 0,
          text = "<b>Note:</b> The figures <br>beyond this point <br>represent<br>prescribing in <br>April 2022 to <br>June 2022 only",
          style = list(
            fontSize = "10px"
          )
        )
      ))
  )%>%
  hc_colors(c("#ED8B00", "#41B6E6", "#006747", "#78BE20", "#003087", "#AE2573"))
}


### Figure 5: Number of prescribed items by financial year for selected BNF chemical substances {.toc-ignore}
{
  figure_five_data <- raw_data$chem_sub_annual %>%
  dplyr::mutate(
    FINANCIAL_YEAR = case_when(
      FINANCIAL_YEAR == max(FINANCIAL_YEAR) ~ paste0(FINANCIAL_YEAR, "<br>(YTD ", ltst_month_tidy, ")"),
      TRUE ~ FINANCIAL_YEAR
    )
  ) %>%
  filter(
    CHEM_SUB_NAME %in% c("Estradiol", "Estriol", "Estradiol with progestogen", "Progesterone", "Norethisterone")
  ) %>%
  mutate(CHEM_SUB = paste0(CHEM_SUB_NAME, " - ", CHEM_SUB_CODE)) %>%
  filter(CHEM_SUB != "Estriol - 0604011M0") %>%
  select(1, 12, 10) %>%
  group_by(FINANCIAL_YEAR, CHEM_SUB) %>%
  summarise(total = sum(ITEM_COUNT, na.rm = T))%>%
  mutate(total = signif(total, 3))



  figure_five_data %>%
  group_chart_hc(
    x = FINANCIAL_YEAR,
    y = total,
    group = CHEM_SUB,
    type = "line",
    marker = FALSE,
    dlOn = FALSE,
    xLab = "Financial year",
    yLab = "Volume",
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
        value = 6,
        color = "grey",
        width = 1,
        dashStyle = "dash",
        label = list(
          rotation = 0,
          text = "<b>Note:</b> The figures <br>beyond this point <br>represent<br>prescribing in <br>April 2022 to <br>June 2022 only",
          style = list(
            fontSize = "10px"
          )
        )
      ))
  )%>%
  hc_colors(c("#ED8B00", "#41B6E6", "#006747", "#78BE20", "#003087", "#AE2573"))
}


### Figure 9: Proportion of patients under/over 60 by IMD quintile in `r ltst_year` {.toc-ignore}

{
figure_nine_data <- raw_data$quintile_age_annual %>%
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


figure_nine_data(
  chart_data,
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
}


### Figure 10: Proportion of chargeable items by financial year {.toc-ignore}
{
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
      EXEMPT_CAT == "Pre-Payment Certificate" ~ "Used PPC",
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

  figure_10_data(
  chart_data,
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
  hc_colors(c("#ED8B00", "#003087", "#41B6E6"))%>%
  hc_yAxis(gridLineColor = "#e6e6e6")

}


### Figure 11: Proportion of chargeable items by month July 2020 to June 2022 {.toc-ignore}
{
figure_eleven_data <- raw_data$exempt_monthly %>%
  filter(
    YEAR_MONTH >= lt_st_month_min
  ) %>%
  filter(
    CHARGE_STATUS != "Null Charge Status"
  ) %>%
  mutate(
    CHARGE_STATUS = case_when(
      EXEMPT_CAT == "Pre-Payment Certificate" ~ "Used PPC",
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

  figure_eleven_data(
  chart_data,
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
  hc_xAxis(
    type = "datetime"
  ) %>%
  hc_colors(c("#ED8B00", "#003087", "#41B6E6"))%>%
  hc_yAxis(gridLineColor = "#e6e6e6")
}


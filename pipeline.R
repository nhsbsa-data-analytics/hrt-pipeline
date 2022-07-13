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

invisible(lapply(c(req_pkgs,  "nhsbsaR"), library, character.only = TRUE))

# 2. set options ----------------------------------------------------------


# 3. build fact table if new data available -------------------
con <- con_nhsbsa(
  dsn = "FBS_8192k",
  driver = "Oracle in OraClient19Home1",
  "DWCP",
  username = rstudioapi::showPrompt(title = "Username", message = "Username"),
  password = rstudioapi::askForPassword()
)

# 4. extract data tables from fact table -----------------------------------------

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



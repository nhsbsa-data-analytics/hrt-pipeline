# Info boxes -------------------------------------------------------------
infoBox_border <- function(header = "Header here",
                           text = "More text here",
                           backgroundColour = "#ccdff1",
                           borderColour = "#005EB8",
                           width = "31%",
                           fontColour = "black") {
  #set handling for when header is blank
  display <- "block"
  
  if (header == "") {
    display <- "none"
  }
  
  paste(
    "<div class='infobox_border' style = 'border: 1px solid ",
    borderColour,
    "!important;
  border-left: 5px solid ",
  borderColour,
  "!important;
  background-color: ",
  backgroundColour,
  "!important;
  padding: 10px;
  width: ",
  width,
  "!important;
  display: inline-block;
  vertical-align: top;
  flex: 1;
  height: 100%;'>
  <h4 style = 'color: ",
  fontColour,
  ";
  font-weight: bold;
  font-size: 18px;
  margin-top: 0px;
  margin-bottom: 10px;
  display: ",
  display,
  ";'>",
  header,
  "</h4>
  <p style = 'color: ",
  fontColour,
  ";
  font-size: 16px;
  margin-top: 0px;
  margin-bottom: 0px;'>",
  text,
  "</p>
</div>"
  )
}

infoBox_no_border <- function(header = "Header here",
                              text = "More text here",
                              backgroundColour = "#005EB8",
                              width = "31%",
                              fontColour = "white") {
  #set handling for when header is blank
  display <- "block"
  
  if (header == "") {
    display <- "none"
  }
  
  paste(
    "<div class='infobox_no_border',
    style = 'background-color: ",
    backgroundColour,
    "!important;padding: 10px;
    width: ",
    width,
    ";
    display: inline-block;
    vertical-align: top;
    flex: 1;
    height: 100%;'>
  <h4 style = 'color: ",
  fontColour,
  ";
  font-weight: bold;
  font-size: 18px;
  margin-top: 0px;
  margin-bottom: 10px;
  display: ",
  display,
  ";'>",
  header,
  "</h4>
  <p style = 'color: ",
  fontColour,
  ";
  font-size: 16px;
  margin-top: 0px;
  margin-bottom: 0px;'>",
  text,
  "</p>
</div>"
  )
}
### CSV Download button
get_download_button <- function(data = data, title = "Download chart data", filename = "data") {
  dt <- datatable(data, rownames = FALSE,
                  extensions = 'Buttons',
                  options = list(
                    searching = FALSE,
                    paging = TRUE,
                    bInfo = FALSE,
                    pageLength = 1,
                    dom = '<"datatable-wrapper"B>',
                    buttons = list(
                      list(extend = 'csv',
                           text = title,
                           filename = filename,
                           className = "nhs-button-style")
                    ),
                    initComplete = JS(
                      "function(settings, json) {",
                      "$(this.api().table().node()).css('visibility', 'collapse');",
                      "}"
                    )
                  )
  )
  
  return(dt)
}
#---------------
### Statistical Disclosure Control

apply_sdc <-
  function(data,
           level = 0,
           rounding = TRUE,
           round_val = 5,
           mask = NA) {
    `%>%` <- magrittr::`%>%`
    
    rnd <- round_val
    
    if (is.character(mask)) {
      type <- function(x)
        as.character(x)
    } else {
      type <- function(x)
        x
    }
    
    data %>% dplyr::mutate(dplyr::across(
      where(is.numeric),
      .fns = ~ dplyr::case_when(
        .x >= level & rounding == T ~ type(rnd * round(.x / rnd)),
        .x < level & .x > 0 & rounding == T ~ mask,
        .x < level & .x > 0 & rounding == F ~ mask,
        TRUE ~ type(.x)
      ),
      .names = "sdc_{.col}"
    ))
  }

### Statistical Disclosure Control

apply_sdc <-
  function(data,
           level = 5,
           rounding = TRUE,
           round_val = 5,
           mask = NA) {
    `%>%` <- magrittr::`%>%`
    
    rnd <- round_val
    
    if (is.character(mask)) {
      type <- function(x)
        as.character(x)
    } else {
      type <- function(x)
        x
    }
    
    data %>% dplyr::mutate(dplyr::across(
      where(is.numeric),
      .fns = ~ dplyr::case_when(
        .x >= level & rounding == T ~ type(rnd * round(.x / rnd)),
        .x < level & .x > 0 & rounding == T ~ mask,
        .x < level & .x > 0 & rounding == F ~ mask,
        TRUE ~ type(.x)
      ),
      .names = "sdc_{.col}"
    ))
  }
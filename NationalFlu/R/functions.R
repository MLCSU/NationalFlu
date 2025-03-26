# Get the link to the latest UEC Daily SitRep data because the filename often
# changes
latest_url <- function() {
  page_url <- 
    "https://www.england.nhs.uk/statistics/statistical-work-areas/uec-sitrep/urgent-and-emergency-care-daily-situation-reports-2024-25/"
  
  #read the page
  html_data <- rvest::read_html(page_url)
  # get the link text and URLs
  links      <- html_data %>% rvest::html_nodes("a")
  linktext   <- links %>%     rvest::html_text()
  urls       <- links %>%     rvest::html_attr("href")
  # search for the URL of the link with the usual text
  latest_url <- urls[match("Web File Timeseries – UEC Daily SitRep", linktext)]  
}

# Load the NHS-E data
# note the awkward structure, including two-row headings
load_NHS_data <- function(filename) {
  data <- read_xlsx(file = filename,
                      sheet = "Flu", start_row = 14, start_col = 2,
                      col_names = FALSE,
                      skip_empty_rows = TRUE, skip_empty_cols = TRUE)
  # names mostly from the second row, but takes date from first row if not NA
  # note, better to make.names here rather than let colnames do it
  colnames(data) <- make.names( as.character( 
    ifelse(is.na(data[1,]), data[2,], data[1,]) ),
    unique = TRUE)
  
  # Relies on REN being the last trust before notes etc
  lastrow <- match("REN", data$Code)
  
  # Make the data tidy
  data <- data %>% 
    # remove the CC flu beds columns so just have the G&A flu beds
    select(!contains("CC.flu.beds")) %>% 
    # remove the top four rows with headings and England totals and also 
    # anything after last data row
    slice(5:lastrow) %>% 
    # and pivot, keeping the first three columns as they are
    pivot_longer(!c(1:3),
                 names_to = "date",
                 values_to = "value") %>% 
    # fix the date format
    mutate(date = as.Date(date, "X%Y.%m.%d")) %>% 
    # fix the value format
    mutate(value = as.integer(value)) %>% 
    # 2022/23 actually goes into April so best to do FY here based on first date
    mutate(FY = 
             glue("FY{year(date[1])-2000}/{year(date[1])-1999}")) %>% 
    # Removing any 29th Feb so that days align for the plot 
    filter(!(month(date) == 2 & day(date) == 29)) %>%
    # Create a label starting in year 1901 (not 1900 as that had 29th Feb)
    mutate(label = make_datetime(year(date) - year(date[1]) + 1901,
                                 month(date),
                                 day(date)))
  
  # 2022/23 actually goes into April so best to do it here based on first date
#  data$FY <- glue("FY{year(data$date[1])-2000}/{year(data$date[1])-1999}")
#  data$label <- 
  
  return(data)
}
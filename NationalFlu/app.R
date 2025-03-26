#
# App to show chart of publicly available data on G&A beds occupied 'flu
# Originally deployed at: https://nhsml-nuct.shinyapps.io/NationalFlu
# Created by the DS Team within NUCT at NHS ML
# Best contact for queries probably Andy McCann on LinkedIn 
#

library(shiny)
library(shinyWidgets)
library(tidyverse)
library(openxlsx2)
library(zoo)
library(glue)
library(lubridate)
library(plotly)
library(rvest)         # To parse html to get link

# Uses functions in the file below
source("R/functions.R")    # shouldn't need this when deploy as shinyapps
                           # should load (though no harm leaving in for testing)


# Load the historic data (using load_NHS_data function in R/functions.R)
# To avoid reloading the historic data from website, 
# 2022_flu.xlsx is tab "Flu" from "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2023/04/UEC-Daily-SitRep-Web-File-Timeseries.xlsx"
# 2023_flu.xlsx is tab "Flu" from "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2024/04/Web-File-Timeseries-UEC-Daily-SitRep.xlsx"
NHS_data <- bind_rows( 
  load_NHS_data("2022_flu.xlsx"),
  load_NHS_data("2023_flu.xlsx")
)

# The url of the latest data often changes, so get it by searching for text.
# Note, I think due to cache this sometimes keeps getting old link for a while
# after the data updates.  Ideally would find a way to force it to get the 
# most recent copy of the page.
 latest_url <- latest_url() 
 # As long as we found the url for the latest data then add it to the dataset
 if ( !is.na(latest_url) ) {
  NHS_data <- bind_rows( 
    NHS_data,
    load_NHS_data(latest_url) )
}

# Fallback if can't wait for the above to find the new link, 
# FIND THE LINK MANUALLY FROM 
# https://www.england.nhs.uk/statistics/statistical-work-areas/uec-sitrep/urgent-and-emergency-care-daily-situation-reports-2024-25/
# uncomment the below and manually put link in
#- NHS_data <- bind_rows( 
#-  NHS_data,
#-  load_NHS_data("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2025/03/Web-File-Timeseries-UEC-Daily-SitRep-1.xlsx") )
#- latest_url <- "fallback"

# Note latest data to display later
latest_date <- max(NHS_data$date)

# create dummy data with 0s in case nothing selected
dummy <- NHS_data %>% 
  group_by(FY, label, date) %>% 
  summarise(MetricValue = 0)
# highlight the latest year
alpha     <- c(rep(0.3, length(unique(NHS_data$FY)) -1), 1.0)
linewidth <- c(rep(0.8, length(unique(NHS_data$FY)) -1), 1.0)

# read the ICB and Region mapping (relies on no trusts being added to data)
mapping <- read.csv("mapping.csv")
# get lists of regions, ICBs and trusts
regions <- mapping$Region %>% unique() %>% sort()
ICBs    <- mapping$ICB %>% unique() %>% sort()
trusts  <- mapping$Name %>% unique() %>% sort()

# Define UI for application
ui <- fluidPage(

    # Application title
    titlePanel("'flu patients in G&A Hospital Beds"),

    # Sidebar with drop-downs for Region, ICB and Trust 
    sidebarLayout(
        sidebarPanel(
          pickerInput("selectedregions",
                      "Region(s):",
                      choices = regions,
                      options = list(`actions-box` = TRUE),
                      multiple = TRUE,
                      selected = regions),
          pickerInput("selectedICBs",
                      "ICB(s):",
                      choices = ICBs,
                      options = list(`actions-box` = TRUE),
                      multiple = TRUE,
                      selected = ICBs),
          pickerInput("selectedtrusts",
                      "Trust(s):",
                      choices = trusts,
                      options = list(`actions-box` = TRUE),
                      multiple = TRUE,
                      selected = trusts)
        ),

        # Show a plot of the generated distribution
        mainPanel(
          HTML(r"(
                <p><b>Please note</b>, due to changes in the team which created
               this app, it will not be updated after March 2025.
               <p>If anyone wishes to create a version for future winters then
               please take the code at:
               <A href="https://github.com/MLCSU/NationalFlu" TARGET="_blank">
                https://github.com/MLCSU/NationalFlu
                </A> 
               and host it elsewhere.
               <p>So long, and thanks for all the fish.  Andy McCann</p>
           )"),
          HTML(r"(
                <p>This chart compares the number of patients with 
             laboratory-confirmed influenza in General & Acute (G&A) beds in the 
             selected trusts, compared with previous winters.
           )"),
          
          plotlyOutput("plot"),
          
          HTML(r"(
                <p>Source: https://nhsml-nuct.shinyapps.io/NationalFlu/ by 
                  <A href="mailto:mlcsu.nuct@nhs.net?subject=Flu Shiny app">
                  NHS ML
                  </A> 
                  from data at: 
                <A href="https://www.england.nhs.uk/statistics/statistical-work-areas/uec-sitrep/urgent-and-emergency-care-daily-situation-reports-2024-25/"
                TARGET="_blank">
                https://www.england.nhs.uk/statistics/statistical-work-areas/uec-sitrep/urgent-and-emergency-care-daily-situation-reports-2024-25/
                </A>
               )"),

          if (is.na(latest_url)) {
            tags$p(
              tags$b("ERROR-URL to latest data file not found")
            )
          }
          else {
            tags$p(
              "Latest data: ", 
              tags$b(format(latest_date, "%A %e %B %Y"))
            )
          },

          HTML(r"(
                <p>"Weekly updates commenced on Thursday 5 December 2024, covering the week ending 1 December 2024, and will continue every Thursday at 09:30 throughout winter. The final publication will be made on Thursday 3 April 2025 covering the week ending Sunday 30 March 2025.
                <p>During the Christmas period, due to bank holidays, no publications will be made on Thursday 26 December 2024. Instead, publications for the weeks ending 22 and 29 December will be made the following Friday, 3 January 2025."
                <p>
                <p>This Shiny App was created by the Data Science Team within
                <A href="https://www.midlandsandlancashirecsu.nhs.uk/our-expertise/nursing-and-urgent-care/">
                the Nursing and Urgent Care Team at NHS ML</A>.
                <p>For specialist, independent, clinical and analytical guidance
                on a regional, national and international scale, 
                <A href="mailto:mlcsu.nuct@nhs.net?subject=Flu Shiny app">
                contact us</A> at: 
                <A href="mailto:mlcsu.nuct@nhs.net?subject=Flu Shiny app">
                mlcsu.nuct@nhs.net</A>
                )")
        )
    )
)

# Define server logic
server <- function(input, output, session) {

  observeEvent(input$selectedregions, {
    choicesI <- mapping %>% 
                      filter(Region %in% input$selectedregions) %>%
                      select(ICB) %>% unique() %>% arrange(ICB)
    updatePickerInput(session = session, inputId = "selectedICBs",
                      choices = choicesI$ICB, selected = choicesI$ICB)
  }, ignoreInit = TRUE)

  observeEvent(input$selectedICBs, {
    choicesT <- mapping %>% 
      filter(ICB %in% input$selectedICBs) %>%
      select(Name) %>% unique() %>% arrange(Name)
    updatePickerInput(session = session, inputId = "selectedtrusts",
                      choices = choicesT$Name, selected = choicesT$Name)
  }, ignoreInit = TRUE)

  grouped <- reactive({
    if(length(input$selectedtrusts) == 0) {
      dummy
    } else {
    NHS_data %>% 
    filter(Name %in% input$selectedtrusts) %>% 
    group_by(FY, label, date) %>% 
    # 25/12/2022 and 26/12/2022 had some NA but only small trusts
    summarise(MetricValue = sum(value, na.rm = T))
    }
  })
  
    output$plot <- renderPlotly({
          if (length(input$selectedtrusts)==length(trusts)) {
                     suffix <- "England"
          }
          else if (length(input$selectedtrusts) == 0) {
            suffix <- "No trusts selected"
          }
          else if (length(input$selectedtrusts) == 1) {
            suffix <- input$selectedtrusts
          }
          else if (length(input$selectedICBs) == 1 & 
                   length(input$selectedtrusts) == 
                     nrow(filter(mapping, ICB==input$selectedICBs))) {
            suffix <- input$selectedICBs
          }
          else if (length(input$selectedregions) == 1 & 
                   length(input$selectedtrusts) == 
                     nrow(filter(mapping, Region==input$selectedregions))) {
            suffix <- input$selectedregions
          }
      else {
            suffix <- "Selected Trusts"
          }
          title <- paste('G&A Beds occupied by Influenza Patients',
                          suffix, sep="\n")
          gplot <- grouped() %>%
          ggplot(aes(y=MetricValue, x=label, group=FY, colour=FY, alpha=FY,
                     linewidth=FY,
                     text = paste0(FY, "\n",
                                   date, "\n",
                                   "Beds: ", MetricValue))) +
# below didn't work, hovering over data lines the info didn't change
#        text = ifelse(length(input$selectedtrusts) == 0, 
#                                   "No trusts selected",
#                                   paste0(FY, "\n",
#                                         date, "\n",
#                                         "Beds: ", MetricValue)))) +
        geom_line() + 
        #  theme_mlcsu() + 
          ggtitle(title) + 
          labs(caption="Source: MLCSU from UEC Daily Sitrep") + 
          theme(axis.title.x = element_blank(),legend.title = element_blank(),plot.caption = element_text(hjust = 0)) + 
          ylab('No. Flu Patients in a G&A bed') +
          ylim(0,ifelse(length(input$selectedtrusts) == 0, 100, NA)) +
         scale_linewidth_manual(values = linewidth) +
         scale_alpha_manual(values = alpha)
         ggplotly(gplot, tooltip = "text")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Load necessary libraries
library(shiny)
library(tidyverse)
library(leaflet)
library(heatmaply)
library(shinyWidgets)
library(shinyjs)


# Define the user interface (UI)
fluidPage(
  setBackgroundImage(
    src = "bgimg.jpg"
  ),
  # add shinyjs usage
  useShinyjs(),
  extendShinyjs(text = "shinyjs.resetClickSpider = function() { Shiny.onInputChange('plotly_click-spider', 'null'); }", functions = c("resetClickSpider")), # create function for clear filters
  extendShinyjs(text = "shinyjs.resetClickLine = function() { Shiny.onInputChange('plotly_click-line', 'null'); }", functions = c("resetClickLine")),
  fixedRow(
    # app title
    div(
      style = "display: flex; justify-content: center; align-items: center;",
      h2("Analysis of Crime and Trends in New York City"))
  ),
  fixedRow(
    column(
      # UI for animated bar chart
      width = 6,
      h3("Crime Evolution Over the Years"),
      div("Arrests were highest for Dangerous Drugs up until 2017, from 2018 onwards 3rd Degree Assault came above all. 
         3rd Degree Assault arrests were very less during 2006 and as the years passed the number of arrests kept increasing until finally it took the top spot from 2018 onwards. 
         As years passed the arrests for Dangerous Drugs kept reducing and crimes such as Petit Larceny, Felony Assault, Vechicle and Traffic Laws 
         saw significant increase thus changing the types of crime arrests over the years."),
      imageOutput("bars")
    ),
    column(
      # UI for line chart
      width = 6,
      h3("Top 10 Crimes Overall"),
      div("Looking at the over number of complaints and arrests we can see that the number of complaints each year have been greater than the number of arrests. 
          From 2014 onwards there have been significant decrease in the number of arrests. 
          As we can see the number of arrests and complaints have a dip during 2020 due to COVID and rise back up again after the lockdowns are over. 
          By the overall trend we can say that the complaints have been reducing but after COVID it reached even greater heights but the arrests have no significant improvement. "),
      plotlyOutput("line_graph"),
      textOutput("clickedGroup"),
      actionButton("clearOfns", "Clear Filter")
    )
  ),
  fixedRow(
    div(
      style = "display: flex; justify-content: center; align-items: center;",
      h3(textOutput("year_choose")))
  ),
  fixedRow(
    column(
      # UI for heat map
      width = 6,
      h3("Gender and Race Distribution of Victims"),
      div("As we can see from the heat map it is evident that the most affect people are the black females. 
          In some cases, the men are affected more in Blacks and Whites. This could be due to various causes such as discrimination or so."),
      plotlyOutput("heat_map"),
    ),
    column(
      # UI for spider chart
      width = 6,
      h3("Age Distribution of Victims"),
      div("In every year people of the age group of 25-44 are affected the most as these people are mostly 
         of the working class and young adults who tend to be more prone to being victims of crimes of scams."),
      plotlyOutput("spider"),
      textOutput("clickedAgeGroup"),
      actionButton("clearAge", "Clear Filter")
    )
  ),
  fixedRow(
    column(
      width = 12,
      div("Below is a map that shows the points of arrests and complaints over the years. Use the filters to filter the data for the Races and Age Groups as well.")
    )
  ),
  fixedRow(
    # UI for rendering map and filters
    column(
      width = 12,
      sidebarLayout(
        # Sidebar panel with input sliders
          sidebarPanel(
            sliderInput("year",
                        "Year:",
                        min = min(year(complaints$RPT_DT)),
                        max = max(year(complaints$RPT_DT)),
                        value = min(year(complaints$RPT_DT))),
            selectInput(
              inputId = "type",  # Input ID for referencing the input
              label = "Data:",  # Label for the dropdown
              choices = c("Complaints", "Arrests"),  # Dropdown options
              selected = "Complaints"  # Default selected option
            ),
            checkboxInput("schools", "Show Schools", value = TRUE)
          ),
        # Main panel with the Leaflet map
        mainPanel(
          leafletOutput("map")
        )
    )
    )
  ),
  tags$footer(
    "Data Sources:",
    tags$a(
      "Current Year Arrest Data",
      target = "_blank",
      href = "https://catalog.data.gov/dataset/nypd-arrest-data-year-to-date"
    ),", ",
    tags$a(
      "Historic Arrest Data",
      target = "_blank",
      href = "https://catalog.data.gov/dataset/nypd-arrests-data-historic"
    ),", ",
    tags$a(
      "Current Year Complaint Data",
      target = "_blank",
      href = "https://catalog.data.gov/dataset/nypd-complaint-data-current-year-to-date"
    ),", ",
    tags$a(
      "Historic Complaint Data",
      target = "_blank",
      href = "https://catalog.data.gov/dataset/nypd-complaint-data-historic"
    ),", ",
    tags$a(
      "School Locations",
      target = "_blank",
      href = "https://catalog.data.gov/dataset/2018-2019-school-locations"
    ),
    style = "position: absolute; width: 100%; color: black; text-align: center;"
  )

)


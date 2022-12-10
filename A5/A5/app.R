#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyWidgets)
library(dplyr)


# Define UI for application that draws a histogram

co2data <- vroom::vroom("co2data.csv", na = "") %>% filter(co2 > 0)

ui <- fluidPage(
  titlePanel("CO2 Emissions in the World"),
  sidebarLayout(
    sidebarPanel(
      # Selector for the country
      selectInput("country", "Country", choices = unique(co2data$country)),
    ),
    mainPanel(
      tableOutput("co2stats"),
      # Plot a graph of CO2 changes throughout years for a given country
      plotOutput("plotData")
    )
  )
)

server <- function(input, output, session) {
  country <- reactive({
    req(input$country)
    filter(co2data, country == input$country)
  })
  
  co2StatsData <- reactive({
    req(input$country)
    country() %>%
      filter(country == input$country) %>%
      summarise(average_co2_emissions=mean(co2), max_co2_emissions=max(co2), min_co2_emissions=min(co2), total_co2_emissions=sum(co2))
  })
  
  co2PlotData <- reactive({
    req(input$country)
    co2filtered <- filter(co2data, country == input$country)
    plot(
      type = "l",
      main = "CO2 emissions per year",
      xlab = "Year",
      ylab = "CO2 volume",
      x = co2filtered$year, 
      y = co2filtered$co2)
  })
  
  output$co2stats <- renderTable(co2StatsData())
  output$plotData <- renderPlot(co2PlotData())
  
  observeEvent(country(), {
    updateSelectInput(session, "year", choices = unique(country()$year), selected = character())
  })
}
shinyApp(ui, server)


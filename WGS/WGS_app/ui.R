load("measures.RData")
load("years.RData")
load("countries.RData")

library(shiny)
library(shinythemes)

# Define UI for application that draws a histogram
shinyUI(

  fluidPage(theme = shinytheme("paper"),

  # Application title
  titlePanel("World Gender Statistics"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(

      tags$h4("Select to display in map:"),

      uiOutput("choose_measure"),

      uiOutput("choose_years"),

      radioButtons("sex", "Gender:",
                   choices = list("Male" = "M",
                                  "Female" = "F",
                                  "Ratio" = "ratio"),
                   selected = "ratio"),

      tags$h4("Select to display in timeline:"),

      selectInput("country", "Country",
                  choices = countries)
  ),

    # Show a plot of the generated distribution
    mainPanel(
      tags$h3("World map of gender statistic"),

      p("Select a statistic and year on sidebar panel."),

       plotOutput("map",
                  click = "plot_click", height = "auto"),

      p("Click on any country on the map to find out more about it:"),

      tableOutput("info"),

      br(),

      tags$h3("Timeline of gender statistic"),

      p("Select a country on sidebar panel."),

      plotOutput("timeline", height = "auto")
    ),
  )
))


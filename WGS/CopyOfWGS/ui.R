load("measures.RData")
load("years.RData")

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

      tags$h3("Select to display:"),
      selectInput("measure", "Measurement:",
                  choices = measures),

      selectInput("year", "Year:",
                choices = rev(years)),

      radioButtons("sex", "Gender:",
                   choices = list("Male" = "M",
                                  "Female" = "F",
                                  "Ratio" = "ratio"),
                   selected = "ratio")
  ),

    # Show a plot of the generated distribution
    mainPanel(
      tags$h2("World map of gender statistics"),

       plotOutput("map",
                  click = "plot_click"),
      verbatimTextOutput("info")
    ),
  )
))


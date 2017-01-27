load("measures.RData")
load("years.RData")

library(shiny)
library(shinythemes)

# Define UI for application that draws a histogram
shinyUI(

  fluidPage(theme = shinytheme("united"),

  # Application title
  titlePanel("World Gender Statistics"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(

    selectInput("measure", "Measurement:",
                choices = measures),

    selectInput("year", "Year:",
                choices = rev(years))
  ),

    # Show a plot of the generated distribution
    mainPanel(
       plotOutput("map")
    )
  )
)


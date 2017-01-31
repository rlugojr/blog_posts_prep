# https://github.com/daattali/timevis

library(shiny)
library(timevis)

data <- data.frame(
  content = c("Chip-Seq Marie Liebmann",
              "Paper schreiben",
              "Antrag übersetzen & überarbeiten",
              "Vortrag für Informatikseminar vorbereiten auf Englisch!",
              "Informatikseminar",
              "Protein-miRNA-WGCNA",
              "Doktorarbeit veröffentlichen ML to predict df based on ft"),
  start = c("2017-02-01", "2017-02-01", "2017-02-01", "2017-02-01", "2017-02-23 14:00:00", "2017-04-01", "2017-04-01"),
  end = c("2017-02-03", "2017-02-24", "2017-02-24", "2017-02-23", NA, NA, NA)
)

data$id <- seq_len(nrow(data))

ui <- fluidPage(
  titlePanel("To do timeline"),

  timevisOutput("timeline")
)

server <- function(input, output, session) {
  output$timeline <- renderTimevis({
    timevis(data)
  })
}

shinyApp(ui = ui, server = server)

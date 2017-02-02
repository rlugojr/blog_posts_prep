# https://github.com/daattali/timevis

library(shiny)
library(shinythemes )
library(timevis)

data <- data.frame(
  content = c("Chip-Seq Marie Liebmann",
              "Treffen mit Marie Liebmann",
              "Paper schreiben",
              "Antrag übersetzen & überarbeiten",
              "Vortrag für Informatikseminar vorbereiten<br>(auf Englisch!)",
              "Informatikseminar",
              "Protein-miRNA-WGCNA",
              "Doktorarbeit veröffentlichen ML to predict df based on ft"),
  start = c("2017-02-01", "2017-02-03", "2017-02-09", "2017-02-01", "2017-02-01", "2017-02-23 14:00:00", "2017-04-01", "2017-04-01"),
  end = c("2017-02-03", NA, "2017-02-24", "2017-02-24", "2017-02-02", NA, NA, NA),
  type = c("background", "point", "range", "range", "range", "point", "box", "box"),
  group = c(1, 1, 2, 2, 3, 3, 2, 2),
  style = c("color: grey;", "color: red;", NA, "color: red;", "color: grey;", NA, NA, NA)
)

data$id <- seq_len(nrow(data))

groups <- data.frame(
  id = 1:3,
  content = c("Collaboration projects", "My own projects", "Presentations/ Seminars")
)


ui <- fluidPage(theme = shinytheme("flatly"),
  titlePanel("To do timeline"),

  timevisOutput("timeline")
)

server <- function(input, output, session) {
  output$timeline <- renderTimevis({
    timevis(data, groups, fit = FALSE, options = list(editable = TRUE, multiselect = TRUE, align = "left"))
  })
}

shinyApp(ui = ui, server = server)

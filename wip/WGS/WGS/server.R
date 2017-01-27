load("diff_table_bind.RData")
load("wmap_countries_robin_df_final.RData")

library(shiny)
library(dplyr)
library(ggplot2)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

  output$map <- renderPlot({

    measure = input$measure
    year = input$year

    diff_table_map <- diff_table_bind[which(diff_table_bind$Indicator.Name == measure), c(1:3, which(colnames(diff_table_bind) == paste0("X", year)))]
    colnames(diff_table_map)[ncol(diff_table_map)] <- "value"

    map <- left_join(subset(wmap_countries_robin_df_final, !continent == "Antarctica"), diff_table_map, by = c("gu_a3" = "Country.Code"))

    ggplot(map) +
      coord_equal() +
      map_theme +
      geom_polygon(aes(long, lat, group=group, fill=log2(value))) +
      geom_path(aes(long, lat, group=group), color="white", size=0.5) +
      labs(title = paste(measure)) +
      scale_fill_gradientn(colours=coolwarm(100), guide = "colourbar")

  })

})

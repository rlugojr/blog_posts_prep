load("diff_table_bind.RData")
load("dataset_fem.RData")
load("dataset_male.RData")
load("wmap_countries_robin_df_final.RData")

library(shiny)
library(dplyr)
library(ggplot2)

library(dplyr)
library(ggplot2)

map_theme <- list(theme(panel.grid.minor = element_blank(),
                        panel.grid.major = element_blank(),
                        panel.background = element_blank(),
                        plot.background = element_rect(fill = "white"),
                        panel.border = element_blank(),
                        axis.line = element_blank(),
                        axis.text.x = element_blank(),
                        axis.text.y = element_blank(),
                        axis.ticks = element_blank(),
                        axis.title.x = element_blank(),
                        axis.title.y = element_blank(),
                        plot.title = element_text(size = 18)))

colfunc <- colorRampPalette(c("yellow", "red"))

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

  output$map <- renderPlot({

    measure = input$measure
    year = input$year

    if (input$sex == "ratio") {

      diff_table_map <- diff_table_bind[which(diff_table_bind$Indicator.Name == measure),
                                        c(1:3, which(colnames(diff_table_bind) == paste0("X", year)))]
      colnames(diff_table_map)[ncol(diff_table_map)] <- "value"

      map <- left_join(subset(wmap_countries_robin_df_final, !continent == "Antarctica"), diff_table_map, by = c("gu_a3" = "Country.Code"))

      ggplot(map, aes(long, lat, group = group, fill = log2(value))) +
        coord_cartesian() +
        map_theme +
        geom_polygon() +
        geom_path(color = "white", size = 0.5) +
        labs(title = paste(measure),
             fill = "log2 of male / female") +
        scale_fill_gradient2(low = "blue", midpoint = 0, mid = "yellow", high = "red")

    } else if (input$sex == "F") {

      fem_table_map <- dataset_fem[which(dataset_fem$Indicator.Name == measure), c(1:3, which(colnames(dataset_fem) == paste0("X", year)))]
      colnames(fem_table_map)[ncol(fem_table_map)] <- "value"

      map_fem <- left_join(subset(wmap_countries_robin_df_final, !continent == "Antarctica"), fem_table_map, by = c("gu_a3" = "Country.Code"))

      ggplot(map_fem, aes(long, lat, group = group, fill = value)) +
        coord_equal() +
        map_theme +
        geom_polygon() +
        geom_path(color = "white", size = 0.5) +
        labs(title = paste(measure),
             fill = "Value") +
        scale_fill_gradientn(colours = colfunc(100))

    } else if (input$sex == "M") {

      male_table_map <- dataset_male[which(dataset_male$Indicator.Name == measure), c(1:3, which(colnames(dataset_male) == paste0("X", year)))]
      colnames(male_table_map)[ncol(male_table_map)] <- "value"

      map_male <- left_join(subset(wmap_countries_robin_df_final, !continent == "Antarctica"), male_table_map, by = c("gu_a3" = "Country.Code"))

      ggplot(map_male, aes(long, lat, group = group, fill = value)) +
        coord_equal() +
        map_theme +
        geom_polygon() +
        geom_path(color = "white", size = 0.5) +
        labs(title = paste(measure),
             fill = "Value") +
        scale_fill_gradientn(colours = colfunc(100))

    }
  })

  output$info <- renderPrint({
    x = input$plot_click$x
    y = input$plot_click$y

    for (country in unique(wmap_countries_robin_df_final$gu_a3)) {

      subs <- subset(wmap_countries_robin_df_final, gu_a3 == country)

      point <- point.in.polygon(x, y, subs$long, subs$lat)

      if (country == "AFG") {
        points_table <- data.frame(country, subs$name_sort[1], point)
      } else {
        points_table <- rbind(points_table, data.frame(country, subs$name_sort[1], point))
      }
    }

    points_table[which(points_table$point == 1), ]
  })

})

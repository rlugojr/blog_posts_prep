load("diff_table_bind.RData")
load("dataset_fem.RData")
load("dataset_male.RData")
load("wmap_countries_smaller_df_final.RData")

load("measures.RData")


library(shiny)
library(dplyr)
library(ggplot2)
library(tidyr)

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

my_theme <- function(base_size = 12, base_family = "sans"){
  theme_minimal(base_size = base_size, base_family = base_family) +
    theme(
      axis.text = element_text(size = 12),
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
      axis.title = element_text(size = 14),
      panel.grid.major = element_line(color = "grey"),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "white"),
      strip.background = element_rect(fill = "royalblue", color = "grey", size = 1),
      strip.text = element_text(face = "bold", size = 12, color = "white"),
      legend.position = "right",
      panel.border = element_rect(color = "grey", fill = NA, size = 0.5)
    )
}

colfunc <- colorRampPalette(c("yellow", "red"))

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {

  # Drop-down selection box for which measurement
  output$choose_measure <- renderUI({
    selectInput("measure", "Statistic", measures)
  })

  # Check years
  output$choose_years <- renderUI({
    measure = input$measure
    subs <- diff_table_bind[which(diff_table_bind$Indicator.Name == measure), -c(1:4)]
    keep <- gsub("X", "", names(which(colSums(subs, na.rm = TRUE) > 0)))

    selectInput("year", "Year",
                choices = rev(keep))
  })


  output$map <- renderPlot({

    measure <- input$measure
    year <- input$year

    if (input$sex == "ratio") {

      diff_table_map <- diff_table_bind[which(diff_table_bind$Indicator.Name == measure),
                                        c(1:3, which(colnames(diff_table_bind) == paste0("X", year)))]
      colnames(diff_table_map)[ncol(diff_table_map)] <- "value"

      map <- left_join(subset(wmap_countries_df_final, !continent == "Antarctica"), diff_table_map, by = c("gu_a3" = "Country.Code"))
      map$value_log2 <- log2(as.numeric(map$value))

      ggplot(map, aes(long, lat, group = group, fill = value_log2)) +
        map_theme +
        geom_polygon() +
        geom_path(color = "white", size = 0.5) +
        labs(title = paste(measure),
             fill = "log2 of\nmale /\nfemale") +
        scale_fill_gradient2(low = "blue", midpoint = 0, mid = "yellow", high = "red", na.value = "grey30")

    } else if (input$sex == "F") {

      fem_table_map <- dataset_fem[which(dataset_fem$Indicator.Name == measure), c(1:3, which(colnames(dataset_fem) == paste0("X", year)))]
      colnames(fem_table_map)[ncol(fem_table_map)] <- "value"

      map_fem <- left_join(subset(wmap_countries_df_final, !continent == "Antarctica"), fem_table_map, by = c("gu_a3" = "Country.Code"))

      ggplot(map_fem, aes(long, lat, group = group, fill = value)) +
        map_theme +
        geom_polygon() +
        geom_path(color = "white", size = 0.5) +
        labs(title = paste(measure),
             fill = "Value") +
        scale_fill_gradientn(colours = colfunc(100), na.value = "grey30")

    } else if (input$sex == "M") {

      male_table_map <- dataset_male[which(dataset_male$Indicator.Name == measure), c(1:3, which(colnames(dataset_male) == paste0("X", year)))]
      colnames(male_table_map)[ncol(male_table_map)] <- "value"

      map_male <- left_join(subset(wmap_countries_df_final, !continent == "Antarctica"), male_table_map, by = c("gu_a3" = "Country.Code"))

      ggplot(map_male, aes(long, lat, group = group, fill = value)) +
        map_theme +
        geom_polygon() +
        geom_path(color = "white", size = 0.5) +
        labs(title = paste(measure),
             fill = "Value") +
        scale_fill_gradientn(colours = colfunc(100), na.value = "grey30")

    }
  }, height = function() {
    0.6 * session$clientData$output_map_width
  })

  output$info <- renderTable({
    table <- nearPoints(wmap_countries_df_final[, c(1, 2, 26, 42, 48, 63)], input$plot_click, xvar = "long", yvar = "lat", maxpoints = 1, threshold = 100)

    colnames(table) <- c("longitude", "latitude", "country", "population est.", "income group", "region")

    table
  })

  output$timeline <- renderPlot({
    measure <- input$measure
    country <- input$country

    diff_table_timeline <- rbind(dataset_fem, dataset_male) %>%
      subset(Indicator.Name == measure) %>%
      subset(Country.Code %in% wmap_countries_df_final$gu_a3) %>%
      subset(as.character(Country.Name) == country)

    diff_table_timeline_gather <- gather(diff_table_timeline, year, value, X1960:X2015)
    diff_table_timeline_gather$year <- gsub("X", "", diff_table_timeline_gather$year)

    ggplot(diff_table_timeline_gather, aes(x = year, y = value, color = gender, group = gender)) +
      geom_line(size = 1, alpha = 0.7) +
      geom_point(size = 2, alpha = 0.7) +
      my_theme() +
      scale_color_brewer(palette = "Set1") +
      labs(title = paste(measure),
           subtitle = paste(country),
           x = "Year",
           y = "Value",
           color = "Gender")
  }, height = function() {
    0.4 * session$clientData$output_timeline_width
  })

})

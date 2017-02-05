Today, I want to share my analysis of the World Gender Statistics dataset.

Last week I introduced [my Shiny app here](https://shiring.shinyapps.io/wgs_app/), where you can explore 160 measurements over 56 years. This week I've put included a statistical analysis of these measurements and put some finishing touches on the app.

Alternatively, if you are using R, you can load the app via Github with the **shiny** package. [There, you can also find the source code for the app](https://github.com/ShirinG/WGS_app).

``` r
library(shiny)
runGitHub("ShirinG/WGS_app") 
```

<br>

### The data

The data was downloaded from [The World Bank's Open Data project](http://data.worldbank.org/) via [Kaggle](https://www.kaggle.com/theworldbank/world-gender-statistic). The main datatable shows

-   Country.Name: the name of the country
-   Country.Code: the country's code
-   Indicator.Name: the name of the variable that this row represents
-   Indicator.Code: a unique id for the variable
-   1960 - 2016: one column EACH for the value of the variable in each year it was available

Unfortunately, the dataset doesn't include a column that indicated which two female/ male statistics belong together and there is no consistent naming scheme either. Therefore, I chose to focus on those statistics, where the counterparts could be easily extracted: indicator codes that differed only in containing ".FE" or ".MA". I then split the subsetted dataset into female and male and produced a third dataset with the ratios between male and female values. The finished datasets were saved as "R.Data" files, so that I could easily load them into the Shiny app.

``` r
dataset <- read.csv("Data.csv")
dataset_subs <- dataset[grep(".FE|.MA", dataset$Indicator.Code), ]
head(dataset_subs)

dataset_subs$Indicator.Name <- as.character(dataset_subs$Indicator.Name)

dataset_fem <- dataset[grep("female", dataset$Indicator.Name), ]
dataset_fem$Indicator.Name <- gsub("female", "", dataset_fem$Indicator.Name)
dataset_fem$Indicator.Name <- gsub(",", "", dataset_fem$Indicator.Name)
dataset_fem$Indicator.Code <- gsub(".FE", "", dataset_fem$Indicator.Code)
dataset_fem$gender <- "female"

dataset_male <- dataset[-grep("female", dataset$Indicator.Name), ]
dataset_male$Indicator.Name <- gsub("male", "", dataset_male$Indicator.Name)
dataset_male$Indicator.Name <- gsub(",", "", dataset_male$Indicator.Name)
dataset_male$Indicator.Code <- gsub(".FE", "", dataset_male$Indicator.Code)
dataset_male$gender <- "male"

dataset_fem <- dataset_fem[which(dataset_fem$Indicator.Name %in% dataset_male$Indicator.Name), ]
dataset_male <- dataset_male[which(dataset_male$Indicator.Name %in% dataset_fem$Indicator.Name), ]

dataset_fem <- dataset_fem[which(dataset_fem$Country.Code %in% dataset_male$Country.Code), ]
dataset_male <- dataset_male[which(dataset_male$Country.Code %in% dataset_fem$Country.Code), ]

library(dplyr)
dataset_fem <- arrange(dataset_fem, Country.Code)
dataset_male <- arrange(dataset_male, Country.Code)

dataset_fem$Country.Code <- as.character(dataset_fem$Country.Code)
dataset_male$Country.Code <- as.character(dataset_male$Country.Code)

save(dataset_fem, file = "dataset_fem.RData")
save(dataset_male, file = "dataset_male.RData")
```

``` r
length(unique(dataset_fem$Indicator.Name)) == length(unique(dataset_male$Indicator.Name))

for (n in 1:length(unique(dataset_fem$Indicator.Name))) {
  
  code <- unique(dataset_fem$Indicator.Name)[n]
  
  print(code)
                 
  fem <- dataset_fem[which(dataset_fem$Indicator.Name == code), ]
  male <- dataset_male[which(dataset_male$Indicator.Name == code), ]

  for (i in 1:nrow(fem)) {
    
    if (i == 1) {
      
      diff <- (male[i, 5:61] + 0.001) / (fem[i, 5:61] + 0.001)
      diff_table <- cbind(male[i, c(1:4)], diff)
      
    } else {
      
      diff <- (male[i, 5:61] + 0.001) / (fem[i, 5:61] + 0.001)
      diff_table <- rbind(diff_table, 
                          cbind(male[i, c(1:4)], diff))
      
    }
  }
  
  if (n == 1) {
    
    diff_table_bind <- diff_table
    
  } else {
    
    diff_table_bind <- rbind(diff_table_bind, diff_table)
    
  }
  
}

diff_table_bind$Country.Code <- as.character(diff_table_bind$Country.Code)
```

``` r
measures <- unique(diff_table_bind$Indicator.Name)
save(measures, file = "measures.RData")

years <- gsub("X", "", colnames(diff_table_bind)[-c(1:4)])
years <- years[-length(years)]
save(years, file = "years.RData")
```

<br>

Map
---

The map has been downloaded from \[the Natural Earth Data website\]{<http://www.naturalearthdata.com/downloads/110m-cultural-vectors/>). The country borders were reduced by 200 meters with [ArcGIS Pro](https://www.esri.de/produkte/arcgis-pro), so that clicking within any country on the map would show the corresponding country as nearest point. [ArcGIS Pro](https://www.esri.de/produkte/arcgis-pro) was also used to convert the map to [Mercator projection](https://en.wikipedia.org/wiki/Mercator_projection). The changed shapefiles can be downloaded from [my Github repository](https://github.com/ShirinG/blog_posts_prep/tree/master/WGS/shapefiles/changed_borders).

``` r
library(rgdal)
library(ggplot2)
library(plyr)
library(dplyr)
library(scales)

wmap_countries <- readOGR(dsn="shapefiles/changed_borders", layer="ne_110m_admin_0_countries_smaller_wm")

wmap_countries_df <- fortify(wmap_countries)
wmap_countries@data$id <- rownames(wmap_countries@data)
wmap_countries_df_final <- join(wmap_countries_df, wmap_countries@data, by = "id")

wmap_countries_df_final$adm0_a3 <- as.character(wmap_countries_df_final$adm0_a3)

save(wmap_countries_df_final, file = "wmap_countries_smaller_df_final.RData")

diff_table_bind <- diff_table_bind[which(diff_table_bind$Country.Code %in% wmap_countries_df_final$adm0_a3), ]

save(diff_table_bind, file = "diff_table_bind.RData")

countries <- as.character(unique(diff_table_bind$Country.Name))
save(countries, file = "countries.RData")
```

To calculate statistical differences, I prepared a dataset that contains the first and last non-NA value in each time series (per statistic and country) to calculate whether the change over time was statistically significant.

``` r
year_table <- diff_table_bind[which(diff_table_bind$Country.Code %in% wmap_countries_df_final$adm0_a3), ]

# last non-NA value
last_val <- apply(year_table[, grep("^X[0-9]+$", colnames(year_table))], 1, function(x) 
  na.omit(x)[length(na.omit(x))]
  )
last_val_df <- data.frame(unlist(last_val))
last_val_df$year_of_val <- gsub("([0-9]+)(.X)([0-9]+)", "\\3", rownames(last_val_df))
rownames(last_val_df) <- gsub("([0-9]+)(.X)([0-9]+)", "\\1", rownames(last_val_df))

# first non-NA value
first_val <- apply(year_table[, grep("^X[0-9]+$", colnames(year_table))], 1, function(x) 
  na.omit(x)[1]
  )
first_val_df <- data.frame(na.omit(first_val))

year_table_last_val <- cbind(year_table[rownames(last_val_df), 1:3], last_val_df, first_val_df)
save(year_table_last_val, file = "year_table_last_val.RData")
```

The world data contains information for each country. The following variables I used to test for statistical significance:

1.  its economic status,
2.  income group,
3.  estimated population size,
4.  estimated gross domestic product (GDP) and
5.  continent.

``` r
library(dplyr)
country.inf <- subset(wmap_countries_df_final[, c(17, 25, 26, 31, 32, 42, 43, 47, 48, 62)], !continent == "Antarctica")
country.inf <- country.inf[!duplicated(country.inf), ]

stats_test <- left_join(subset(year_table_last_val, Indicator.Name == measure), country.inf, by = c("Country.Code" = "adm0_a3"))
```

``` r
stats_test %>%
  ggplot(aes(x = pop_est, y = na.omit.first_val., fill = pop_est, color = pop_est)) +
      geom_point(alpha = 0.7, size = 2) +
      my_theme()
```

``` r
stats_test %>%
  ggplot(aes(x = gdp_md_est, y = na.omit.first_val., fill = gdp_md_est, color = gdp_md_est)) +
      geom_point(alpha = 0.7, size = 2) +
      my_theme()
```

Because not all measures' data was normally distributed I explored [non-parametric models](https://en.wikipedia.org/wiki/Nonparametric_statistics), as well as an [ANOVA](https://en.wikipedia.org/wiki/Analysis_of_variance). The plots for each statistic can be explored in my app, so here, I am only showing the results and which meaures were statistically significant for the different variables.

<br>

Wilcoxon Signed-Rank Test
-------------------------

The [Wilcoxon Signed-Rank test](https://en.wikipedia.org/wiki/Wilcoxon_signed-rank_test) can be used to test whether observations from repeated measures differ statistically from each other. Here, I am using it to test whether the most recent recorded value is significantly different from the first recorded value.

<br>

Kruskal-Wallis Test
-------------------

The [Kruskal-Wallis test](https://en.wikipedia.org/wiki/Kruskal%E2%80%93Wallis_one-way_analysis_of_variance) is a simple non-parametric statistical test. I used it here to test for each variable of interest whether it has a statistically significant effect on the most recent recorded value. Because I am testing the same data multiple times, I am correcting the p-values and only consider them significant with False Discovery Rate (FDR) below 10% (adjusted p-value &lt; 0.1).

------------------------------------------------------------------------

<br>

``` r
sessionInfo()
```

    ## R version 3.3.2 (2016-10-31)
    ## Platform: x86_64-apple-darwin13.4.0 (64-bit)
    ## Running under: macOS Sierra 10.12.1
    ## 
    ## locale:
    ## [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
    ## 
    ## attached base packages:
    ## [1] stats     graphics  grDevices utils     datasets  methods   base     
    ## 
    ## other attached packages:
    ## [1] dplyr_0.5.0
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] Rcpp_0.12.8      codetools_0.2-15 digest_0.6.11    rprojroot_1.1   
    ##  [5] assertthat_0.1   R6_2.2.0         DBI_0.5-1        backports_1.0.4 
    ##  [9] magrittr_1.5     evaluate_0.10    stringi_1.1.2    rmarkdown_1.3   
    ## [13] tools_3.3.2      stringr_1.1.0    yaml_2.1.14      htmltools_0.3.5 
    ## [17] knitr_1.15.1     tibble_1.2

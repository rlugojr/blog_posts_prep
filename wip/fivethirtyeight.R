install.packages("fivethirtyeight")

#https://mran.microsoft.com/web/packages/fivethirtyeight/vignettes/fivethirtyeight.html
#https://mran.microsoft.com/web/packages/fivethirtyeight/fivethirtyeight.pdf

# https://mran.microsoft.com/web/packages/fivethirtyeight/vignettes/bechdel.html

library(fivethirtyeight)

str(food_world_cup)

str(nutrition_pvalues)
head(nutrition_pvalues)

library(dplyr)
library(ggplot2)
library(knitr)
library(magrittr)
library(broom)
library(stringr)
library(fivethirtyeight)
library(ggthemes)
library(scales)
data("bechdel")
# Turn off scientific notation
options(scipen = 99)

install.packages("fivethirtyeight")

#https://mran.microsoft.com/web/packages/fivethirtyeight/vignettes/fivethirtyeight.html
#https://mran.microsoft.com/web/packages/fivethirtyeight/fivethirtyeight.pdf

# https://mran.microsoft.com/web/packages/fivethirtyeight/vignettes/bechdel.html

library(fivethirtyeight)

library(dplyr)
library(ggplot2)
library(knitr)
library(magrittr)
library(broom)
library(stringr)
library(fivethirtyeight)
library(ggthemes)
library(scales)

food_world_cup

nutrition_pvalues

ggplot(nutrition_pvalues, aes(x = characteristic, y = food, fill = p_values)) +
  geom_tile(width = 0.9, height = 0.9)

data("bechdel")
# Turn off scientific notation
options(scipen = 99)

college_all_ages


data("drinks")

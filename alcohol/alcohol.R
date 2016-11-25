install.packages("ttbbeer")

# https://mran.microsoft.com/web/packages/ttbbeer/ttbbeer.pdf

library(ttbbeer)

head(beermaterials)

beermaterials_df <- as.data.frame(beermaterials)

beermaterials_df$Date <- paste("01", beermaterials_df$Month, beermaterials_df$Year)
beermaterials_df$Date <- as.Date(beermaterials_df$Date, format = "%d %B %Y")

head(beermaterials_df)
tail(beermaterials_df)

devtools::install_github('Ather-Energy/ggTimeSeries')

library(ggplot2)
library(ggTimeSeries)

p1 = ggplot_waterfall(
  dtData = beermaterials_df,
  'Date',
  'Malt_and_malt_products'
)
p1 +
  xlab(NULL) +
  ylab(NULL)


###

artificialcarbontax$tax <- "artificialcarbon"
artificialcarbontax[nrow(artificialcarbontax), 1] <- "1991-01-01"
head(artificialcarbontax)

beertax$tax <- "beer"
head(beertax)

champagnetax$tax <- "champagne"
head(champagnetax)

spirittax$tax <- "spirit"
head(spirittax)

tobaccotax$ITEM <- tolower(tobaccotax$ITEM)
tobaccotax$ITEM <- gsub(" ", "_", tobaccotax$ITEM)
tobaccotax$tax <- paste("tobacco", tobaccotax$ITEM, sep = "_")
tobaccotax_2 <- tobaccotax[, -1]
tobaccotax_2[18, 2] <- "1977-01-31"
tobaccotax_2[18, 1] <- "1977-01-01"
head(tobaccotax_2)

winetax14$tax <- "wine_14"
head(winetax14)

winetax1421$tax <- "wine_14_21"
head(winetax1421)

winetax2124$tax <- "wine_21_24"
head(winetax2124)

tax <- rbind(artificialcarbontax, beertax, champagnetax, spirittax, tobaccotax_2, winetax14, winetax1421, winetax2124)
head(tax)

tax_2 <- tax
tax_2$ID <- paste(tax_2$tax, rownames(tax_2), sep = "_")
head(tax_2)

library(tidyr)
tax_gather <- tax_2 %>%
  gather(dat_column, Date, FROM:TO)
head(tax_gather)

ggplot(data = tax, aes(x = FROM, y = RATE, color = tax)) +
  geom_point(size = 1.5, alpha = 0.6)

ggplot(tax_gather,aes(x=Date,y=RATE,color=tax))+geom_line()+geom_path(aes(group = ID))


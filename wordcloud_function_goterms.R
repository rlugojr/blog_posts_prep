library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)

wordcloud_function <- function(data = data,
                               removewords = c("process", "activity", "positive", "negative", "response", "regulation"),
                               min.freq = 4,
                               max.words=Inf,
                               random.order=TRUE){
  input <- Corpus(VectorSource(data))

  input <- tm_map(input, content_transformer(tolower))
  input <- tm_map(input, content_transformer(removePunctuation))
  input <- tm_map(input, removeNumbers)
  input <- tm_map(input, stripWhitespace)

  toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
  input <- tm_map(input, toSpace, "/")
  input <- tm_map(input, toSpace, "@")
  input <- tm_map(input, toSpace, "\\|")

  input <- tm_map(input, function(x) removeWords(x, stopwords("english")))

  # specify your stopwords as a character vector
  input <- tm_map(input, removeWords, removewords)

  tdm <- TermDocumentMatrix(input)
  m <- as.matrix(tdm)
  v <- sort(rowSums(m),decreasing=TRUE)
  d <- data.frame(word = names(v),freq=v)

  set.seed(1234)
  wordcloud(words = d$word, freq = d$freq, min.freq = min.freq, scale=c(8,.2),
            max.words=max.words, random.order=random.order, rot.per=0.15,
            colors=brewer.pal(8, "Dark2"))
}
```

```{r echo=TRUE, message=FALSE, warning=FALSE, fig.width=15, fig.height=6, fig.align="center", cache=FALSE}
layout(matrix(c(1:20), nrow = 2, byrow = FALSE), heights = c(0.1, 1))

for (i in 1:nrow(datasets_2)){
  species <- datasets_2$dataset[i]
  df <- summary(get(paste("go_enrich", species, sep = "_")))

  par(mar=rep(0, 4))
  plot.new()
  text(x=0.5, y=0.5, paste0(datasets_2$description[i]), cex = 2)
  wordcloud_function(data = df$Description)
}

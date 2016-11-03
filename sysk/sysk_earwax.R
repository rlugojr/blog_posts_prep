# http://www.stuffyoushouldknow.com/podcasts/earwax-live-with-it-2.htm
# Earwax: Live With It
# POSTED MAR 19, 2015

library(dplyr)
library(stringr)
library(tidyr)

setwd("U:/Github_blog/blog_posts_prep/sysk")

raw <- readLines("sysk_earwax.transcript.txt")
head(raw)

df <- do.call("rbind", lapply(raw, as.data.frame))
dim(df)
head(df)

# separate into Josh and Chuck

df_Josh <- as.character(df[grep("^Josh:", df[, 1]), ])
str(df_Josh)

df_Chuck <- as.character(df[grep("^Chuck:", df[, 1]), ])

# and remove the beginning of each line telling me who's speaking

df_Josh <- gsub("^Josh: ", "", df_Josh)
df_Chuck <- gsub("^Chuck: ", "", df_Chuck)

head(df_Josh)
head(df_Chuck)


# https://eight2late.wordpress.com/2015/05/27/a-gentle-introduction-to-text-mining-using-r/

library(tm)

# build a corpus, which is a collection of text documents
# VectorSource specifies that the source is character vectors.
vs <- VectorSource(df_Josh)
inspect(VCorpus(vs))

Corpus_Josh <- Corpus(VectorSource(df_Josh))

#create the toSpace content transformer
toSpace <- content_transformer(function(x, pattern) {return (gsub(pattern, " ", x))})

Corpus_Josh <- tm_map(Corpus_Josh, toSpace, "-")
Corpus_Josh <- tm_map(Corpus_Josh, toSpace, ":")

#Remove punctuation – replace punctuation marks with " "
Corpus_Josh <- tm_map(Corpus_Josh, removePunctuation)

Corpus_Josh <- tm_map(Corpus_Josh, toSpace, "’")
Corpus_Josh <- tm_map(Corpus_Josh, toSpace, "‘")
Corpus_Josh <- tm_map(Corpus_Josh, toSpace, " -")

#Transform to lower case (need to wrap in content_transformer)
Corpus_Josh <- tm_map(Corpus_Josh, content_transformer(tolower))

#Strip digits (std transformation, so no need for content_transformer)
Corpus_Josh <- tm_map(Corpus_Josh, removeNumbers)

#remove stopwords using the standard list in tm
Corpus_Josh <- tm_map(Corpus_Josh, removeWords, stopwords("english"))
#docs <- tm_map(docs, removeWords, c("department", "email"))
#toString <- content_transformer(function(x, from, to) gsub(from, to, x))
#docs <- tm_map(docs, toString, "harbin institute technology", "HIT")

#Strip whitespace (cosmetic?)
Corpus_Josh <- tm_map(Corpus_Josh, stripWhitespace)

# Stemming

#load library
library(SnowballC)
#Stem document
Corpus_Josh <- tm_map(Corpus_Josh,stemDocument)

#treat your preprocessed documents as text documents.
Corpus_Josh <- tm_map(Corpus_Josh, PlainTextDocument)


dtm_Josh <- DocumentTermMatrix(Corpus_Josh)

tdm_Josh <- TermDocumentMatrix(Corpus_Josh)
tdm_Josh

freq_Josh <- colSums(as.matrix(dtm_Josh))

#create sort order (descending)
ord <- order(freq_Josh,decreasing=TRUE)

#inspect most frequently occurring terms
freq_Josh[head(ord)]

#inspect least frequently occurring terms
freq_Josh[tail(ord)]

findFreqTerms(dtm_Josh,lowfreq=40)
findAssocs(dtm_Josh, "ear",0.3)

# Correlations Plots
plot(dtm_Josh,
     terms=findFreqTerms(dtm_Josh, lowfreq=10),
     corThreshold=0.3)

#  Start by removing sparse terms:
dtms_Josh <- removeSparseTerms(dtm_Josh, 0.01) # This makes a matrix that is 10% empty space, maximum.
inspect(dtms_Josh)

wf=data.frame(term=names(freq_Josh),occurrences=freq_Josh)
head(wf)
library(ggplot2)
p <- ggplot(subset(wf, freq_Josh>40), aes(term, occurrences))
p <- p + geom_bar(stat="identity")
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))
p

#wordcloud
library(wordcloud)
#setting the same seed each time ensures consistent look across clouds
set.seed(42)
#limit words by specifying min frequency
wordcloud(names(freq_Josh),freq_Josh, min.freq=5,colors=brewer.pal(6,"Dark2"))


# https://eight2late.wordpress.com/2015/07/22/a-gentle-introduction-to-cluster-analysis-using-r/

#convert dtm to matrix
m <- as.matrix(dtm_Josh)

#shorten rownames for display purposes
rownames(m) <- paste(substring(rownames(m),1,3),rep("..",nrow(m)),
                                           substring(rownames(m), nchar(rownames(m))-12,nchar(rownames(m))-4))
#compute distance between document vectors
d <- dist(m)

#run hierarchical clustering using Ward’s method
groups <- hclust(d,method="ward.D")
#plot dendogram, use hang to ensure that labels fall below tree
plot(groups, hang=-1)

#cut into 2 subtrees – try 3 and 5
rect.hclust(groups,2)

# https://cran.r-project.org/web/packages/syuzhet/vignettes/syuzhet-vignette.html

library(syuzhet)

mySentiment <- get_nrc_sentiment(df_Josh)
head(mySentiment)

rbind(
  sign(head(get_sentiment(df_Josh, method="syuzhet"))),
  sign(head(get_sentiment(df_Josh, method="bing"))),
  sign(head(get_sentiment(df_Josh, method="afinn"))),
  sign(head(get_sentiment(df_Josh, method="nrc")))
)

s_v_sentiment <- get_sentiment(df_Josh)
plot(
  s_v_sentiment,
  type="l",
  main="Example Plot Trajectory",
  xlab = "Narrative Time",
  ylab= "Emotional Valence"
)

percent_vals <- get_percentage_values(s_v_sentiment, bins = 10)
plot(
  percent_vals,
  type="l",
  main="Percentage-Based Means",
  xlab = "Narrative Time",
  ylab= "Emotional Valence",
  col="red"
)

ft_values <- get_transformed_values(
  s_v_sentiment,
  low_pass_size = 3,
  x_reverse_len = 100,
  padding_factor = 2,
  scale_vals = TRUE,
  scale_range = FALSE
)
plot(
  ft_values,
  type ="l",
  main ="Joyce's Portrait using Transformed Values",
  xlab = "Narrative Time",
  ylab = "Emotional Valence",
  col = "red"
)

simple_plot(s_v_sentiment)

######################################################

Needed <- c("tm", "SnowballC", "RColorBrewer", "ggplot2", "wordcloud", "biclust", "cluster", "igraph", "fpc")
install.packages(Needed, dependencies=TRUE)

install.packages("Rcampdf", repos = "http://datacube.wu.ac.at/", type = "source")

library(tm)
summary(Corpus_Josh)



######################################################

# http://onepager.togaware.com/TextMiningO.pdf

# Quantitative Analysis

words <- dtm_Josh %>%
  as.matrix %>%
  colnames %>%
  (function(x) x[nchar(x) < 20])

length(words)
summary(nchar(words))
table(nchar(words))
dist_tab(nchar(words))

# Word Length Counts
data.frame(nletters=nchar(words)) %>%
  ggplot(aes(x=nletters)) +
  geom_histogram(binwidth=1) +
  geom_vline(xintercept=mean(nchar(words)),
             colour="green", size=1, alpha=.5) +
  labs(x="Number of Letters", y="Number of Words")

# Letter Frequency

library(dplyr)

library(stringr)

words %>%

  str_split("") %>%

  sapply(function(x) x[-1]) %>%

  unlist %>%

  dist_tab %>%

  mutate(Letter=factor(toupper(interval),

                       levels=toupper(interval[order(freq)]))) %>%

  ggplot(aes(Letter, weight=percent)) +

  geom_bar() +

  coord_flip() +

  labs(y="Proportion") +

  scale_y_continuous(breaks=seq(0, 12, 2),

                     label=function(x) paste0(x, "%"),expand=c(0,0), limits=c(0,12))

# Letter and Position Heatmap

words %>%

  lapply(function(x) sapply(letters, gregexpr, x, fixed=TRUE)) %>%

  unlist %>%

  (function(x) x[x!=-1]) %>%

  (function(x) setNames(x, gsub("\\d", "", names(x)))) %>%

  (function(x) apply(table(data.frame(letter=toupper(names(x)),

                                      position=unname(x))),

                     1, function(y) y/length(x))) %>%

  qheat(high="green", low="yellow", by.column=NULL,

        values=TRUE, digits=3, plot=FALSE) +

  labs(y="Letter", x="Position") +

  theme(axis.text.x=element_text(angle=0)) +

  guides(fill=guide_legend(title="Proportion"))



install.packages("tmcn.word2vec", repos="http://R-Forge.R-project.org")

library(tmcn.word2vec)

model <- word2vec(system.file("examples", "rfaq.txt", package = "tmcn.word2vec"))

distance(model$model_file, "the")


library(lda)

# From demo(lda)

library("ggplot2")

library("reshape2")

data(cora.documents)

data(cora.vocab)

theme_set(theme_bw())

set.seed(8675309)

K <- 10 ## Num clusters

result <- lda.collapsed.gibbs.sampler(cora.documents,

                                      K, ## Num clusters

                                      cora.vocab,

                                      25, ## Num iterations

                                      0.1,

                                      0.1,

                                      compute.log.likelihood=TRUE)


## Get the top words in the cluster

top.words <- top.topic.words(result$topics, 5, by.score=TRUE)


## Number of documents to display

N <- 10

topic.proportions <- t(result$document_sums) / colSums(result$document_sums)


topic.proportions <-

  topic.proportions[sample(1:dim(topic.proportions)[1], N),]


topic.proportions[is.na(topic.proportions)] <- 1 / K


colnames(topic.proportions) <- apply(top.words, 2, paste, collapse=" ")


topic.proportions.df <- melt(cbind(data.frame(topic.proportions),

                                   document=factor(1:N)),

                             variable.name="topic",

                             id.vars = "document")


ggplot(topic.proportions.df, aes(x=topic, y=value, fill=topic)) +

  geom_bar(stat="identity") +

  theme(axis.text.x = element_text(angle=45, hjust=1, size=7),

        legend.position="none") +

  coord_flip() +

  facet_wrap(~ document, ncol=5)




######################################################

install_github('mananshah99/sentR')

require('sentR')

# Create small vectors for happy and sad words (useful in aggregate(...) function)
positive <- c('happy', 'well-off', 'good', 'happiness')
negative <- c('sad', 'bad', 'miserable', 'terrible')

# Words to test sentiment
test <- c('I am a very happy person.', 'I am a very sad person',
          'I’ve always understood happiness to be appreciation. There is no greater happiness than appreciation for what one has- both physically and in the way of relationships and ideologies. The unhappy seek that which they do not have and can not fully appreciate the things around them. I don’t expect much from life. I don’t need a high paying job, a big house or fancy cars. I simply wish to be able to live my life appreciating everything around me.
')

# 1. Simple Summation
out <- classify.aggregate(test, positive, negative)
out

# 2. Naive Bayes
out <- classify.naivebayes(test)
out

#######################################################

# https://eight2late.wordpress.com/2015/07/22/a-gentle-introduction-to-cluster-analysis-using-r/


docs <- tm_map(docs, content_transformer(gsub),pattern = "organiz", replacement = "organ")
docs <- tm_map(docs, content_transformer(gsub), pattern = "organis", replacement = "organ")
docs <- tm_map(docs, content_transformer(gsub), pattern = "andgovern", replacement = "govern")
docs <- tm_map(docs, content_transformer(gsub), pattern = "inenterpris", replacement = "enterpris")
docs <- tm_map(docs, content_transformer(gsub), pattern = "team-", replacement = "team")

myStopwords <- c("can", "say","one","way","use",
                  +                  "also","howev","tell","will",
                  +                  "much","need","take","tend","even",
                  +                  "like","particular","rather","said",
                  +                  "get","well","make","ask","come","end",
                  +                  "first","two","help","often","may",
                  +                  "might","see","someth","thing","point",
                  +                  "post","look","right","now","think","’ve ",
                  +                  "’re ")
#remove custom stopwords
docs <- tm_map(docs, removeWords, myStopwords)

#######################################################

# http://www.rdatamining.com/examples/text-mining

# After that, the corpus needs a couple of transformations, including changing letters to lower case, removing punctuations/numbers and removing stop words.
# The general English stop-word list is tailored by adding "available" and "via" and removing "r".

myCorpus <- tm_map(myCorpus_Josh, tolower)
# remove punctuation
myCorpus <- tm_map(myCorpus, removePunctuation)
# remove numbers
myCorpus <- tm_map(myCorpus, removeNumbers)

# remove stopwords
# keep "r" by removing it from stopwords
myStopwords <- c(stopwords('english'), "available", "via")
idx <- which(myStopwords == "r")
myStopwords <- myStopwords[-idx]
myCorpus <- tm_map(myCorpus, removeWords, myStopwords)

# Stemming Words

# In many cases, words need to be stemmed to retrieve their radicals. For instance, "example" and "examples" are both stemmed to "exampl". However, after that, one may want to complete the stems to their original forms, so that the words would look "normal".

dictCorpus <- myCorpus
# stem words in a text document with the snowball stemmers,
# which requires packages Snowball, RWeka, rJava, RWekajars
myCorpus <- tm_map(myCorpus, stemDocument)
# inspect the first three ``documents"
inspect(myCorpus[1:3])

# stem completion
myCorpus <- tm_map(myCorpus, stemCompletion, dictionary=dictCorpus)


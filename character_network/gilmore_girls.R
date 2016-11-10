

for(i in 1:7){ # there are 7 seasons

  if(i == 1){

    for(j in 1:21){ # all seasons except the first have 22 episodes (the first has 21)

      cat("\nSeason", i, ", Episode", j, "\n")

      thepage <- readLines(paste0("http://www.crazy-internet-people.com/site/gilmoregirls/pages/s", i, "/s", i, "s/", j, ".html"))

      thepage <- thepage[grep("[[:upper:]]+:", thepage)]
      thepage <- gsub("\t", "", thepage)
      thepage <- gsub("<.*>", "", thepage)

      thepage <- as.data.frame(thepage)
      thepage$season <- i
      thepage$episode <- paste(i, j, sep = "_")

      if(i == 1 & j == 1){
        transcripts <- thepage
      } else {
        transcripts <- rbind(transcripts, thepage)
      }
    }

    } else {

      for(j in 1:22){ # all seasons except the first have 22 episodes (the first has 21)

        cat("\nSeason", i, ", Episode", j, "\n")

        if(i == 2){
          n <- j+21
        }

        if(i == 3){
          n <- j+21+22
        }

        if(i == 4){
          n <- j+21+22+22
        }

        if(i == 5){
          n <- j+21+22+22+22
        }

        if(i == 6){
          n <- j+21+22+22+22+22
        }

        if(i == 7){
          n <- j+21+22+22+22+22+22
        }

        thepage <- readLines(paste0("http://www.crazy-internet-people.com/site/gilmoregirls/pages/s", i, "/s", i, "s/", n, ".html"))

        thepage <- thepage[grep("[[:upper:]]{2,}:", thepage)]
        thepage <- gsub("\t", "", thepage)
        thepage <- gsub("<.*>", "", thepage)

        thepage <- as.data.frame(thepage)
        thepage$season <- i
        thepage$episode <- paste(i, j, sep = "_")

        if(i == 1 & j == 1){
          transcripts <- thepage
        } else {
          transcripts <- rbind(transcripts, thepage)
        }
        }
  }
}

# strip whitespace

library(tidyr)
transcripts <- separate(transcripts, "thepage", into = c("character", "dialogue"), sep = ":")

head(transcripts)
nrow(transcripts)

characters <- as.data.frame(table(transcripts$character))
head(characters)

head(count(transcripts, season, character))
head(count(transcripts, episode, character))

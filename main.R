library(tm)
library(openNLP)

# Uncomment the following lines if you wish to work with a smaller subset
# of the data for experimenting purposes
corpus <- VCorpus(DirSource("data/en_US/sandbox", encoding = "UTF-8"),
                                readerControl = list(language = "en_US"))

# Uncomment the following lines if you wish to work with the full dataset
# corpus <- PCorpus(DirSource("data/en_US", encoding = "UTF-8"),
#                                 readerControl = list(language = "en_US"),
#                                 dbControl = list(dbName = "en_US.db",
#                                                  dbType = "DB1"))

replace.non.ascii.punctuation <- function(x) {
  y <- gsub("[‘’]", "'", x)
  y <- gsub("[“”]", '"', y)
  y <- gsub("…", "...", y)
}

corpus <- tm_map(corpus, content_transformer(replace.non.ascii.punctuation))
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, stripWhitespace)
profanity_list <- readLines("data/en_US/profanity_list.txt")
corpus <- tm_map(corpus, removeWords, profanity_list)
rm(profanity_list)

sentences_by_line <- list()
sentences_by_line$blogs <- content(corpus[["en_US.blogs.txt"]])
sentences_by_line$news <- content(corpus[["en_US.news.txt"]])
sentences_by_line$twitter <- content(corpus[["en_US.twitter.txt"]])


sent_token_annotator <- Maxent_Sent_Token_Annotator()
split_sentences <- function(x) {
  x <- as.String(x)
  y <- annotate(x, sent_token_annotator)
  x[y]
}

sentences <- list()
for (i in seq_along(sentences_by_line)) {
  for (j in seq_along(sentences_by_line[[i]])) {
    sents <- split_sentences(sentences_by_line[[i]][j])
    if (j == 1) {
      sentences[[i]] <- sents
    } else {
      sentences[[i]] <- c(sentences[[i]], sents)
    }
  }
}
names(sentences) <- c("blogs", "news", "twitter")
rm(sentences_by_line)
library(ggplot2)
library(dplyr)

source("main.R")


sentences <- readLines(paste(base.dir, "sentences/train_sentences.txt", sep = "/"))
unigrams <- readLines(paste(base.dir, "ngrams/unigrams.txt", sep = "/"))
bigrams <- readLines(paste(base.dir, "ngrams/bigrams.txt", sep = "/"))
trigrams <- readLines(paste(base.dir, "ngrams/trigrams.txt", sep = "/"))

number_of_sentences <- length(sentences)

number_of_words <- length(unigrams)
number_of_bigrams <- length(bigrams)
number_of_trigrams <- length(trigrams)


count_words <- function(sentence) {
    length(scan_tokenizer(sentence))
}

words_per_sentence <- sapply(sentences, count_words)
qplot(words_per_sentence,
      xlab = "Nr. of words in sentence",
      ylab = "Count")


words_table <- table(unigrams)
words_table <- sort(words_table, decreasing = TRUE)

bigrams_table <- table(bigrams)
bigrams_table <- sort(bigrams_table, decreasing = TRUE)

trigrams_table <- table(trigrams)
trigrams_table <- sort(trigrams_table, decreasing = TRUE)


words_table_freq <- words_table / number_of_words
qplot(seq_along(words_table) / length(words_table), cumsum(words_table_freq),
      xlab = "Proportion of the total number of unique words",
      ylab = "Proportion of the total number of words")

bigrams_table_freq <- bigrams_table / number_of_bigrams
qplot(seq_along(bigrams_table) / length(bigrams_table), cumsum(bigrams_table_freq),
      xlab = "Proportion of the total number of unique bigrams",
      ylab = "Proportion of the total number of bigrams")

trigrams_table_freq <- trigrams_table / number_of_trigrams
qplot(seq_along(trigrams_table) / length(trigrams_table), cumsum(trigrams_table_freq),
      xlab = "Proportion of the total number of unique trigrams",
      ylab = "Proportion of the total number of trigrams")


head(words_table, n = 20)
head(bigrams_table, n = 20)
head(trigrams_table, n = 20)

tail(words_table, n = 20)
tail(bigrams_table, n = 20)
tail(trigrams_table, n = 20)

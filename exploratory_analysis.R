library(ggplot2)
library(dplyr)
library(tm)

# Set this constant to TRUE if you want to work with a smaller dataset
# for experimenting purposes
# Set this constant to FALSE to work with the full dataset
SANDBOX <- TRUE

if (SANDBOX) {
    base.dir <- "data/en_US/sandbox"
} else {
    base.dir <- "data/en_US"
}

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
qplot(log(as.vector(words_table_freq)),
      xlab = "Natural log of word frequency",
      ylab = "Count")
qplot(seq_along(words_table) / length(words_table), cumsum(words_table_freq),
      xlab = "Proportion of the total number of unique words",
      ylab = "Proportion of the total number of words")

bigrams_table_freq <- bigrams_table / number_of_bigrams
qplot(log(as.vector(bigrams_table_freq)),
      xlab = "Natural log of bigram frequency",
      ylab = "Count")
qplot(seq_along(bigrams_table) / length(bigrams_table), cumsum(bigrams_table_freq),
      xlab = "Proportion of the total number of unique bigrams",
      ylab = "Proportion of the total number of bigrams")

trigrams_table_freq <- trigrams_table / number_of_trigrams
qplot(log(as.vector(trigrams_table_freq)),
      xlab = "Natural log of trigram frequency",
      ylab = "Count")
qplot(seq_along(trigrams_table) / length(trigrams_table), cumsum(trigrams_table_freq),
      xlab = "Proportion of the total number of unique trigrams",
      ylab = "Proportion of the total number of trigrams")
trigrams_table <- table(trigrams)
trigrams_table <- table(trigrams)
trigrams_table <- sort(trigrams_table, decreasing = TRUE)
trigrams_table <- sort(trigrams_table, decreasing = TRUE)


head(words_table, n = 20)
head(bigrams_table, n = 20)
head(trigrams_table, n = 20)

tail(words_table, n = 20)
tail(bigrams_table, n = 20)
tail(trigrams_table, n = 20)

# Exploring ngrams that do not contain <s> or </s>
bigrams_no_sent_markers <- bigrams[!grepl("<s>|</s>", bigrams)]
bigrams_table_no_sent_markers <- table(bigrams_no_sent_markers)
bigrams_table_no_sent_markers <- sort(bigrams_table_no_sent_markers,
                                      decreasing = TRUE)
head(bigrams_table_no_sent_markers, n = 20)

trigrams_no_sent_markers <- trigrams[!grepl("<s>|</s>", trigrams)]
trigrams_table_no_sent_markers <- table(trigrams_no_sent_markers)
trigrams_table_no_sent_markers <- sort(trigrams_table_no_sent_markers,
                                      decreasing = TRUE)
head(trigrams_table_no_sent_markers, n = 20)

# Clear the environment
# rm(list = ls())

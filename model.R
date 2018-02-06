library(tidyverse)
library(tm)
library(Matrix)
library(hashmap)

# Set this constant to TRUE if you want to work with a smaller dataset
# for experimenting purposes
# Set this constant to FALSE to work with the full dataset
SANDBOX <- TRUE

if (SANDBOX) {
    base.dir <- "data/en_US/sandbox"
} else {
    base.dir <- "data/en_US"
}

unigrams <- readLines(paste(base.dir, "ngrams/unigrams.txt", sep = "/"))
words_table <- table(unigrams)
words_table <- sort(words_table, decreasing = TRUE)
# Keeping the environment as lean as possible
rm(unigrams)

replace_with_unk <- function(word) {
    if (word %in% names(words_table)) {
        "<UNK>"
    } else {
        word
    }
}


trigrams <- readLines(paste(base.dir, "ngrams/trigrams.txt", sep = "/"))

# TODO: remove line for smaller dataset
# Work with a smaller dataset for experimentation purposes
trigrams <- trigrams[1:5000]

tokenized_trigrams <- sapply(trigrams, scan_tokenizer, USE.NAMES = FALSE)
first_bigram_in_trigrams <- apply(tokenized_trigrams[1:2,],
                                  2, paste, collapse = " ")
third_words <- tokenized_trigrams[3,]
unique_bigrams <- unique(first_bigram_in_trigrams)
unique_next_words <- unique(third_words)

trigram_matrix <- Matrix(data = 0,
                         nrow = length(unique_bigrams),
                         ncol = length(unique_next_words),
                         dimnames = list(unique_bigrams,
                                         unique_next_words))

rows.hash <- hashmap(unique_bigrams, 1:length(unique_bigrams))
columns.hash <- hashmap(unique_next_words, 1:length(unique_next_words))
get.matrix.indices <- function(bigram, next.word) {
    i <- rows.hash[[bigram]] 
    j <- columns.hash[[next.word]] 
    c(i, j)
}

populate_trigram_matrix <- function(trigram) {
    tokens <- scan_tokenizer(trigram)
    bigram <- paste(tokens[1:2], collapse = " ")
    next.word <- tokens[3]
    indices <- get.matrix.indices(bigram, next.word)
    trigram_matrix[indices[1], indices[2]] <<- trigram_matrix[indices[1], indices[2]] + 1
}

trigrams %>% walk(populate_trigram_matrix)
trigram_matrix <- trigram_matrix / rowSums(trigram_matrix)


# # Clean the environment
# rm(tokenized_trigrams, trigrams, first_bigram_in_trigrams, third_words)

# trigrams_starting_with <- function(bigram) {
    

# }

# last_word <- function(text) {
#     x <- scan_tokenizer(text)
#     x[length(x)]
# }

# trigrams_by_bigram <- sapply(bigrams, trigrams_starting_with)



# predict_next_word <- function(sentence) {
#     tokens <- scan_tokenizer(sentence)
#     tokens <- sapply(tokens, replace_with_unk)
#     last_bigram <- paste(tail(tokens, 2), collapse = " ")
# }

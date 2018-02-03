library(tidyverse)
library(tm)
library(Matrix)

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
# Work with a smaller dataset for experimentation purposes
trigrams <- trigrams[1:100000]

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

# TODO: test the idea of using hash tables to make searching the matrix
# faster. The idea is creating two hash tables, one for rows and another
# one for columns. When a value is requested, search the hash tables to get
# the indices and then use those indices to get the value from the matrix.
# TODO: define a new class that inherits from sparseMatrix but defines a new
# method to get a value from the matrix. This new method will use hash tables
# (one for rows and another one for columns) to make searching the matrix
# faster.
populate_trigram_matrix <- function(trigram) {
    tokens <- scan_tokenizer(trigram)
    bigram <- paste(tokens[1:2], collapse = " ")
    next.word <- tokens[3]
    trigram_matrix[bigram, next.word] <<- trigram_matrix[bigram, next.word] + 1
}


trigrams %>% walk(populate_trigram_matrix)


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

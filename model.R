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
trigrams_table <- table(trigrams)
trigrams_table <- sort(trigrams_table, decreasing = TRUE)

trigrams_in_words <- sapply(names(trigrams_table), scan_tokenizer)
trigrams_in_words <- unname(trigrams_in_words)
rows_of_trigram_matrix <- trigrams_in_words[1:2,]
rows_of_trigram_matrix <- apply(rows_of_trigram_matrix, 2,
                                paste, collapse = " ")
cols_of_trigram_matrix <- trigrams_in_words[3,]


predict_next_word <- function(sentence) {
    tokens <- scan_tokenizer(sentence)
    tokens <- sapply(tokens, replace_with_unk)
    last_bigram <- paste(tail(tokens, 2), collapse = " ")
}

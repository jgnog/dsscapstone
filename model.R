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

# unigrams <- readLines(paste(base.dir, "ngrams/unigrams.txt", sep = "/"))
# words_table <- table(unigrams)
# words_table <- sort(words_table, decreasing = TRUE)
# # Keeping the environment as lean as possible
# rm(unigrams)

# replace_with_unk <- function(word) {
#     if (word %in% names(words_table)) {
#         "<UNK>"
#     } else {
#         word
#     }
# }

# TODO: generalize trigram matrix for bigrams and unigrams
unigrams <- readLines(paste(base.dir, "ngrams/unigrams.txt", sep = "/"))
bigrams <- readLines(paste(base.dir, "ngrams/bigrams.txt", sep = "/"))
trigrams <- readLines(paste(base.dir, "ngrams/trigrams.txt", sep = "/"))

# TODO: remove line for smaller dataset
# Work with a smaller dataset for experimentation purposes
unigrams <- unigrams[1:10]
bigrams <- bigrams[1:10]
trigrams <- trigrams[1:10]

ngrams <- list(unigrams = unigrams,
               bigrams = bigrams,
               trigrams = trigrams)

rm(unigrams, bigrams, trigrams)

decompose_ngram <- function(ngram) {
# This function decomposes an ngram into a two elements list: the first
# contains all the words except the last and the second element contains the last word.
#
# ngram   the character vector containing the ngram to be decomposed
    words <- scan_tokenizer(ngram)
    n_words <- length(words)
    # For the first element get all except the last element of vector words
    # and then paste the words together with a space between them.
    # For the second element simply get the last word in the vector words
    list(precedent = paste(words[1:n_words - 1], collapse = " "),
         subsequent = words[n_words])
}



decomposed_ngrams <- list(bigrams = transpose(ngrams$bigrams %>% map(decompose_ngram)),
                          trigrams = transpose(ngrams$trigrams %>% map(decompose_ngram)))
decomposed_ngrams <- list(bigrams = list(precedents = flatten_chr(decomposed_ngrams$bigrams[[1]]),
                                         subsequents = flatten_chr(decomposed_ngrams$bigrams[[2]])),
                          trigrams = list(precedents = flatten_chr(decomposed_ngrams$trigrams[[1]]),
                                          subsequents = flatten_chr(decomposed_ngrams$trigrams[[2]])))

decomposed_ngrams$bigrams <- list(precedents = unique(decomposed_ngrams$bigrams$precedents),
                                  subsequents = unique(decomposed_ngrams$bigrams$subsequents))
decomposed_ngrams$trigrams <- list(precedents = unique(decomposed_ngrams$trigrams$precedents),
                                   subsequents = unique(decomposed_ngrams$trigrams$subsequents))

# Compute the frequencies of unigrams
unigrams_count <- as.vector(table(ngrams$unigrams))
names(unigrams_count) <- sort(unique(ngrams$unigrams))
unigrams_freq <- unigrams_count / sum(unigrams_count)
rm(unigrams_count)


ngram_mt <- list(unigrams = unigrams_freq,
                 bigrams = Matrix(data = 0,
                                  nrow = length(decomposed_ngrams$bigrams$precedents),
                                  ncol = length(decomposed_ngrams$bigrams$subsequents),
                                  dimnames = list(decomposed_ngrams$bigrams$precedents,
                                                  decomposed_ngrams$bigrams$subsequents)),
                 trigrams = Matrix(data = 0,
                                   nrow = length(decomposed_ngrams$trigrams$precedents),
                                   ncol = length(decomposed_ngrams$trigrams$subsequents),
                                   dimnames = list(decomposed_ngrams$trigrams$precedents,
                                                   decomposed_ngrams$trigrams$subsequents)))

rm(decomposed_ngrams, unigrams_freq)

generate_hash <- function(v) {
    hashmap(v, 1:length(v))
}

matrices_hashes <- list(unigrams = generate_hash(ngram_mt$unigrams),
                        bigrams = list(precedents = generate_hash(rownames(ngram_mt$bigrams)),
                                       subsequents = generate_hash(colnames(ngram_mt$bigrams))),
                        trigrams = list(precedents = generate_hash(rownames(ngram_mt$trigrams)),
                                        subsequents = generate_hash(colnames(ngram_mt$trigrams))))

get_matrix_indices <- function(precedent, subsequent) {

    nr_of_words_in_precedent <- length(scan_tokenizer(precedent))

    if (nr_of_words_in_precedent == 2) {
        type_of_ngram <- "trigrams"
    } else if (nr_of_words_in_precedent == 1) {
        type_of_ngram <- "bigrams"
    } else {
        return(NULL)
    }

    rows_hash <- matrices_hashes[[type_of_ngram]][['precedents']]
    cols_hash <- matrices_hashes[[type_of_ngram]][['subsequents']]
    
    i <- rows_hash[[precedent]] 
    j <- cols_hash[[subsequent]] 

    c(i, j)
}

populate_matrix <- function(ngram, type_of_ngram) {
    decomp_ngram <- decompose_ngram(ngram)
    indices <- get_matrix_indices(decomp_ngram$precedent,
                                  decomp_ngram$subsequent)

    if (type_of_ngram == "bigram") {
        ngram_mt$bigrams[indices[1], indices[2]] <<- ngram_mt$bigrams[indices[1], indices[2]] + 1
    } else if (type_of_ngram == "trigram") {
        ngram_mt$trigrams[indices[1], indices[2]] <<- ngram_mt$trigrams[indices[1], indices[2]] + 1
    }

}

ngrams$bigrams %>% walk(populate_matrix, "bigram")
ngrams$trigrams %>% walk(populate_matrix, "trigram")

ngram_mt$bigrams <- ngram_mt$bigrams / rowSums(ngram_mt$bigrams)
ngram_mt$trigrams <- ngram_mt$bigrams / rowSums(ngram_mt$bigrams)


    



                          


# tokenized_trigrams <- sapply(trigrams, scan_tokenizer, USE.NAMES = FALSE)
# first_bigram_in_trigrams <- apply(tokenized_trigrams[1:2,],
#                                   2, paste, collapse = " ")
# third_words <- tokenized_trigrams[3,]
# unique_bigrams <- unique(first_bigram_in_trigrams)
# unique_next_words <- unique(third_words)

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

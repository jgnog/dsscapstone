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
bigrams <- readLines(paste(base.dir, "ngrams/bigrams.txt", sep = "/"))
trigrams <- readLines(paste(base.dir, "ngrams/trigrams.txt", sep = "/"))

# TODO: remove line for smaller dataset
# Work with a smaller dataset for experimentation purposes
# unigrams <- unigrams[1:10]
# bigrams <- bigrams[1:10]
# trigrams <- trigrams[1:10]

all_ngrams <- c(unigrams, bigrams, trigrams)

# Eliminate any empty strings in the ngrams list
all_ngrams <- all_ngrams[all_ngrams != ""]

rm(unigrams, bigrams, trigrams)

decompose_ngram <- function(ngram) {
# This function decomposes an ngram into a two elements list: the first
# contains all the words except the last and the second element contains the last word.
#
# ngram   the character vector containing the ngram to be decomposed


    words <- scan_tokenizer(ngram)
    n_words <- length(words)

    # If the ngram is a unigram we have a special case where the precedent is
    # set to the special symbol <NULL>
    if (n_words == 1) {

        list(precedent = "<NULL>",
             subsequent = ngram)

    } else {

        # For the first element get all except the last element of vector words
        # and then paste the words together with a space between them.
        # For the second element simply get the last word in the vector words
        list(precedent = paste(words[1:n_words - 1], collapse = " "),
             subsequent = words[n_words])
    }
}

decomp_ngrams <- all_ngrams %>% map(decompose_ngram) %>% transpose()
decomp_ngrams$precedent <- flatten_chr(decomp_ngrams$precedent)
decomp_ngrams$subsequent <- flatten_chr(decomp_ngrams$subsequent)

model_matrix <- Matrix(data = 0,
                       nrow = length(unique(decomp_ngrams$precedent)),
                       ncol = length(unique(decomp_ngrams$subsequent)),
                       dimnames = list(unique(decomp_ngrams$precedent),
                                       unique(decomp_ngrams$subsequent)))
 

generate_hash <- function(v) {
    hashmap(v, 1:length(v))
}

model_mat_hash <- list(precedent = generate_hash(rownames(model_matrix)),
                       subsequent = generate_hash(colnames(model_matrix)))

get_matrix_indices <- function(precedent, subsequent) {
    
    i <- model_mat_hash$precedent[[precedent]] 
    j <- model_mat_hash$subsequent[[subsequent]] 

    c(i, j)
}

populate_matrix <- function(ngram) {
    decomp_ngram <- decompose_ngram(ngram)
    indices <- get_matrix_indices(decomp_ngram$precedent,
                                  decomp_ngram$subsequent)

    # TODO: this is ugly! find a better way to do this.
    model_matrix[indices[1], indices[2]] <<- model_matrix[indices[1], indices[2]] + 1

}

# Count the occurences of each ngram and then divide it by the total counts
# of each precedent to obtain a frequency
all_ngrams %>% walk(populate_matrix)

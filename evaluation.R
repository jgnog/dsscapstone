# Evaluate using perplexity

# Set this constant to TRUE if you want to work with a smaller dataset
# for experimenting purposes
# Set this constant to FALSE to work with the full dataset
SANDBOX <- TRUE

if (SANDBOX) {
    base.dir <- "data/en_US/sandbox"
} else {
    base.dir <- "data/en_US"
}

# First we need a function to compute the probability of the next word in a
# sentence
prob_of_next_word <- function(sentence_words, next_word) {
    # sentence_words is a vector of words

    n_words <- length(sentence_words)

    # The highest order of ngrams in our model is 3, so we need to truncate
    # the sentence words to the last three in case they are more than 3
    if (n_words > 3) {
        sentence_words <- sentence_words[n_words - 2:n_words]
    }

    indices <- get_matrix_indices(sentence_words, next_word)
    # Compute the sum of the counts of the precedent's row
    # This will be used to compute to transform the count into
    # a frequency, i.e. a probability
    row_sum <- sum(model_matrix[indices[1]])
    model_matrix[indices[1], indices[2]] / row_sum
}


# Then we need a function to compute the probability of a sentence.
# That will be a product of the probabilities of each word in the sentence
prob_of_sentence <- function(sentence) {

    # First we need to break the sentence into words
    words <- scan_tokenizer(sentence)
    n_words <- length(words)

    # Create a vector to hold the results of the probabily calculations 
    # for each word of the sentence
    word_probs <- vector("numeric", n_words - 1)

    # Feed a growing sentence into the function prob_of_next_word
    for (i in seq_len(n_words) - 1) {
        sentence_words <- words[1:i]
        next_word <- words[i + 1]
        word_probs[i] <- prob_of_next_word(sentence_words, next_word)
    }

    # We take the base 2 logarithm of the probabilities of the words to
    # allows us to sum all the probabilites.
    word_probs <- log2(word_probs)
    sentence_probability <- sum(word_probs)
    # We exponentiate the result of the sum to get the true probability
    2 ^ sentence_probability

}

# Count the number of unique words in a set of sentences
size_of_vocab <- function(sentences) {
    words <- sentences %>% map(scan_tokenizer)    
    words <- flatten_chr(words)
    length(unique(words))
}

# Calculate the perplexity of the test corpus.
perplexity <- function(sentences) {
    probs <- sentences %>% map(prob_of_sentence)
    probs <- log2(probs)
    
    size_of_vocab <- size_of_vocab(sentences)

    2 ^ (sum(probs) / size_of_vocab)

}

# Replace the unseen words in the training vocab with <UNK>
unigrams <- readLines(paste(base.dir, "ngrams/unigrams.txt", sep = "/"))
valid_words <- unique(unigrams)
replace_with_unk <- function(word) {
    if (!(word %in% valid_words)) {
        "<UNK>"
    } else {
        word
    }
}

clean_sentence <- function(sentence) {
    words <- scan_tokenizer(sentence)
    words <- map(words, replace_with_unk)
    paste(words, collapse = " ")
}

test_sentences <- readLines(paste(base.dir,
                                  "sentences/test_sentences.txt",
                                  sep = "/"))
test_sentences <- map(test_sentences, clean_sentence)
perplexity <- perplexity(test_sentences)

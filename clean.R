library(tm)
library(rlist)
library(caret)
library(openNLP)

source("utilities.R")

# Set this constant to TRUE if you want to work with a smaller dataset
# for experimenting purposes
# Set this constant to FALSE to work with the full dataset
SANDBOX <- TRUE

if (SANDBOX) {
    base.dir <- "data/en_US/sandbox"
} else {
    base.dir <- "data/en_US"
}

# FUNCTION DEFINITIONS

get_clean_corpus <- function(sandbox = FALSE) {

    if (!file.exists(paste(base.dir, "clean/en_US.blogs.txt", sep = "/"))) {

        if (sandbox) {
            corpus <- VCorpus(DirSource(base.dir , encoding = "UTF-8"),
                                        readerControl = list(language = "en_US"))
        } else {
            corpus <- PCorpus(DirSource(base.dir, encoding = "UTF-8"),
                                        readerControl = list(language = "en_US"),
                                        dbControl = list(dbName = "en_US.db",
                                                         dbType = "DB1"))
        }

        replace.non.ascii.punctuation <- function(x) {
          y <- gsub("[‘’]", "'", x)
          y <- gsub("[“”]", '"', y)
          y <- gsub("…", "...", y)
        }

        corpus <- tm_map(corpus, content_transformer(replace.non.ascii.punctuation))
        corpus <- tm_map(corpus, removeNumbers)
        corpus <- tm_map(corpus, stripWhitespace)
        profanity_list <- readLines("data/en_US/profanity/profanity_list.txt")
        corpus <- tm_map(corpus, removeWords, profanity_list)
        rm(profanity_list)

        clean.dir <- paste(base.dir, "clean", sep = "/")
        dir.create(clean.dir)
        writeCorpus(corpus, path=clean.dir, c("en_US.blogs.txt"))
    } else {
        clean.dir <- paste(base.dir, "clean", sep = "/")
        if (sandbox) {
            corpus <- VCorpus(DirSource(clean.dir , encoding = "UTF-8"),
                                        readerControl = list(language = "en_US"))
        } else {
            corpus <- PCorpus(DirSource(clean.dir, encoding = "UTF-8"),
                              readerControl = list(language = "en_US"),
                              dbControl = list(dbName = "en_US.db",
                                               dbType = "DB1"))
        }
    }
    corpus
}

split_sentences <- function(x, annotator) {
    x <- as.String(x)
    y <- NLP::annotate(x, annotator)
    x[y]
}

# This removes all punctuation except for apostrophes
# This exception is important in order to keep the contractions
# typical of the english language (e.g. let's, can't)
remove_punctuation <- function(x) {
  y <- gsub("(?!')[[:punct:]]", "", x, perl = TRUE)
  gsub('[^[:alpha:][:space:]\'"«»?()&!,;.:-]', "", y)
}
 
insert_sent_annotations <- function(x) {
  paste("<s>", x, "</s>")
}

clean_sentence <- function(x) {
    y <- remove_punctuation(x)
    y <- trimws(y)
    y <- insert_sent_annotations(y)
}

ngrams_of_sentence <- function(sentence, n) {
  tokens_sent <- scan_tokenizer(sentence)
  vapply(ngrams(tokens_sent, n), paste, "", collapse = " ")
}

write_sentences <- function(line, sentences.file, annotator) {
    sents <- split_sentences(line, annotator)
    sents <- sapply(sents, clean_sentence)
    write(sents, sentences.file)
}

write_ngrams <- function(sentence,
                         bigrams.file,
                         trigrams.file) {
     write(ngrams_of_sentence(sentence, 2), bigrams.file)
     write(ngrams_of_sentence(sentence, 3), trigrams.file)
}

write_unigrams <- function(sentence, unigrams.file) {
     tokens <- scan_tokenizer(sentence)
     write(tokens, unigrams.file)
}

sentence_from_start_and_end <- function(start, end) {
    paste(unigrams[start:end], collapse = " ")
}

replace_with_unk <- function(word) {
    if (word %in% words_to_replace) {
        # Remove found word from list of words to replace to make
        # the search faster
        words_to_replace <- words_to_replace[!words_to_replace==word]
        "<UNK>"
    } else {
        word
    }
}

# MAIN BODY

corpus <- get_clean_corpus(sandbox = SANDBOX)

sent_token_annotator <- Maxent_Sent_Token_Annotator()

dir.create(paste(base.dir, "sentences", sep = "/"))

if (!file.exists(paste(base.dir, "sentences/sentences.txt", sep = "/"))) {
    sentences <- file(paste(base.dir, "sentences/sentences.txt", sep = "/"), "w")

    for (doc in seq_along(corpus)) {
        for (line in seq_along(content(corpus[[doc]]))) {
            line <- content(corpus[[doc]])[line]
            write_sentences(line, sentences, sent_token_annotator)
        }
    }
    close(sentences)
}


if (!file.exists(paste(base.dir, "sentences/train_sentences.txt", sep = "/"))) {
    # First divide into training and test datasets
    sentences_file <- paste(base.dir, "sentences/sentences.txt", sep = "/")
    n_sentences <- count_lines(sentences_file)
    set.seed(12345678)
    training_indices <- createDataPartition(seq(1, n_sentences), p = 0.8)
    training_indices <- training_indices[[1]]

    sentences <- readLines(sentences_file)

    train_sentences <- sentences[training_indices]
    test_sentences <- sentences[-training_indices]

    write(train_sentences, paste(base.dir, "sentences/train_sentences.txt", sep = "/"))
    write(test_sentences, paste(base.dir, "sentences/test_sentences.txt", sep = "/"))

    # Now divide the training sentences into training and validation datasets
    sentences_file <- paste(base.dir, "sentences/train_sentences.txt", sep = "/")
    n_sentences <- count_lines(sentences_file)
    training_indices <- createDataPartition(seq(1, n_sentences), p = 0.8)
    training_indices <- training_indices[[1]]

    sentences <- readLines(sentences_file)

    train_sentences <- sentences[training_indices]
    validation_sentences <- sentences[-training_indices]

    write(train_sentences, paste(base.dir, "sentences/train_sentences.txt", sep = "/"))
    write(validation_sentences, paste(base.dir, "sentences/validation_sentences.txt", sep = "/"))
}


if (!file.exists(paste(base.dir, "ngrams/unigrams.txt", sep = "/"))) {
    dir.create(paste(base.dir, "ngrams", sep = "/"))
    unigrams.file <- file(paste(base.dir, "ngrams/unigrams.txt", sep = "/"), "w")
    train_sentences <- readLines(paste(base.dir, "sentences/train_sentences.txt", sep = "/"))  
    sapply(train_sentences, write_unigrams, unigrams.file)
}

unigrams <- readLines(paste(base.dir, "ngrams/unigrams.txt", sep = "/"))
words_table <- table(unigrams)
words_to_replace <- names(words_table[words_table == 1])
rm(words_table)


unigrams <- sapply(unigrams, replace_with_unk)
# Delete the names of the vector to save memory
unigrams <- unname(unigrams)
# Write the list of words with <UNK> to file
write(unigrams, paste(base.dir, "ngrams/unigrams.txt", sep = "/"))

# Build list of sentences from list of words
start_of_sentences <- which(unigrams=="<s>")
end_of_sentences <- which(unigrams=="</s>")
sentences <- mapply(sentence_from_start_and_end,
                    start_of_sentences,
                    end_of_sentences)
# Exclude empty sentences
sentences <- sentences[!sentences == "<s> </s>"]
sentences <- unname(sentences)

# Calculate and write to file the bigrams and trigrams of the
# training sentences
if (!file.exists(paste(base.dir, "ngrams/bigrams.txt", sep = "/"))) {
    bigrams.file <- file(paste(base.dir, "ngrams/bigrams.txt", sep = "/"), "w")
    trigrams.file <- file(paste(base.dir, "ngrams/trigrams.txt", sep = "/"), "w")
    sapply(sentences, write_ngrams, bigrams.file, trigrams.file)
}

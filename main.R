library(tm)
library(openNLP)
library(rlist)
library(caret)

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
        writeCorpus(corpus, path=clean.dir,
                    c("en_US.blogs.txt",
                      "en_US.news.txt",
                      "en_US.twitter.txt")
                    )
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

corpus <- get_clean_corpus(sandbox = SANDBOX)


sent_token_annotator <- Maxent_Sent_Token_Annotator()
split_sentences <- function(x) {
  x <- as.String(x)
  y <- annotate(x, sent_token_annotator)
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

write_sentences <- function(line, sentences.file) {
    sents <- split_sentences(line)
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

dir.create(paste(base.dir, "sentences", sep = "/"))

if (!file.exists(paste(base.dir, "sentences/sentences.txt", sep = "/"))) {
    sentences <- file(paste(base.dir, "sentences/sentences.txt", sep = "/"), "w")

    for (doc in seq_along(corpus)) {
        for (line in seq_along(content(corpus[[doc]]))) {
            line <- content(corpus[[doc]])[line]
            write_sentences(line, sentences)
        }
    }
    close(sentences)
}


if (!file.exists(paste(base.dir, "sentences/train_sentences.txt", sep = "/"))) {
    sentences.file <- paste(base.dir, "sentences/sentences.txt", sep = "/")
    n_sentences <- count_lines(sentences.file)
    set.seed(12345678)
    training.indices <- createDataPartition(seq(1, n_sentences), p = 0.6)
    training.indices <- training.indices[[1]]

    train_sentences <- file(paste(base.dir, "sentences/train_sentences.txt", sep = "/"), "w")
    test_sentences <- file(paste(base.dir, "sentences/test_sentences.txt", sep = "/"), "w")
    sentences <- file(sentences.file, "r")

    CHUNKSIZE <- 20000

    linesread <- readLines(sentences, CHUNKSIZE)
    nolinesread_previous <- 0
    while((nolinesread <- length(linesread)) > 0) {
        adjusted_training_indices <- training.indices[
                          training.indices <= nolinesread + nolinesread_previous]
        adjusted_training_indices <- adjusted_training_indices - nolinesread_previous
        adjusted_training_indices <- adjusted_training_indices[
                                               adjusted_training_indices > 0]
        write(linesread[adjusted_training_indices], train_sentences)
        write(linesread[-adjusted_training_indices], test_sentences)
        nolinesread_previous <- nolinesread_previous + nolinesread
        linesread <- readLines(sentences, CHUNKSIZE)
    }
    close(sentences)
    close(test_sentences)
    close(train_sentences)
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

unigrams <- sapply(unigrams, replace_with_unk)
# Delete the names of the vector to save memory
unigrams <- unname(unigrams)
# Write the list of words with <UNK> to file
write(unigrams, paste(base.dir, "ngrams/unigrams.txt", sep = "/"))

# Build list of sentences from list of words
start_of_sentences <- which(unigrams=="<s>")
end_of_sentences <- which(unigrams=="</s>")
sentence_from_start_and_end <- function(start, end) {
    paste(unigrams[start:end], collapse = " ")
}
sentences <- mapply(sentence_from_start_and_end,
                    start_of_sentences,
                    end_of_sentences)
sentences <- unname(sentences)

write(sentences, paste(base.dir, "sentences/train_sentences.txt", sep = "/"))

# Calculate and write to file the bigrams and trigrams of the
# training sentences
bigrams.file <- file(paste(base.dir, "ngrams/bigrams.txt", sep = "/"), "w")
trigrams.file <- file(paste(base.dir, "ngrams/trigrams.txt", sep = "/"), "w")
sapply(sentences, write_ngrams, bigrams.file, trigrams.file)

# Clear the environment
rm(list = ls())

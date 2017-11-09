library(tm)
library(openNLP)
library(rlist)


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
  gsub("(?!')[[:punct:]]", "", x, perl = TRUE)
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
                         unigrams.file,
                         bigrams.file,
                         trigrams.file) {
     tokens <- scan_tokenizer(sentence[i])
     write(tokens, unigrams.file)
     write(ngrams_of_sentence(sentence, 2), bigrams.file)
     write(ngrams_of_sentence(sentence, 3), trigrams.file)
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
}

library(tm)
library(hunspell)

random.text.file.sample <- function(filepath, proportion.to.keep) {
  file.con <- file(filepath, "r")
  temp.file <- file("", "w+")
  while (TRUE) {
    line <- readLines(file.con, n = 1)
    if (length(line) == 0) {
      break
    }
    else if (rbinom(1, 1, proportion.to.keep) == 1) {
      writeLines(line, temp.file)
    }
  }
  sample <- readLines(temp.file)
  close(file.con)
  close(temp.file)
  sample
}

PROPORTION.OF.SANDBOX.DATA <- 0.001
en_US.docs.location <- c("data/en_US")
en_US.documents <- c("en_US.blogs.txt",
                     "en_US.news.txt",
                     "en_US.twitter.txt")

en_US.sandbox.location <- file.path(en_US.docs.location, "sandbox")
dir.create(en_US.sandbox.location, showWarnings = FALSE)

set.seed(12345678)
for (i in seq_along(en_US.documents)) {
  full.doc.path <- file.path(en_US.docs.location, en_US.documents[i])
  sandbox.doc <- random.text.file.sample(full.doc.path, PROPORTION.OF.SANDBOX.DATA)
  sandbox.filepath <- file.path(en_US.sandbox.location, en_US.documents[i])
  writeLines(sandbox.doc, sandbox.filepath)
  rm(sandbox.doc)
}

en.us.sandbox.corpus <- VCorpus(DirSource("data/en_US/sandbox", encoding = "UTF-8"),
                                readerControl = list(language = "en_US"))

# This function removes all punctuation except for apostrophes
# This exception is important because we want to keep the contractions
# of the English language that use this character (e.g. let's, can't)
remove.punctuation <- function(x) {
  gsub("(?!')[[:punct:]]", "", x, perl = TRUE)
}

replace.curly.apostrophes <- function(x) {
  gsub("’", "'", x)
}

replace.curly.quote.marks <- function(x) {
  gsub("[“”]", '"', x)
}

en.us.sandbox.corpus <- tm_map(en.us.sandbox.corpus, content_transformer(tolower))
en.us.sandbox.corpus <- tm_map(en.us.sandbox.corpus, content_transformer(replace.curly.apostrophes))
en.us.sandbox.corpus <- tm_map(en.us.sandbox.corpus, content_transformer(replace.curly.quote.marks))
en.us.sandbox.corpus <- tm_map(en.us.sandbox.corpus, content_transformer(remove.punctuation))
en.us.sandbox.corpus <- tm_map(en.us.sandbox.corpus, removeNumbers)
en.us.sandbox.corpus <- tm_map(en.us.sandbox.corpus, stripWhitespace)
profanity_list <- readLines("data/en_US/profanity_list.txt")
en.us.sandbox.corpus <- tm_map(en.us.sandbox.corpus, removeWords, profanity_list)

sandbox.tokens <- character()
for (i in seq_along(en.us.sandbox.corpus)) {
  sandbox.tokens <- c(sandbox.tokens, scan_tokenizer(en.us.sandbox.corpus[[i]]))
}
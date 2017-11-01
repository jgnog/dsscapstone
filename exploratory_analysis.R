source("main.R")

# Some basic characteristics of the data
number_of_lines_in_doc <- function(textdoc) {
  length(content(textdoc))
}

number_of_lines <- sapply(en.us.sandbox.corpus, number_of_lines_in_doc)
names(number_of_lines) <- c("blogs", "news", "twitter")
number_of_tokens <- sapply(tokens, length)
n_char_in_doc <- function(textdoc) {
  sum(sapply(content(textdoc), nchar))
}
number_of_characters <- sapply(en.us.sandbox.corpus, n_char_in_doc)

data_characteristics <- data.frame(lines = number_of_lines,
                                   tokens = number_of_tokens,
                                   characters = number_of_characters)
print(data_characteristics)


# This file contains some utility function for the project

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


create_sandbox_corpus <- function(proportion) {
    en_US.docs.location <- c("data/en_US")
    en_US.documents <- c("en_US.blogs.txt",
                         "en_US.news.txt",
                         "en_US.twitter.txt")
    
    en_US.sandbox.location <- file.path(en_US.docs.location, "sandbox")
    dir.create(en_US.sandbox.location, showWarnings = FALSE)

    set.seed(12345678)
    for (i in seq_along(en_US.documents)) {
      full.doc.path <- file.path(en_US.docs.location, en_US.documents[i])
      sandbox.doc <- random.text.file.sample(full.doc.path, proportion)
      sandbox.filepath <- file.path(en_US.sandbox.location, en_US.documents[i])
      writeLines(sandbox.doc, sandbox.filepath)
      rm(sandbox.doc)
    }
}

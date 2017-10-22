longest_line <- function(category) {
  filename <- paste("en_US.", category, ".txt", sep = "")
  filepath <- file.path("data", "en_US", filename)
  file.con <- file(filepath, "r")
  result <- 0
  while (TRUE) {
    line <- readLines(file.con, n = 1)
    length.line <- nchar(line)
    if (length(line) == 0) {
      break
    }
    else if (length.line > result) {
      result <- length.line
    }
  }
  close(file.con)
  result
}

blogs.longest.line <- longest_line("blogs")
news.longest.line <- longest_line("news")
twitter.longest.line <- longest_line("twitter")

love.by.hate <- function() {
  file.con <- file("data/en_US/en_US.twitter.txt", "r")
  love.counter <- 0
  hate.counter <- 0
  while (TRUE) {
    line <- readLines(file.con, n = 1)
    if (length(line) == 0) {
      break
    }
    else if (grepl("love", line)) {
      love.counter <- love.counter + 1
    }
    else if (grepl("hate", line)) {
      hate.counter <- hate.counter + 1
    }
  }
  love.counter / hate.counter
}

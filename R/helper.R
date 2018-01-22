ext.ymd <- function(age) {
  year <- month <- day <- 0
  y <- gsub("y.*$", "\\1", age)
  if (grepl("^[0-9]+$", y)) {
    year <- as.integer(y)
    age <- gsub(paste0(y, "y"), "", age)
  } else {
    age <- y
  }
  
  m <- gsub("m.*$", "\\1", age)
  if (grepl("^[0-9]+$", m)) {
    month <- as.integer(m)
    age <- gsub(paste0(m, "m"), "", age)
  } else {
    age <- m
  }
  
  if (nchar(age) > 0) {
    d <- gsub("d.*$", "\\1", age)
    day <- as.integer(d)
  }
  return(c(year = year, month = month, day = day))
}

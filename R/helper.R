# for a given control, find the matched case from cases
match.single <- function(control, cases, birthYMD.threshold = 15) {
  control.id <- control$id
  x <- subset(cases, sex == control$sex & province == control$province & prefecture == control$prefecture & abs(birthYMD - control$birthYMD) <= birthYMD.threshold)
  if (nrow(x) == 0) {
    case.id <- NA
  } else if (nrow(x) == 1) {
    case.id <- x$id
  } else {
    x1 <- subset(x, !is.na(county) & county == control$county)
    if (nrow(x1) == 0) {
      idx <- which.min(abs(x$birthYMD - control$birthYMD))
      case.id <- x[idx, "id"]
    } else if (nrow(x1) == 1) {
      case.id <- x1$id
    } else {
      idx <- which.min(abs(x1$birthYMD - control$birthYMD))
      case.id <- x1[idx, "id"]
    }
  }
  return(data.frame(control.id, case.id))
}

# match controls with cases
match <- function(controls, cases, birthYMD.threshold = 15) {
  id.lookup <- data.frame(control.id = NULL, case.id = NULL)
  
  for (i in 1:nrow(controls)) {
    control.idx <- controls[i, ]
    cases.remain <- subset(cases, !id %in% id.lookup$case.id)
    ret <- match.single(control.idx, cases.remain, birthYMD.threshold = 15)
    id.lookup <- rbind(id.lookup, ret)
  }
  return(id.lookup)
}

# extract year, month and day from age strings
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

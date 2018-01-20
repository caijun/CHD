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


# reference url: https://rpubs.com/Koundy/71792
theme_publication <- function(base_size = 14, base_family = "Helvetica") {
  library(grid)
  library(ggthemes)
  (theme_foundation(base_size = base_size, base_family = base_family) 
    + theme(plot.title = element_text(face = "bold", size = rel(1.2), hjust = 0.5),
            text = element_text(),
            panel.background = element_rect(colour = NA),
            plot.background = element_rect(colour = NA),
            panel.border = element_rect(colour = NA),
            axis.title = element_text(face = "bold", size = rel(1)),
            axis.title.y = element_text(angle = 90, vjust = 2),
            axis.title.x = element_text(vjust = -0.2),
            axis.text = element_text(), 
            axis.line = element_line(colour = "black"),
            axis.ticks = element_line(),
            panel.grid.major = element_line(colour = "#f0f0f0"),
            panel.grid.minor = element_blank(),
            legend.key = element_rect(colour = NA),
            legend.position = "bottom",
            legend.direction = "horizontal",
            legend.key.size = unit(0.2, "cm"),
            legend.spacing = unit(0, "cm"),
            legend.title = element_text(face = "italic"),
            plot.margin = unit(c(10, 5, 5, 5), "mm"),
            strip.background = element_rect(colour = "#f0f0f0", fill = "#f0f0f0"),
            strip.text = element_text(face = "bold")
    ))
}
### Run first script

### Clear workspace (optional)

# rm(list = ls())

###################
#### Libraries ----

# Package names
packages <- c("remotes", "tidyverse", "reshape2", "data.table", "DBI", "mfdb", "gadget3", "cowplot")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())

if (any(installed_packages == FALSE)) {

  if("remotes" %in% packages[!installed_packages]) {
    install.packages("remotes")
  }

  if("gadget3" %in% packages[!installed_packages]) {
    remotes::install_github("gadget-framework/gadget3", upgrade = "never")
  }

  installed_packages <- packages %in% rownames(installed.packages())

  install.packages(packages[!installed_packages])
}

# Packages loading
invisible(lapply(packages, library, character.only = TRUE))

rm(packages, installed_packages)

## Dplyr options

options(dplyr.summarise.inform = FALSE)

##########################
## Function shortcuts ----

h <- head

########################
## Custom functions ----

## Intervals for gadged ldist files. From:

create_intervals <- function (prefix, vect) {

  x <- structure(vect[1:(length(vect)-1)],
                 names = paste0(prefix, vect[1:(length(vect)-1)])) %>%
    as.list(.) %>%
    purrr::map(~structure(seq(.,vect[-1][which(vect[1:(length(vect)-1)]==.)],1)[-length(seq(.,vect[-1][which(vect[1:(length(vect)-1)]==.)],1))],
                          min = .,
                          max = vect[-1][which(vect[1:(length(vect)-1)]==.)]))

  x[[length(x)]] <- c(x[[length(x)]], attributes(x[[length(x)]])$max) %>%
    structure(.,
              min = min(.),
              max = max(.))


  return(x)
}

#' @title Back-transform predictor variables from a logit model

unlogit <- function(p, model) {
  mean <- unname((log(p/(1 - p)) - coef(model)[1])/coef(model)[2])

  tmp.cis <- suppressMessages(confint(model))

  ci.max <- unname((log(p/(1 - p)) - tmp.cis[1])/tmp.cis[2])
  ci.min <- unname((log(p/(1 - p)) - tmp.cis[3])/tmp.cis[4])

  data.frame(mean = mean, ci.min = ci.min, ci.max = ci.max)
}


### Read clipboard for Mac

read.clipboard <- function() read.table(pipe("pbpaste"), sep="\t", header=T)

### Standard error of mean

#' @title Standard error of mean
#' @param x numeric vector

se <- function (x){
  sd(x, na.rm = T)/sqrt(sum(!is.na(x)))}

### Column numbers of a data frame

#' @title Column numbers of a data frame
#' @param x data.frame
#' @return retuns a named vector with column names as names and order of columns as elements
#' @author Mikko Vihtakari

coln <- function (x)
{
  y <- rbind(seq(1, ncol(x)))
  colnames(y) <- colnames(x)
  rownames(y) <- "col.number"
  return(y)
}

### Check colors

#' @title Plot color vector to inspect the colors visually
#' @param cols a character vector containing accepted R \link[grDevices]{colors}
#' @return returns a base R plot with the colors given in \code{cols} argument
#' @author Mikko Vihtakari

check_cols <- function(cols) {

  if (is.null(names(cols))) {
    labs <- seq_along(cols)
  } else {
    labs <- paste0(names(cols), "\n[", seq_along(cols), "]")
  }

  mp <- barplot(rep(1, length(cols)), yaxt = "n", col = cols, border = NA, names.arg = labs, xlab = "Color sequence",  ylim = c(0,1.2))
  points(mp, rep(1.1, length(cols)), col = cols, pch = 16, cex = 4)
}

### Select

#' @title Select an element of each vector from a list
#' @description Selects y'th element of each vector from a list
#' @param x list
#' @param y number of element. Must be integer

select.element <- function(x,y) sapply(x, "[", y)

### round_any

#' @title Round to multiple of any number
#' @param x numeric vector to round
#' @param accuracy number to round to; for POSIXct objects, a number of seconds
#' @param f rounding function: \code{\link{floor}}, \code{\link{ceiling}} or
#'  \code{\link{round}}
#' @keywords internal

round_any <- function(x, accuracy, f = round) {
  f(x / accuracy) * accuracy
}

### Font and line size conversions for ggplot2

#' @title Convert font sizes measured as points to ggplot font sizes
#' @description Converts font sizes measured as points (as given by most programs such as MS Word etc.) to ggplot font sizes
#' @param x numeric vector giving the font sizes in points
#' @return Returns a numeric vector of lenght \code{x} of ggplot font sizes
#' @author Mikko Vihtakari

FS <- function(x) x/2.845276

#' @title Convert line sizes measured as points to ggplot line sizes
#' @description Converts line sizes measured as points (as given by most programs such as Adobe Illustrator etc.) to ggplot font sizes
#' @param x numeric vector giving the lines sizes in points
#' @return Returns a numeric vector of lenght \code{x} of ggplot line sizes
#' @author Mikko Vihtakari

LS <- function(x) x/2.13

###################
## Definitions ----

### Sizes and definitions for figures in Frontiers in Marine Science

colwidth <- 85 # mm
pagewidth <- 180 # mm
unit <- "mm"

colwidth_in <- colwidth * 0.0393701
pagewidth_in <- pagewidth * 0.0393701

### ggplot theme

theme_cust <- theme_classic(base_size = 11) %+replace%
  theme(strip.background = element_blank(),
        panel.background = element_blank(),
        plot.background = element_blank(),
        legend.background = element_blank(),
        legend.box.background = element_blank(),
        plot.margin = margin(c(5.5, 10, 5.5, 5.5)))

theme_set(theme_cust) # Set default theme globally for the entire project

####################
## Color themes ----

### Functions to lighten and darken colors, source: https://gist.github.com/Jfortin1/72ef064469d1703c6b30

darken <- function(color, factor = 1.2){
  col <- col2rgb(color)
  col <- col/factor
  col <- rgb(t(col), maxColorValue = 255)
  col
}


lighten <- function(color, factor = 1.2){
  col <- col2rgb(color)
  col <- col*factor
  col[col > 255] <- 255
  col <- rgb(t(col), maxColorValue = 255)
  col
}

### Vector of standard colors

cols <- c("#D696C8", "#449BCF", "#82C893", "#FF5F68", "#FF9252", "#FFC95B", "#056A89")

# check_cols(cols)

size_colors <- cols[1:3]
size_hues <- unlist(lapply(size_colors, function(k) c(lighten(k), k, darken(k))))

## End ----

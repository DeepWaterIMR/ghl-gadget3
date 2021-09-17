## ---------------------------
##
## Script name: Figure functions for the Ghl Gadget project
##
## Purpose of script:
##
## Author: Mikko Vihtakari // Institute of Marine Research, Norway
## Email: mikko.vihtakari@hi.no
##
## Date Created: 2021-06-17
##
## ---------------------------

## Source the run first script

# source("0 run first.R")

## ---------------------------

## Load packages

## ---------------------------

## Source or list custom functions used within the script

## ---------------------------

## Maturity ogive

plot.mat <- function(x, quarterly = all(names(model_params$timestep_fun) == 1:4)) {

  x$Length <- as.integer(gsub("len", "", x$length))

  if(quarterly) {
    x$Date <- zoo::as.yearqtr(paste(x$year, x$step, sep = "Q"))
  } else {
    x$Date <- x$year
  }

  x <- x %>% arrange(Date, maturity_stage, Length)

  ggplot(x, aes(x = Length, y = number, color = maturity_stage)) +
    geom_path() +
    facet_wrap(~as.character(Date)) +
    labs(x = "Length (cm)", y = "Count", color = "Stock") +
    scale_color_manual(values = c("female_imm" = "tomato1", "female_mat" = "tomato4", "male_imm" = "dodgerblue1", "male_mat" = "dodgerblue4")) +
    theme(legend.position = "bottom")
}

plot.matp <- function(x, quarterly = all(names(model_params$timestep_fun) == 1:4)) {

  x <- x %>% complete(crossing(year, step, area, maturity_stage, age, length), fill = list(number = 0))

  x$Length <- as.integer(gsub("len", "", x$length))
  x$Sex <- gsub("\\_.*$", "", x$maturity_stage)
  x$Stage <- gsub("^.*\\_", "", x$maturity_stage)

  if(quarterly) {
    x$Date <- zoo::as.yearqtr(paste(x$year, x$step, sep = "Q"))
  } else {
    x$Date <- x$year
  }

  x <- x %>% group_by(year, step, Date, area, Sex, Length) %>%
    summarise(n_imm = sum(number[Stage == "imm"]), n_mat = sum(number[Stage == "mat"])) %>%
    mutate(mat = n_mat/(n_imm + n_mat), imm = n_imm/(n_imm + n_mat)) %>%
    #replace_na(list(mat = 0, imm = 0)) %>%
    pivot_longer(cols = c(mat, imm)) %>%
    mutate(maturity_stage = paste(Sex, name, sep = "_")) %>%
    filter(!is.na(value))

  ggplot(x, aes(x = Length, y = value, color = maturity_stage)) +
    geom_path() +
    facet_wrap(~as.character(Date)) +
    labs(x = "Length (cm)", y = "Count", color = "Stock") +
    scale_color_manual(values = c("female_imm" = "tomato1", "female_mat" = "tomato4", "male_imm" = "dodgerblue1", "male_mat" = "dodgerblue4")) +
    theme(legend.position = "bottom")
}

## Length distributions

plot.ldist <- function(x, quarterly = all(names(model_params$timestep_fun) == 1:4)) {

  x$Length <- as.integer(gsub("len", "", x$length))

  if(quarterly) {
    x$Date <- zoo::as.yearqtr(paste(x$year, x$step, sep = "Q"))
  } else {
    x$Date <- x$year
  }

  x <- x %>% arrange(Date, Length)

  ggplot(x, aes(x = Length, y = number, color = step)) +
    geom_path() +
    facet_wrap(~year, scales = "free_y", ncol = 4, dir = "v") +
    labs(x = "Length (cm)", y = "Count", color = "Timestep") +
    theme(legend.position = "bottom")
}

## Age-Length distributions

# x <- EggaN_adist
plot.adist <- function(x, quarterly = all(names(model_params$timestep_fun) == 1:4)) {

  x$Length <- as.integer(gsub("len", "", x$length))
  x$Age <- as.integer(gsub("age", "", x$age))

  if(quarterly) {
    x$Date <- zoo::as.yearqtr(paste(x$year, x$step, sep = "Q"))
  } else {
    x$Date <- x$year
  }

  x <- x %>% arrange(Date, Age, Length)

  ggplot(x, aes(x = Length, y = number, fill = Age)) +
    geom_col() +
    facet_grid(Age~as.character(Date)) + # , scales = "free_y", ncol = 4, dir = "v"
    labs(x = "Length (cm)", y = "Count", fill = "Age") +
    scale_fill_viridis_c() +
    theme(legend.position = "bottom")
}


## Landings

plot.landings <- function(x, quarterly = all(names(model_params$timestep_fun) == 1:4)) {

  if(nrow(x) == 0) {
    return({
      ggplot() +
        geom_blank() +
        annotate("text", x = 0.5, y = 0.5, label = "No data", size = FS(12), fontface = 2) +
        labs(x = "Year", y = "Total weight (kg)") +
        theme(axis.text = element_blank())
    })
  }

  if(quarterly) {
    x$date <- zoo::as.yearqtr(paste(x$year, x$step, sep = "Q"))
  } else {
    stop("Other time steps than quarterly have not been implemented.")
  }

  ggplot(x, aes(x = date, y = total_weight)) +
    geom_col(fill = "#449BCF") +
    scale_x_continuous(
      breaks = seq(min(x$year), max(x$year), 2)) +
    coord_cartesian(expand = FALSE) +
    labs(x = "Year", y = "Total weight (kg)")
}

# Maturity percentage

#' @param filter.exp Expression to filter data. E.g. 'sampling_type == "ENS"'
#' @param plot Logical indicated whether plot should be generated. If \code{FALSE}, L50 values will be returned instead
plot.maturity <- function(filter.exp = NULL, plot = TRUE) {

  if(is.null(filter.exp)) {
    dt <- mfdb_dplyr_sample(mdb) %>%
      filter(!is.na(length), !is.na(sex), !is.na(maturity_stage)) %>%
      select(year, month, areacell, age, sex, maturity_stage, length) %>%
      collect() %>%
      mutate(maturity = as.integer(maturity_stage > 3))
  } else {
    dt <- mfdb_dplyr_sample(mdb) %>%
      filter(!is.na(length), !is.na(sex), !is.na(maturity_stage)) %>%
      filter(!!rlang::parse_expr(filter.exp)) %>%
      select(year, month, areacell, age, sex, maturity_stage, length) %>%
      collect() %>%
      mutate(maturity = as.integer(maturity_stage > 3))
  }

  modF <- glm(maturity ~ length, data = dt[dt$sex == "F",],
              family = binomial(link = "logit"))
  modM <- glm(maturity ~ length, data = dt[dt$sex == "M",],
              family = binomial(link = "logit"))

  Fdat <- unlogit(0.5, modF)
  Fdat$sex <- "F"
  Mdat <- unlogit(0.5, modM)
  Mdat$sex <- "M"
  modDat <- rbind(Fdat, Mdat)
  modDat <- left_join(modDat, dt %>% group_by(sex) %>% count, by = "sex")

  if(plot) {

  ggplot() +
    geom_point(data = dt, aes(x = length, y = maturity, shape = sex)) +
    geom_segment(data = modDat,
                 aes(x = mean, xend = mean, y = 0, yend = 0.5, color = sex),
                 linetype = 2) +
    geom_segment(data = modDat,
                 aes(x = -Inf, xend = mean, y = 0.5, yend = 0.5, color = sex),
                 linetype = 2) +
    geom_errorbarh(data = modDat,
                   aes(xmin = ci.min, xmax = ci.max, y = 0.5, color = sex),
                   height = 0.1) +
    geom_text(data = modDat,
              aes(x = mean, y = -0.03, label =
                    paste0(round(mean, 1), " cm (n = ", n, ")"),
                color = sex), size = 3) +
    stat_smooth(data = dt, aes(x = length, y = maturity, color = sex),
                method = "glm", formula = y ~ x,
                method.args = list(family = "binomial")) +
    xlab("Total length (cm)") +
    ylab("Maturity") +
    scale_color_manual("Sex", values = c("#FF5F68", "#449BCF")) +
    scale_shape("Sex", solid = FALSE) +
    facet_wrap(~sex, ncol = 1) +
    theme_bw() +
    guides(color=guide_legend(override.aes=list(fill=NA))) +
    theme(legend.position = "none",
          legend.background = element_blank(), legend.key = element_blank())

  } else {
    modDat
  }
}

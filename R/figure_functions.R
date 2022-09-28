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
      mutate(maturity = as.integer(maturity_stage >= 3))
  } else {
    dt <- mfdb_dplyr_sample(mdb) %>%
      filter(!is.na(length), !is.na(sex), !is.na(maturity_stage)) %>%
      filter(!!rlang::parse_expr(filter.exp)) %>%
      select(year, month, areacell, age, sex, maturity_stage, length) %>%
      collect() %>%
      mutate(maturity = as.integer(maturity_stage >= 3))
  }

  modF <- glm(maturity ~ length, data = dt[dt$sex == "F",],
              family = binomial(link = "logit"))
  modM <- glm(maturity ~ length, data = dt[dt$sex == "M",],
              family = binomial(link = "logit"))

  Fdat <- unlogit(0.5, modF)
  Fdat$sex <- "F"
  Fdat$intercept <- coef(modF)[1]
  Fdat$slope <- coef(modF)[2]

  Mdat <- unlogit(0.5, modM)
  Mdat$sex <- "M"
  Mdat$intercept <- coef(modM)[1]
  Mdat$slope <- coef(modM)[2]

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

## Growth curve

#' @title Plot age-length relationships and growth curves
#' @param length Character argument giving the name of the length column in \code{dt}
#' @param age Character argument giving the name of the age column in \code{dt}
#' @param growth.model Integer defining the growth model. 1 = von Bertalanffy, 2 = Gompertz, 3 = Logistic.
#' @param force.zero.group.length Numeric indicating the length to which 0-group should be forced.
#' @param force.zero.group.strengt Numeric indicating how many percent of total fish should be added to the specified \code{force.zero.group.length}.
#' @inheritParams plot.maturity
#' @param boxplot Logical indicating whether boxplots (\code{TRUE}) should be used to show data over points (\code{FALSE})
#' @param base_size Base size parameter for ggplot. See \link[ggplot2]{ggtheme}.
#' @details Uses the \code{fishmethods::growth} function to calculate the growth curves
#' @author Mikko Vihtakari // Institute of Marine Research. Version 2021-10-06
#' @import dplyr ggplot2
#' @importFrom fishmethods growth

# Debug parameters:
# dt = x; length = "Length"; age = "Age"; sex = "Sex"; female.sex = "F"; male.sex = "M"; length.unit = "cm"; filter.exp = NULL; split.by.sex = FALSE; growth.model = 1; force.zero.group.length = NA; force.zero.group.strength = 10
plot.growth <- function(dt, length = "Length", age = "Age", sex = "Sex", female.sex = "F", male.sex = "M", length.unit = "cm", filter.exp = NULL, split.by.sex = FALSE, growth.model = 1, force.zero.group.length = NA, force.zero.group.strength = 10, boxplot = TRUE, base_size = 8) {

  # Growth model

  if(!growth.model %in% 1:3) stop("growth.model has to be an integer between 1 and 3")

  modName <- c("von Bertalanffy" = "vout", "Gompertz" = "gout", "Logistic" = "lout")
  mod.name <- names(modName[growth.model])
  growth.model <- unname(modName[growth.model])

  # Add row number

  dt$id <- rownames(dt)

  # Fix sex column

  if(split.by.sex) {
    if(is.null(sex)) stop("Sex column has to be specified when split.by.sex = TRUE")
    if(dt %>% dplyr::pull(!!rlang::enquo(sex)) %>% na.omit() %>% length() < 10) stop("Either invalid sex column or not enough sex data")

    orig.nrow <- nrow(dt)

    dt <- dt %>%
      dplyr::rename("sex" = tidyselect::all_of(sex)) %>%
      dplyr::filter(!is.na(sex))

    sex.missing <- orig.nrow - nrow(dt)
  }

  # Filter

  if(is.null(filter.exp)) {
    if(!exists("orig.nrow")) orig.nrow <- nrow(dt)

    dt <- dt %>%
      dplyr::rename("age" = tidyselect::all_of(age),
                    "length" = tidyselect::all_of(length)
      )

    length.missing <- sum(is.na(dt$length))
    age.missing <- sum(is.na(dt$age))

    dt <- dt %>% dplyr::filter(!is.na(age) & !is.na(length))

  } else {
    if(!exists("orig.nrow")) orig.nrow <- nrow(dt)

    dt <- dt %>%
      dplyr::filter(!!rlang::parse_expr(filter.exp)) %>%
      dplyr::rename("age" = tidyselect::all_of(age),
                    "length" = tidyselect::all_of(length)
      )

    length.missing <- sum(is.na(dt$length))
    age.missing <- sum(is.na(dt$age))

    dt <- dt %>% dplyr::filter(!is.na(age) & !is.na(length))

  }

  # Select columns and add zero group if requested

  if(split.by.sex){

    dt <- dt %>% dplyr::select(id, sex, age, length)

    if(!is.na(force.zero.group.length)) {
      dt <- dplyr::bind_rows(
        dt,
        tibble::tibble(
          id = NA, sex = female.sex, age = 0,
          length =
            rep(force.zero.group.length,
                ceiling(sum(dt$sex == female.sex)*(force.zero.group.strength/100)))),
        tibble::tibble(
          id = NA, sex = male.sex, age = 0,
          length = rep(force.zero.group.length,
                       ceiling(sum(dt$sex == male.sex)*(force.zero.group.strength/100))))
      )
    }
  } else {

    dt <- dt %>% dplyr::select(id, age, length)

    if(!is.na(force.zero.group.length)) {
      dt <- dplyr::bind_rows(
        dt,
        tibble::tibble(id = NA, age = 0,
                       length =
                         rep(force.zero.group.length,
                             ceiling(nrow(dt)*(force.zero.group.strength/100))))
      )
    }
  }

  # Plot sexed data

  if(split.by.sex) {

    if(any(dt %>% group_by(sex) %>% count() %>% pull(n) < 10)) {

      Plot <- ggplot(dt, aes(x = age, y = length, color = sex)) +
        geom_point(shape = 21, alpha = 0.7) +
        annotation_custom(
          grid::textGrob("Not enough age data for\nsex separated growth models",
                         gp = grid::gpar(fontsize = 8, fontface = "bold")),
          xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
        scale_color_manual("Sex", values = c("#FF5F68", "#449BCF")) +
        ylab(paste0("Total length (", length.unit, ")")) +
        xlab("Age (years)") +
        coord_cartesian(expand = FALSE, clip = "off") +
        theme(legend.position = "bottom",
              text = element_text(size = base_size))

      Text <- paste0(
        "Not enough age data:",
        "\n Number of included specimens = ", sum(dt$sex == female.sex), " females and ", sum(dt$sex == male.sex), " males"
      )

    } else {

      laModF <- fishmethods::growth(
        age = dt %>% dplyr::filter(sex == female.sex) %>% dplyr::pull(age),
        size = dt %>% dplyr::filter(sex == female.sex) %>% dplyr::pull(length),
        Sinf = dt %>% dplyr::filter(sex == female.sex) %>% dplyr::pull(length) %>% max,
        K = 0.1, t0 = 0, graph = FALSE)

      laModM <- fishmethods::growth(
        age = dt %>% dplyr::filter(sex == male.sex) %>% dplyr::pull(age),
        size = dt %>% dplyr::filter(sex == male.sex) %>% dplyr::pull(length),
        Sinf = dt %>% dplyr::filter(sex == male.sex) %>% dplyr::pull(length) %>% max,
        K = 0.1, t0 = 0, graph = FALSE)

      laModFpred <- data.frame(age = 0:max(dt$age), length = predict(eval(parse(text = paste0("laModF$", growth.model))), newdata = data.frame(age = 0:max(dt$age))))
      laModMpred <- data.frame(age = 0:max(dt$age), length = predict(eval(parse(text = paste0("laModM$", growth.model))), newdata = data.frame(age = 0:max(dt$age))))

      tryshit <- try(broom::tidy(eval(parse(text = paste0("laModF$", growth.model))), conf.int = TRUE))

      if(any(class(tryshit) == "try-error")) {
        laModparsF <- dplyr::bind_cols(sex = female.sex, broom::tidy(eval(parse(text = paste0("laModF$", growth.model))), conf.int = FALSE))
      } else {
        laModparsF <- dplyr::bind_cols(sex = female.sex, tryshit)
      }

      tryshit <- try(broom::tidy(eval(parse(text = paste0("laModM$", growth.model))), conf.int = TRUE), silent = TRUE)

      if(any(class(tryshit) == "try-error")) {
        laModparsM <- dplyr::bind_cols(sex = male.sex, broom::tidy(eval(parse(text = paste0("laModM$", growth.model))), conf.int = FALSE))
      } else {
        laModparsM <- dplyr::bind_cols(sex = male.sex, tryshit)
      }

      laModpars <- dplyr::bind_rows(laModparsF, laModparsM)

      ## Plot

      Plot <-
        suppressWarnings({
          ggplot() +
            {if(boxplot) geom_boxplot(data = dt, aes(x = age, y = length, color = sex, group = interaction(age, sex)), alpha = 0.5, outlier.size = 0.5)} +
            {if(!boxplot) geom_point(data = dt, aes(x = age, y = length, color = sex, text = paste0("row number: ", id)), alpha = 0.5, shape = 21)} +
            geom_hline(yintercept = laModparsF$estimate[1], linetype = 2, color = "#FF5F68", alpha = 0.5) +
            geom_hline(yintercept = laModparsM$estimate[1], linetype = 2, color = "#449BCF", alpha = 0.5) +
            geom_path(data = laModFpred, aes(x = age, y = length), color = "#FF5F68", size = LS(3)) +
            geom_path(data = laModMpred, aes(x = age, y = length), color = "#449BCF", size = LS(3)) +
            expand_limits(x = c(0, round_any(max(dt$age), 2, ceiling)), y = c(0, round_any(max(dt$length), 5, ceiling))) +
            scale_x_continuous(breaks = seq(0,100,2)) +
            scale_y_continuous(breaks = seq(0,200,5)) +
            scale_color_manual("Sex", values = c("#FF5F68", "#449BCF")) +
            labs(y = paste0("Total length (", length.unit, ")"),  x = "Age (years)") +
            coord_cartesian(expand = FALSE, clip = "off") +
            theme(legend.position = "bottom",
                  text = element_text(size = base_size))
        })

      Text <- paste0(
        mod.name, " growth function coefficients for females and males, respectively:  \n Linf (asymptotic average length) = ",
        round(laModparsF$estimate[1], 1), " ", length.unit, " +/- ",
        if("conf.low" %in% names(laModparsF)) {paste0(round(laModparsF$conf.low[1], 1), " - ", round(laModparsF$conf.high[1], 1), " (95% CIs) and ")} else {paste0("no CIs and ")},
        round(laModparsM$estimate[1], 1), " ", length.unit, " +/- ",
        if("conf.low" %in% names(laModparsM)) {paste0(round(laModparsM$conf.low[1], 1), " - ", round(laModparsM$conf.high[1], 1), " (95% CIs)")} else {paste0("no CIs")},
        "  \n K (growth rate coefficient) = ",
        round(laModparsF$estimate[2], 4), " ", length.unit, " +/- ",
        if("conf.low" %in% names(laModparsF)) {paste0(round(laModparsF$conf.low[2], 3), " - ", round(laModparsF$conf.high[2], 3), " (95% CIs) and ")} else {paste0("no CIs and ")},
        round(laModparsM$estimate[2], 4), " ", length.unit, " +/- ",
        if("conf.low" %in% names(laModparsM)) {paste0(round(laModparsM$conf.low[2], 3), " - ", round(laModparsM$conf.high[2], 3), " (95% CIs)")} else {paste0("no CIs")},
        "  \n t0 (age at length 0) = ",
        round(laModparsF$estimate[3], 2), " (years) +/- ",
        if("conf.low" %in% names(laModparsF)) {paste0(round(laModparsF$conf.low[3], 1), " - ", round(laModparsF$conf.high[3], 1), " (95% CIs) and ")} else {paste0("no CIs and ")},
        round(laModparsM$estimate[3], 2), " (years) +/- ",
        if("conf.low" %in% names(laModparsM)) {paste0(round(laModparsM$conf.low[3], 3), " - ", round(laModparsM$conf.high[3], 3), " (95% CIs)")} else {paste0("no CIs")},
        "  \n tmax (life span; t0 + 3/K) = ", round(laModparsF$estimate[3] + 3 / laModparsF$estimate[2], 1), " and ", round(laModparsM$estimate[3] + 3 / laModparsM$estimate[2], 1), " years",
        "  \n Number of included specimens = ", nrow(dt[dt$sex == female.sex,]), " and ", nrow(dt[dt$sex == male.sex,]),
        "  \n Total number of measured = ", orig.nrow,
        "  \n Excluded (length or age missing): \n Length = ", length.missing, "; age = ", age.missing, "; sex = ", sex.missing
      )

    }
  } else {
    # Plot non-sex split data

    if(nrow(dt) < 30) {

      #if(eval(parse(text = paste0("laMod$", growthModelSwitch))) == "Fit failed") {

      Plot <- ggplot(dt, aes(x = age, y = length)) +
        geom_point(shape = 21, alpha = 0.7) +
        annotation_custom(
          grid::textGrob("Not enough age data to\ncalculate a growth model",
                         gp = grid::gpar(fontsize = 8, fontface = "bold", col = "red")),
          xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
        ylab(paste0("Total length (", length.unit, ")")) +
        xlab("Age (years)") +
        coord_cartesian(expand = FALSE, clip = "off") +
        theme(text = element_text(size = base_size))

      Text <- paste0(
        "Not enough age data:",
        "  \n Number of included specimens = ", nrow(dt)
      )

    } else {

      laMod <- fishmethods::growth(age = dt$age, size = dt$length, Sinf = max(dt$length), K = 0.1, t0 = 0, graph = FALSE)

      laModpred <- data.frame(age = 0:max(dt$age), length = predict(eval(parse(text = paste0("laMod$", growth.model))), newdata = data.frame(age = 0:max(dt$age))))

      laModpars <- broom::tidy(eval(parse(text = paste0("laMod$", growth.model))), conf.int = TRUE)

      Plot <-
        suppressWarnings({
          ggplot() +
            {if(boxplot) geom_boxplot(data = dt, aes(x = age, y = length, group = age), outlier.size = 0.5, alpha = 0.5)} +
            {if(!boxplot) geom_point(data = dt, aes(x = age, y = length, text = paste0("row number: ", id)), shape = 21, alpha = 0.5)} +
            expand_limits(x = c(0, round_any(max(dt$age), 2, ceiling)), y = c(0, round_any(max(dt$length), 5, ceiling))) + # c(0, max(pretty(c(0, max(dt$length)))))
            geom_hline(yintercept = laModpars$estimate[1], linetype = 2, color = "blue", alpha = 0.5) +
            scale_x_continuous(breaks = seq(0,100,2)) +
            scale_y_continuous(breaks = seq(0,200,5)) +
            geom_path(data = laModpred, aes(x = age, y = length), color = "blue") +
            labs(y = paste0("Total length (", length.unit, ")"),  x = "Age (years)") +
            coord_cartesian(expand = FALSE, clip = "off") +
            theme(text = element_text(size = base_size))
        })

      Text <- paste0(
        mod.name, " growth function coefficients:  \n Linf (asymptotic average length) = ",
        round(laModpars$estimate[1], 1), " ", length.unit, " +/- ", round(laModpars$conf.low[1], 1), " - ", round(laModpars$conf.high[1], 1), " (95% CIs)",
        "  \n K (growth rate coefficient) = ",
        round(laModpars$estimate[2], 4), " +/- ", round(laModpars$conf.low[2], 3), " - ", round(laModpars$conf.high[2], 3), " (95% CIs)",
        "  \n t0 (age at length 0) = ",
        round(laModpars$estimate[3], 2), " (years) +/- ", round(laModpars$conf.low[3], 1), " - ", round(laModpars$conf.high[3], 1), " (95% CIs)",
        "  \n tmax (life span; t0 + 3/K) = ", round(laModpars$estimate[3] + 3 / laModpars$estimate[2], 1), " years",
        "  \n Number of included specimens = ", nrow(dt),
        "  \n Total number of measured = ", orig.nrow,
        "  \n Excluded (length or age missing):  \n Length = ", length.missing, "; age = ", age.missing
      )

    }

  }

  ## Return

  return(list(plot = Plot, text = Text, params = if(exists("laModpars")) {laModpars} else {NULL}))
}


#' @title Plot Maturity ogive (length at 50% mature) for a dataset
#' @param dt A data.frame, tibble or data.table
#' @param length Character argument giving the name of the length (or age) column in \code{dt}
#' @param maturity Character argument giving the name of the maturity column in \code{dt}. Should be either logical (\code{TRUE == mature, FALSE == immature}) or integer (\code{1 == mature, 0 == immature}).
#' @param sex Character argument giving the name of the sex column in \code{dt}. Ignored if \code{split.by.sex == FALSE}.
#' @param length.unit A character argument giving the units of length. Will be used in the x-axis label of the figure.
#' @param length.bin.width Numeric specifying the increment (delta length) by which length data should be binned to calculate maturity proportions. Use \code{NULL} to remove from the plot.
#' @param split.by.sex Logical indicating whether the result should be split by sex.
#' @param filter.exp Expression to filter data. E.g. 'sampling_type == "ENS"'
#' @param xlab Character giving the x-axis label without unit
#' @param plot Logical indicated whether plot should be generated. If \code{FALSE}, L50 values will be returned instead.
#' @param base_size Base size parameter for ggplot. See \link[ggplot2]{ggtheme}.
#' @return Returns a ggplot2 or tibble depending on the \code{plot} argument showing the maturity ogives.
#' @details Depends on the tidyverse and ggridges packages. The dplyr and ggplot2 packages must be loaded into the workspace.
#' @author Mikko Vihtakari // Institute of Marine Research. Version 2021-10-06
#' @import dplyr ggplot2
#' @importFrom ggridges geom_density_ridges

# Debug parameters
# dt = x; length = "Age"; maturity = "Mature"; sex = "Sex"; female.sex = "F"; male.sex = "M"; length.unit = "cm"; length.bin.width = 5 ;split.by.sex = TRUE; filter.exp = NULL; plot = TRUE
plot.maturity2 <- function(dt, length = "Length", maturity = "Mature", sex = "Sex", female.sex = "F", male.sex = "M", length.unit = "cm", length.bin.width = 5, split.by.sex = FALSE, filter.exp = NULL, xlab = "Total length", plot = TRUE, base_size = 8) {

  # Checks

  if(split.by.sex) {
    if(is.null(sex)) stop("Sex column has to be specified when split.by.sex = TRUE")
    if(dt %>% pull(!!enquo(sex)) %>% na.omit() %>% length() < 10) stop("Either invalid sex column or not enough sex data")
  }

  if(is.null(filter.exp)) {
    dt <- dt %>%
      dplyr::rename("maturity" = tidyselect::all_of(maturity),
                    "length" = tidyselect::all_of(length)) %>%
      dplyr::filter(!is.na(maturity) &
                      !is.na(length)) %>%
      dplyr::mutate(maturity = as.integer(maturity))
  } else {
    dt <- dt %>%
      dplyr::filter(!!rlang::parse_expr(filter.exp)) %>%
      dplyr::rename("maturity" = tidyselect::all_of(maturity),
                    "length" = tidyselect::all_of(length)) %>%
      dplyr::filter(!is.na(maturity) &
                      !is.na(length)) %>%
      dplyr::mutate(maturity = as.integer(maturity))
  }

  if(split.by.sex) {

    dt <- dt %>%
      dplyr::rename("sex" = tidyselect::all_of(sex)) %>%
      dplyr::filter(!is.na(sex))

    if(!is.null(length.bin.width)) {
      mat.pr.dt <- dt %>%
        mutate(bin = ggplot2::cut_interval(x = length, length = length.bin.width)) %>%
        group_by(sex, bin) %>%
        summarise(mat.pr = sum(maturity == 1)/(length(maturity)), .groups = "keep") %>%
        mutate(bin1 = as.numeric(gsub("\\D", "",
                                      select.element(strsplit(as.character(bin), "\\,"), 1))),
               bin2 = as.numeric(gsub("\\D", "",
                                      select.element(strsplit(as.character(bin), "\\,"), 2)))
        )
    }

    tmp <- dt %>% group_by(sex, maturity) %>% summarise(mean = mean(length), .groups = "keep")

    if(tmp[tmp$sex == female.sex & tmp$maturity == 0, "mean"] > tmp[tmp$sex == female.sex & tmp$maturity == 1, "mean"]) {
      warning("Mean size of female immature fish larger than mature fish. Unable to calculate L50 reliably")
      Fdat <- tibble(mean = NA, ci.min = NA, ci.max = NA, sex = female.sex) %>% mutate(across(c("mean", "ci.min", "ci.max"), as.numeric))
    } else {
      modF <- glm(maturity ~ length, data = dt %>% dplyr::filter(sex == female.sex),
                  family = binomial(link = "logit"))

      if(broom::tidy(modF)$p.value[2] > 0.05) {
        warning("The length term in the female L50 logistic model is non-siginificant. This indicates problems with the underlying data.")
      }

      Fdat <- unlogit(0.5, modF)
      Fdat$sex <- female.sex
      Fdat$intercept <- coef(modF)[1]
      Fdat$slope <- coef(modF)[2]
    }

    if(tmp[tmp$sex == male.sex & tmp$maturity == 0, "mean"] > tmp[tmp$sex == male.sex & tmp$maturity == 1, "mean"]) {
      warning("Mean size of male immature fish larger than mature fish. Unable to calculate L50 reliably")
      Mdat <- tibble(mean = NA, ci.min = NA, ci.max = NA, sex = male.sex) %>% mutate(across(c("mean", "ci.min", "ci.max"), as.numeric))
    } else {
      modM <- glm(maturity ~ length, data = dt %>% dplyr::filter(sex == male.sex),
                  family = binomial(link = "logit"))

      Mdat <- unlogit(0.5, modM)
      Mdat$sex <- male.sex
      Mdat$intercept <- coef(modM)[1]
      Mdat$slope <- coef(modM)[2]
    }

    modDat <- dplyr::bind_rows(Fdat, Mdat)
    modDat <- dplyr::left_join(modDat, dt %>% dplyr::group_by(sex) %>% count, by = "sex")

  } else {

    tmp <- dt %>% group_by(maturity) %>% summarise(mean = mean(length))

    if(!is.null(length.bin.width)) {
      mat.pr.dt <- dt %>%
        mutate(bin = ggplot2::cut_interval(x = length, length = length.bin.width)) %>%
        group_by(bin) %>%
        summarise(mat.pr = sum(maturity == 1)/(length(maturity)), .groups = "keep") %>%
        mutate(bin1 = as.numeric(gsub("\\D", "", select.element(strsplit(as.character(bin), "\\,"), 1))),
               bin2 = as.numeric(gsub("\\D", "", select.element(strsplit(as.character(bin), "\\,"), 2))))
    }

    if(tmp[tmp$maturity == 0, "mean"] > tmp[tmp$maturity == 1, "mean"]) {
      warning("Mean size of immature fish larger than mature fish. Unable to calculate L50 reliably")
      modDat <- tibble(mean = NA, ci.min = NA, ci.max = NA, n = nrow(dt)) %>% mutate_all(., as.numeric)
    } else {
      mod <- glm(maturity ~ length, data = dt,
                 family = binomial(link = "logit"))
      modDat <- unlogit(0.5, mod)
      modDat <- dplyr::bind_cols(modDat, n = nrow(dt))
    }
  }

  if(plot) {

    if(split.by.sex) {
      p <-
        ggplot() +
        #facet_wrap(~sex, ncol = 1) +
        {if(!is.null(length.bin.width)) geom_step(data = mat.pr.dt, aes(x = bin1, y = mat.pr, color = sex), alpha = 0.5)} +
        ggridges::geom_density_ridges(data = dt,
                                      aes(x = length, y = maturity, group = paste(sex, maturity), fill = sex),
                                      scale = 0.3, size = LS(0.5), alpha = 0.5) +
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
                        paste0(round(mean, 1), " ", length.unit, " (n = ", n, ")"),
                      color = sex), size = FS(base_size)) +
        stat_smooth(data = dt, aes(x = length, y = maturity, color = sex),
                    method = "glm", formula = y ~ x,
                    method.args = list(family = "binomial"), size = LS(1)) +
        xlab(paste0(xlab, " (", length.unit, ")")) +
        ylab("Maturity") +
        coord_cartesian(xlim = c(0, ceiling(max(dt$length)))) +
        scale_color_manual("Sex", values = c("#FF5F68", "#449BCF")) +
        scale_fill_manual("Sex", values = c("#FF5F68", "#449BCF")) +
        # theme_bw(base_size = 8) +
        guides(color=guide_legend(override.aes=list(fill=NA))) +
        theme(legend.position = "none",
              text = element_text(size = base_size),
              legend.background = element_blank(),
              legend.key = element_blank())

      suppressWarnings({print(p)})

    } else {
      p <- ggplot() +
        {if(!is.null(length.bin.width)) geom_step(data = mat.pr.dt, aes(x = bin1, y = mat.pr), alpha = 0.5)} +
        ggridges::geom_density_ridges(data = dt,
                                      aes(x = length, y = maturity, group = maturity),
                                      scale = 0.3, size = LS(0.5), alpha = 0.5) +
        geom_segment(data = modDat,
                     aes(x = mean, xend = mean, y = 0, yend = 0.5), linetype = 2) +
        geom_segment(data = modDat,
                     aes(x = -Inf, xend = mean, y = 0.5, yend = 0.5), linetype = 2) +
        geom_errorbarh(data = modDat,
                       aes(xmin = ci.min, xmax = ci.max, y = 0.5), height = 0.1) +
        geom_text(data = modDat,
                  aes(x = mean, y = -0.03, label =
                        paste0(round(mean, 1), " ", length.unit, " (n = ", n, ")")),
                  size = FS(base_size)) +
        stat_smooth(data = dt, aes(x = length, y = maturity),
                    method = "glm", formula = y ~ x,
                    method.args = list(family = "binomial"), size = LS(1)) +
        xlab(paste0(xlab, " (", length.unit, ")")) +
        ylab("Maturity") +
        coord_cartesian(xlim = c(0, ceiling(max(dt$length)))) +
        # theme_bw(base_size = 8) +
        guides(color=guide_legend(override.aes=list(fill=NA))) +
        theme(legend.position = "none",
              text = element_text(size = base_size),
              legend.background = element_blank(),
              legend.key = element_blank())

      suppressWarnings({print(p)})
    }
  } else {
    modDat
  }
}

#ldist = EggaN_ldist; mat = EggaN_mat
# compare_mat_ldist <- function(ldist, mat) {
#
#
#
# }

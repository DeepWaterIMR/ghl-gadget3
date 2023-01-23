## ---------------------------
##
## Script name: Functions to clean up maturity and sex ratio data
## Author: Mikko Vihtakari // Institute of Marine Research, Norway
## Email: mikko.vihtakari@hi.no
##
## Date Created: 2023-01-23
##
## ---------------------------

## Function to clean up maturity data
clean_mat_data <- function(x, return_gadget_compatible = TRUE, plot = FALSE) {

  length_groups <- sapply(attributes(x)$length, function(k) attr(k, "min"))

  nasse <- x %>%
    mutate(len = as.numeric(gsub("len", "", length))) %>%
    filter(
      (grepl("^female_mat", maturity_stage) &
         len > stock_params$female_mat$min_possible_data_length) |
        (grepl("^female_imm", maturity_stage) &
           len < stock_params$female_imm$max_possible_data_length) |
        (grepl("^male_mat", maturity_stage) &
           len > stock_params$male_mat$min_possible_data_length) |
        (grepl("^male_imm", maturity_stage) &
           len < stock_params$male_imm$max_possible_data_length)
    ) %>%
    mutate(len = as.numeric(gsub("len", "", length))) %>%
    filter(len > stock_params$force_even_stock_distribution_length,
           len <= stock_params$male_mat$max_possible_data_length,
           len >= min(length_groups))

  # remove <- x %>%
  #   group_by(year, step, area) %>%
  #   complete(maturity_stage, length, fill = list(number = 0)) %>%
  #   group_by(year, step, length) %>%
  #   summarise(n = sum(number), ratio = number[sex == "female"]/n) %>%
  #   mutate(Length = as.integer(gsub("len", "", length))) %>%
  #   filter((Length <= 50 & ratio > 0.65) | (n <= 5 & Length > 70 & ratio < 0.9)) %>%
  #   ungroup()

  # length_groups <- length_groups[length_groups>=nasse %>% filter(number>0) %>% pull(len) %>% min()]


  ## Mature females

  tmp <- tibble(year = unique(x$year),
                step = unique(x$step),
                area = unique(x$area),
                maturity_stage = "female_mat")

  if(any(length_groups < stock_params$female_mat$min_possible_data_length)) {
    tmp <- tmp %>%
      tidyr::expand(
        nesting(year, step, area, maturity_stage),
        length =
          names(length_groups)[length_groups < stock_params$female_mat$min_possible_data_length]
      ) %>% mutate(number = 0)
  }

  if(any(length_groups > stock_params$male_mat$max_possible_data_length)) {
    tmp <- tmp %>%
      tidyr::expand(
        nesting(year, step, area, maturity_stage),
        length =
          names(length_groups)[length_groups > stock_params$male_mat$max_possible_data_length]
      ) %>% mutate(number = 1)
  }

  if(any(length_groups > stock_params$female_imm$max_possible_data_length)) {
    tmp <- tmp %>%
      tidyr::expand(
        nesting(year, step, area, maturity_stage),
        length =
          names(length_groups)[length_groups > stock_params$female_imm$max_possible_data_length]
      ) %>% mutate(number = 1)
  }

  nasse <- bind_rows(nasse, tmp %>% mutate(len = as.numeric(gsub("len", "", length))))

  ## Immature females

  tmp <- tibble(year = unique(x$year),
                step = unique(x$step),
                area = unique(x$area),
                maturity_stage = "female_imm")

  if(any(length_groups > stock_params$female_imm$max_possible_data_length)) {
    tmp <- tmp %>%
      tidyr::expand(
        nesting(year, step, area, maturity_stage),
        length =
          names(length_groups)[length_groups > stock_params$female_imm$max_possible_data_length]
      ) %>% mutate(number = 0)
  }

  nasse <- bind_rows(nasse, tmp %>% mutate(len = as.numeric(gsub("len", "", length))))

  ## Mature males

  tmp <- tibble(year = unique(x$year),
                step = unique(x$step),
                area = unique(x$area),
                maturity_stage = "male_mat")

  if(any(length_groups < stock_params$male_mat$min_possible_data_length)) {
    tmp <- tmp %>%
      tidyr::expand(
        nesting(year, step, area, maturity_stage),
        length =
          names(length_groups)[length_groups < stock_params$female_mat$min_possible_data_length]
      ) %>% mutate(number = 0)
  }

  if(any(length_groups > stock_params$male_mat$max_possible_data_length)) {
    tmp <- tmp %>%
      tidyr::expand(
        nesting(year, step, area, maturity_stage),
        length =
          names(length_groups)[length_groups > stock_params$male_mat$max_possible_data_length]
      ) %>% mutate(number = 0)
  }

  nasse <- bind_rows(nasse, tmp %>% mutate(len = as.numeric(gsub("len", "", length))))

## Immature males

  tmp <- tibble(year = unique(x$year),
                step = unique(x$step),
                area = unique(x$area),
                maturity_stage = "male_imm")

  if(any(length_groups > stock_params$male_imm$max_possible_data_length)) {
    tmp <- tmp %>%
      tidyr::expand(
        nesting(year, step, area, maturity_stage),
        length =
          names(length_groups)[length_groups > stock_params$male_imm$max_possible_data_length]
      ) %>% mutate(number = 0)
  }

  nasse <- bind_rows(nasse, tmp %>% mutate(len = as.numeric(gsub("len", "", length))))

  ## Force even stock distribution
  nasse <- nasse %>%
    bind_rows(
      tibble(year = unique(x$year),
             step = unique(x$step),
             area = unique(x$area)) %>%
        tidyr::expand(nesting(year, step, area),
                      maturity_stage = c("female_imm", "male_imm")) %>%
        tidyr::expand(
          nesting(year, step, area, maturity_stage),
          length = names(length_groups)[length_groups <=
                                          stock_params$force_even_stock_distribution_length]
        ) %>%
        mutate(number = 1,
               len = as.numeric(gsub("len", "", length)))
    ) %>%
    group_by(year, step, area) %>%
    complete(maturity_stage, nesting(length, len), fill = list(number = 0)) %>%
    group_by(year, step, area, maturity_stage, length, len) %>%
    summarise(number = sum(number), .groups = "drop") %>%
    arrange(year, step, area, maturity_stage, len) %>%
    group_by(year, step, area, length, len) %>%
    mutate(n = sum(number), ratio = number/n) %>%
    ungroup() %>%
    replace_na(list(ratio = 0))

  nasse <- lapply(nasse %>% split(list(.$year, .$step, .$area, .$maturity_stage)), function(k) {
    mod <- stats::loess(ratio~len, span= 0.8, data = k, na.action = "na.omit",
                        control=loess.control(surface="direct"))
    tmp <- data.frame(
      year = unique(k$year),
      step = unique(k$step),
      area = unique(k$area),
      maturity_stage = unique(k$maturity_stage)) %>%
      full_join(
        data.frame(year = unique(k$year),
                   len = length_groups),
        by = "year") %>%
      arrange(year, step, area, maturity_stage, len) %>%
      mutate(length = paste0("len", len), .before = "len")

    tmp2 <- stats::predict(mod, tmp)
    tmp2[tmp2 < 0] <- 0
    tmp2[tmp2 > 1] <- 1

    if(unique(k$maturity_stage) == "male_mat") {
      tmp2[tmp$len > stock_params$male_mat$max_possible_data_length] <- 0
    }

    tmp$pred.ratio <- tmp2

    full_join(
      k, tmp,
      by = c("year", "step", "area", "maturity_stage", "length", "len")) %>%
      arrange(year, step, area, maturity_stage, len)

  }) %>%
    bind_rows() %>%
    replace_na(list(n = 1)) %>%
    mutate(pred.number = round(pred.ratio * n, 0))

  if(plot) {
    print(
      ggplot(nasse) +
        geom_point(aes(x = len, y = ratio, color = maturity_stage)) +
        geom_path(aes(x = len, y = pred.ratio, color = maturity_stage)) +
        labs(x = "Length (cm)", y = "Stock ratio (points = data, lines = smoothed)") +
        facet_wrap(~year)
    )
  }

  if(!return_gadget_compatible) {
    return(nasse)
  } else {
    nasse <- nasse %>%
      dplyr::select(-len, -number, -n, -ratio, -pred.ratio) %>%
      rename("number" = "pred.number") %>%
      as.data.frame()

    attributes(nasse) <- c(attributes(nasse)[names(attributes(nasse)) %in% c("class", "row.names", "names")],
                           attributes(x)[!names(attributes(x)) %in% c("class", "row.names", "names")])

    attributes(nasse)$length <- attributes(nasse)$length[sapply(attributes(nasse)$length, function(k) attributes(k)$min %in% length_groups)]

    return(nasse)
  }
}

## Function to clean up sex ratio data. Many assumptions...
clean_sexratio_data <- function(x, return_gadget_compatible = TRUE, plot = FALSE) {

  length_groups <- sapply(attributes(x)$length, function(k) attr(k, "min"))

  remove <- x %>%
    group_by(year, step, area) %>%
    complete(sex, length, fill = list(number = 0)) %>%
    group_by(year, step, length) %>%
    summarise(n = sum(number), ratio = number[sex == "female"]/n) %>%
    mutate(Length = as.integer(gsub("len", "", length))) %>%
    filter((Length <= 50 & ratio > 0.65) | (n <= 5 & Length > 70 & ratio < 0.9)) %>%
    ungroup()

  nasse <- anti_join(x, remove, by = c("year", "step", "length")) %>%
    mutate(len = as.numeric(gsub("len", "", length)))

  length_groups <- length_groups[length_groups>=nasse %>% filter(number>0) %>% pull(len) %>% min()]

  nasse <- nasse %>%
    filter(len <= stock_params$male_mat$max_possible_data_length,
           len >= min(length_groups)) %>%
    bind_rows(
      tibble(year = unique(x$year),
             step = unique(x$step),
             area = unique(x$area)) %>%
        tidyr::expand(nesting(year, step, area),
                      sex = c("female", "male")) %>%
        tidyr::expand(
          nesting(year, step, area, sex),
          length = names(attributes(x)$length)
          [as.numeric(gsub("[^0-9.-]", "",
                           names(attributes(x)$length))) >
              stock_params$male_mat$max_possible_data_length]
        ) %>%
        mutate(number = ifelse(sex == "female", 10, 0),
               len = as.numeric(gsub("len", "", length)))
    ) %>%
    group_by(year, step, area) %>%
    complete(sex, nesting(length, len), fill = list(number = 0)) %>%
    arrange(year, step, area, sex, len) %>%
    group_by(year, step, area, length) %>%
    mutate(n = sum(number), ratio = number[sex == "female"]/n) %>%
    ungroup()

  nasse <- lapply(nasse %>% split(list(.$year, .$step, .$area)), function(k) {
    mod <- stats::loess(ratio~len, span= 0.75, data = k, na.action = "na.omit",
                        control=loess.control(surface="direct"))
    tmp <- data.frame(
      year = unique(k$year),
      step = unique(k$step),
      area = unique(k$area),
      sex = c("female", "male")) %>%
      full_join(
        data.frame(year = unique(k$year),
                   len = length_groups),
        by = "year") %>%
      arrange(year, step, area, sex, len) %>%
      mutate(length = paste0("len", len), .before = "len")

    tmp2 <- stats::predict(mod, tmp %>% filter(sex == "female"))
    tmp2[tmp2 < 0] <- 0
    tmp2[tmp2 > 1] <- 1

    tmp$pred.ratio <- c(tmp2, 1-tmp2)

    full_join(
      k, tmp,
      by = c("year", "step", "area", "sex", "length", "len")) %>%
      arrange(year, step, area, sex, len) %>%
      mutate(ratio = ifelse(sex == "male", 1 - ratio, ratio))
  }) %>%
    bind_rows() %>%
    replace_na(list(n = 10)) %>%
    mutate(pred.number = round(pred.ratio * n, 0))

  if(plot) {
    print(
      ggplot(nasse) +
        geom_point(aes(x = len, y = ratio, color = sex)) +
        geom_path(aes(x = len, y = pred.ratio, color = sex)) +
        labs(x = "Length (cm)", y = "Sex ratio (points = data, lines = smoothed)") +
        facet_wrap(~year)
    )
  }

  if(!return_gadget_compatible) {
    return(nasse)
  } else {
    nasse <- nasse %>%
      dplyr::select(-len, -number, -n, -ratio, -pred.ratio) %>%
      rename("number" = "pred.number") %>%
      as.data.frame()

    attributes(nasse) <- c(attributes(nasse)[names(attributes(nasse)) %in% c("class", "row.names", "names")],
                           attributes(x)[!names(attributes(x)) %in% c("class", "row.names", "names")])

    attributes(nasse)$length <- attributes(nasse)$length[sapply(attributes(nasse)$length, function(k) attributes(k)$min %in% length_groups)]

    return(nasse)
  }
}

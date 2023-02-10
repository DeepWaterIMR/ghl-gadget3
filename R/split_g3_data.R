#' @title Split gadget3 length distributions using external information
#' @description Splits gadget3 length distribution data using another g3 length aggregated dataset such as sex or stock distributions
#' @param x g3data data frame to split
#' @param y data frame guiding the splitting in g3data format. Must have same length bins than \code{x}
#' @param split_column Character specifying the column containing the split information in \code{y}
#' @param method Character specifying the split method. \code{"mean"} takes average ratio for each length bin in \code{y} and uses that for the splitting. Anything else uses data for time-steps when provided and average ratios for missing years.
#' @param separate Logical indicating whether the split data should be returned as separate g3data frames in a list or whether the \code{split_column} should be retained.
#' @param plot Logical indicating whether a control plot of the splitting should be produced
#' @return Returns g3data frames in a list if \code{separate = TRUE}, otherwise a single g3data frame. The attributes are fetched from \code{x} and \code{y}
#' @author Mikko Vihtakari (Institute of Marine Research)
#' @export

# x = TrawlNor_ldist # %>% filter(year %in% c(2019, 2020, 2021))
# y = TrawlNor_sexratio #%>% filter(year %in% c(2020, 2021))
# split_column = "sex"; method = "fill_mean"; separate = TRUE; plot = FALSE
split_g3_data <- function(x, y, split_column, method = "fill_mean", separate = TRUE, plot = FALSE) {

  x_attr <- attributes(x)
  length_attr <- x_attr$length
  y_attr <- attributes(y)

  # length_groups <- names(attributes(y)$length) %>%
  #   gsub("[^0-9.-]", "", .) %>%
  #   as.numeric()
  #
  # first_length_group <- attributes(y)$length[1]
  # last_length_group <- attributes(y)$length[length(length_groups)]

  x <- x %>%
    dplyr::mutate(length = factor(length, names(length_attr))) %>%
    tidyr::complete(year, step, area, length, fill = list(number = 0))

  y <- y %>%
    dplyr::mutate(length = factor(length, names(length_attr))) %>%
    tidyr::complete(year, step, area, !!as.symbol(split_column), length, fill = list(number = 0))

  y_mean <- y %>%
    dplyr::group_by(!!as.symbol(split_column), length) %>%
    dplyr::summarise(number = sum(number)) %>%
    dplyr::group_by(length) %>%
    dplyr::mutate(total = sum(number),
                  ratio = number/total) %>%
    dplyr::select(-total, -number) %>%
    dplyr::group_by(!!as.symbol(split_column)) %>%
    tidyr::fill(ratio, .direction = "updown") %>%
    dplyr::ungroup()

  if(method == "mean") {
    out <- full_join(x, y_mean, by = c('length'), multiple = "all")
    tmp_y <- y_mean
  } else {

    y_out <- y %>%
      dplyr::group_by(year, step, area, length) %>%
      dplyr::mutate(total = sum(number),
                    ratio = number/total) %>%
      dplyr::select(-total, -number) %>%
      dplyr::group_by(year, step, area, !!as.symbol(split_column)) %>%
      tidyr::fill(ratio, .direction = "updown") %>%
      dplyr::ungroup() %>%
      dplyr::mutate(type = "data") %>%
      dplyr::filter(year %in% unique(x$year))

    if(length(setdiff(unique(x$year), unique(y_out$year))) > 0) {

      y_out <- y_mean %>%
        tidyr::expand(
          year = setdiff(unique(x$year), unique(y_out$year)),
          step = "1",
          area = "all",
          tidyr::nesting(sex, length, ratio),
          type = "mean"
        ) %>% bind_rows(y_out) %>%
        dplyr::arrange(year, step, area, length)
    }

    tmp_y <- y_out

    out <- full_join(x, y_out %>% dplyr::select(-type),
                     by = c('year', 'step', 'area', 'length'),
                     multiple = "all")


  }

  out <- out %>%
    dplyr::mutate(number = round(number * ratio, 0)) %>%
    # dplyr::arrange(year, step, length) %>%
    dplyr::mutate(length = as.character(length)) %>%
    dplyr::relocate(number, .after = dplyr::last_col()) %>%
    dplyr::select(-ratio)

  if(plot) {

    tmp_y <- tmp_y %>%
      left_join(
        lapply(seq_along(length_attr), function(i) {
          data.frame(length = names(length_attr)[i],
                     lower = attr(length_attr[[i]], "min"),
                     upper = attr(length_attr[[i]], "max"))
        }) %>% dplyr::bind_rows(),
        by = "length"
      )

    tmp_y <- tmp_y %>% dplyr::select(-.data$upper) %>%
      dplyr::bind_rows(
        tmp_y %>%
          dplyr::filter(.data$upper == max(.data$upper)) %>%
          dplyr::select(-.data$lower) %>%
          dplyr::rename("lower"= "upper")
      )

    p1 <- ggplot2::ggplot(
      tmp_y,
      ggplot2::aes(x = .data$lower, y = ratio, lty = sex, color = type)) +
      geom_step() + {
        if(method != "mean") {
          ggplot2::facet_wrap(
            ~.data$year+.data$step,
            labeller = ggplot2::label_wrap_gen(multi_line=FALSE))
        }} +
      ggplot2::expand_limits(y = c(0,1)) +
      ggplot2::scale_y_continuous(breaks = seq(0,1,0.25)) +
      ggplot2::labs(y = 'Proportion', x = 'Length') +
      ggplot2::scale_color_manual(values = c("data" = "black", "mean" = "red")) +
      # ggplot2::scale_color_manual(values = cols) +
      ggplot2::theme_classic(base_size = 8) +
      ggplot2::theme(legend.position = "none",
                     strip.background = ggplot2::element_blank())

    tmp_x <- x %>%
      left_join(
        lapply(seq_along(length_attr), function(i) {
          data.frame(length = names(length_attr)[i],
                     lower = attr(length_attr[[i]], "min"),
                     upper = attr(length_attr[[i]], "max"))
        }) %>% dplyr::bind_rows(),
        by = "length"
      )

    tmp_x <- tmp_x %>% dplyr::select(-.data$upper) %>%
      dplyr::bind_rows(
        tmp_x %>%
          dplyr::filter(.data$upper == max(.data$upper)) %>%
          dplyr::select(-.data$lower) %>%
          dplyr::rename("lower"= "upper")
      )

    p2 <- ggplot2::ggplot(
      tmp_x, ggplot2::aes(x = .data$lower, y = .data$number))+
      ggplot2::geom_step() +
      ggplot2::facet_wrap(~.data$year+.data$step,
                          labeller = ggplot2::label_wrap_gen(multi_line=FALSE)) +
      ggplot2::labs(y = 'Number', x = 'Length') +
      ggplot2::theme_classic(base_size = 8) +
      ggplot2::coord_cartesian(expand = FALSE) +
      ggplot2::theme(strip.background = ggplot2::element_blank())

    tmp_out <- out %>%
      left_join(
        lapply(seq_along(length_attr), function(i) {
          data.frame(length = names(length_attr)[i],
                     lower = attr(length_attr[[i]], "min"),
                     upper = attr(length_attr[[i]], "max"))
        }) %>% dplyr::bind_rows(),
        by = "length"
      )

    tmp_out <- tmp_out %>% dplyr::select(-.data$upper) %>%
      dplyr::bind_rows(
        tmp_out %>%
          dplyr::filter(.data$upper == max(.data$upper)) %>%
          dplyr::select(-.data$lower) %>%
          dplyr::rename("lower"= "upper")
      )

    p3 <- ggplot2::ggplot(
      tmp_out, ggplot2::aes(x = .data$lower, y = .data$number, lty = .data$sex))+
      ggplot2::geom_step() +
      ggplot2::facet_wrap(~.data$year+.data$step,
                          labeller = ggplot2::label_wrap_gen(multi_line=FALSE),
                          scales = "free_y") +
      ggplot2::labs(y = 'Number', x = 'Length') +
      ggplot2::theme_classic(base_size = 8) +
      ggplot2::coord_cartesian(expand = FALSE) +
      ggplot2::theme(strip.background = ggplot2::element_blank())

    ## Plot it

    print(
      cowplot::plot_grid(
        cowplot::plot_grid(p1, p2, labels = c("y", "x")),
        p3, rel_heights = c(1,1), ncol = 1, labels = c("", "out"))
    )
  }

  if(separate) {
    lapply(split(out, out[[split_column]]), function(k) {
      tmp <- as.data.frame(k[!names(k) %in% split_column])
      attributes(tmp) <- c(attributes(tmp), x_attr[setdiff(names(x_attr), names(attributes(tmp)))])
      tmp
    })
  } else {
    tmp <- as.data.frame(out)
    attributes(tmp) <- c(attributes(tmp), x_attr[setdiff(names(x_attr), names(attributes(tmp)))])
    attributes(tmp) <- c(attributes(tmp), y_attr[setdiff(names(y_attr), names(attributes(tmp)))])
    tmp
  }
}

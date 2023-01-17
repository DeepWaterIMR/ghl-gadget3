## Settisäätöä

#' @param x A data frame, tibble or data table
#' @param params A list of parameters defining the grouping. See details in \code{mfdb_sample_count}.
#' @param column_names A named vector explaining the column names in \code{x} if not the same than in mfdb.
#'

x <- y %>%
  filter(sampling_type == "ENS",
         gear == "BottomTrawls",
         sex == "F",
         !is.na(age),
         year >= 1996)

params = list(
  age = mfdb_interval(
    "age", stock_params$minage:stock_params$maxage,
    open_ended = c("upper")
  ),
  length = mfdb_interval(
    "len",
    seq(stock_params$minlength, stock_params$maxlength,
        by = 5),
    open_ended = c("upper","lower")
  )
)

verbose = TRUE

column_names = c("year" = "year", "step" = "month", "area" = "areacell")

data_count <- function(x, params = list(), column_names = c("year" = "year", "step" = "month", "area" = "areacell"), verbose = TRUE)

  ## Assign x to new object for easier debugging

  dt <- x

## Add step, area and age

if(!"step" %in% names(column_names) & !"step" %in% colnames(dt)) {
  if(verbose) message("The (time)step column not found. Assuming step = 1")
  dt$step <- 1
}

if(!"step" %in% names(params)) {
  if(verbose) message("(Time)step not specified. Assuming yearly aggregation. Change by defining step = mfdb_timestep_*")
  params$step <- mfdb_timestep_yearly
}

if(!"area" %in% names(params)) {
  if(verbose) message("area not defined. Assuming no areal aggregation.")
  dt$area <- "all"
  if("area" %in% names(column_names)) {
    tmp <- as.character(sort(unique(x[[unname(column_names[names(column_names) == "area"])]])))
  } else{
    tmp <- "all"
  }
  params$area$all <- tmp
}

if(!"age" %in% names(params)) {
  if(verbose) message("age not defined. Assuming no age aggregation.")
  dt$age <- "all"
}

## Rename column names

names(dt)[match(column_names, names(dt))] <- names(column_names)

## Tests

if (!is.list(params)) {
  stop("params argument must be a list")
}

# if(any(!cols %in% names(params))) {
#   stop(paste(setdiff(cols, names(params)), collapse = ", "), " are not specified in params. Tell how they should be aggregated")
# }

if(any(!c("year", names(params)) %in% colnames(dt)))   stop(paste(setdiff(c("year", params), colnames(dt)), collapse = ", "), " is not a column in x. Specify the expected column names using column_names")

## Select relevant columns for easier debugging

cols <- names(params)[!names(params) %in% c("year", "step", "area", "age")]
dt <- dt[c("year", "step", "area", "age", cols)]

## Grouping

dt[names(params)] <-
  lapply(names(params), function(k) {
    tmp <- params[[k]]

    if(inherits(tmp, "mfdb_group")) {
      tmp <- sapply(tmp, max)
      cut(dt[[k]], c(-Inf, tmp), labels = names(tmp), right = FALSE)
    } else if(inherits(tmp, "mfdb_interval")) {
      cut_seq <- unlist(unname(c(tmp)))
      labs <- names(tmp)
      if("lower" %in% attributes(tmp)$open_ended) {
        cut_seq[1] <- -Inf
      }
      if("upper" %in% attributes(tmp)$open_ended) {
        cut_seq[length(cut_seq)] <- Inf
        labs <- labs[-length(tmp)]
      }
      cut(dt[[k]], cut_seq, labels = labs, right = FALSE)
    } else if(inherits(tmp, "list")) { # clumsy area aggregation case. Used for attributes later
      dt[[k]] <- "all"
    } else {
      stop(class(tmp)[1], " not implemented yet.")
    }
  })

## Aggregation

out <- dt %>%
  dplyr::group_by(year, step, area, age, dplyr::across(tidyselect::all_of(cols))) %>%
  dplyr::summarise(number = dplyr::n()) %>%
  # dplyr::arrange(year, step, area, age, dplyr::across(tidyselect::all_of(cols))) %>%
  as.data.frame()

out <- rapply(out, as.character, classes = "factor", how = "replace")

## Add stupid attributes

attributes(out)$year <-
  setNames(
    lapply(unique(out$year), function(k) k),
    lapply(unique(out$year), function(k) k)
  )

# Step (picks also other group parameters here. A possible problem)
if(any(sapply(params, function(k) inherits(k, "mfdb_group")))) {
  tmp_params <- sapply(params, function(k) inherits(k, "mfdb_group"))
  tmp_params <- names(tmp_params[tmp_params])
  attributes(out) <-
    c(attributes(out),
      setNames(
        lapply(tmp_params, function(k) {
          params[[k]]
        }),
        tmp_params
      )
    )
}

if(any(sapply(params, function(k) inherits(k, "list")))) {
  tmp_params <- sapply(params, function(k) inherits(k, "list"))
  tmp_params <- names(tmp_params[tmp_params])
  attributes(out) <-
    c(attributes(out),
      setNames(
        lapply(tmp_params, function(k) {
          params[[k]]
        }),
        tmp_params
      )
    )
}

if(any(sapply(params, function(k) inherits(k, "mfdb_interval")))) {
  tmp_params <- sapply(params, function(k) inherits(k, "mfdb_interval"))
  tmp_params <- names(tmp_params[tmp_params])
  attributes(out) <-
    c(attributes(out),
      setNames(
        lapply(tmp_params, function(k) {
          tmp <- params[[k]]

          setNames(lapply(seq_along(tmp)[-length(tmp)], function(i) {

            min_val <- unname(tmp[i])
            max_val <- unname(tmp[i+1])
            outout <- call("seq", min_val, max_val -1)

            attr(outout, "min") <- min_val
            attr(outout, "max") <- max_val

            if("lower" %in% attributes(tmp)$open_ended & i == 1) {
              attr(outout, "min_open_ended") <- TRUE
            }
            if("upper" %in% attributes(tmp)$open_ended & i == length(tmp) -1) {
              attr(outout, "max_open_ended") <- TRUE
            }

            outout
          }),
          names(tmp)[-length(tmp)]
          )
        }),
        tmp_params
      )
    )
}

attributes(out)$generator <- "data_count"

## Other shit

# From: https://stackoverflow.com/a/47614529/1082004
make_bar <- function(x, y, width = 0.9) {
  xoff <- width/2
  data.frame(x = c(x-xoff*(1+2e-8), x-xoff*(1+1e-8), x-xoff, x+xoff, x+xoff*(1+1e-8), x+xoff*(1+2e-8)),
             height = c(NA, 0, y, y, 0, NA))
}

ggplot2::ggplot(
  dat %>%
    dplyr::group_by(year, step, area, age) %>%
    dplyr::summarise(number = sum(number)) %>%
    dplyr::group_by(year) %>%
    dplyr::mutate(p = number/sum(number)) %>%
    dplyr::mutate(bars = purrr::map2(age, p, ~make_bar(.x, .y))) %>%
    tidyr::unnest(cols = c(bars)),
  ggplot2::aes(x=x, y=year, height=2*height, group=year, fill=factor(year))) +
  ggridges::geom_ridgeline(alpha = 0.5) +
  ggplot2::scale_y_reverse(breaks = seq(1900,2050,2)) +
  ggplot2::scale_fill_viridis_d() +
  ggplot2::coord_cartesian(expand = FALSE) +
  ggplot2::labs(x = "Age", y = "Year") +
  ggplot2::theme_bw() +
  ggplot2::theme(legend.position = "none")


ggplot2::ggplot(
  fit$stock.full %>%
    dplyr::group_by(year) %>%
    dplyr::mutate(p = number/sum(number)),
  ggplot2::aes(x = length, y = year, height = 100*p, fill = stock,
               group = interaction(year, stock))) +
  ggridges::geom_ridgeline(alpha = 0.5, size = 0.5/2.13) +
  ggplot2::coord_cartesian(expand = FALSE) +
  ggplot2::scale_y_reverse(breaks = seq(1900,2050,2)) +
  ggplot2::labs(x = "Length", y = "Year", fill = "Stock") +
  ggplot2::theme_bw()

ggplot(,
) +
  geom_density_ridges(aes(x = age, y = year, height = 10*p, group = year),
                      alpha=0.5, stat = "binline", bins=diff(range(dat$age))+1) +
  # ggridges::geom_ridgeline(alpha = 0.5) +
  coord_cartesian(expand = FALSE) +
  scale_y_reverse(breaks = seq(1900,2050,2)) +
  labs(x = "Age", y = "Year", fill = "Stock") +
  theme_bw()


ggridges::geom_density_ridges(
  alpha = 0.5, stat = "identity", size = 0.5/2.13) +
  scale_fill_viridis_d() +
  ggplot(x
         aes(x = Age, y = year, height = 10*p, group = year)) +

  scale_fill_viridis_c() +
  coord_cartesian(expand = FALSE) +
  scale_y_reverse(breaks = seq(1980,2030,2)) +
  labs(x = "Age", y = "Year")

labs(x = "Length (cm)", y = "Year") +
  scale_y_reverse(breaks = seq(1980,2030,2)) +
  theme_bw()

ggplot(x %>% group_by(year) %>% mutate(p = value/sum(value)),
       aes(x = Age, y = year, height = 10*p, group = year)) +

  scale_fill_viridis_c() +
  coord_cartesian(expand = FALSE) +
  scale_y_reverse(breaks = seq(1980,2030,2)) +
  labs(x = "Age", y = "Year")

ggplot(x, #%>% group_by(year) %>% mutate(p = number/sum(number)),
       aes(x = length, y = year, #height = 10*p,
           group = interaction(age, year), fill = age)) +
  ggridges::geom_density_ridges(alpha = 0.5) +
  scale_fill_viridis_c() +
  coord_cartesian(expand = FALSE) +
  scale_y_reverse() +
  theme(legend.position = "bottom")

#' @title Aggregate count data from a data frame to gadget3
#' @description Aggregates count data from a data frame ready to be passed to gadget3. Useful for generating datasets for \code{g3l_catchdistribution}. Imitates the behavior of \code{mfdb_sample_count} and returns the required object attributes as instructed by the \code{params} argument.
#' @param x A data frame, tibble or data table
#' @param params A list of parameters defining the grouping. See details in \code{mfdb_sample_count}.
#' @param column_names A named vector explaining the column names in \code{x} if not the same than in mfdb. See the default suggestion for example.
#' @param floating_point_correction Logical indicating whether 1e-6 should be added to all numeric columns except for \code{year} and \code{step}. Corrects for floating point issues associated with certain numbers such as 28 and 57.
#' @param verbose Logical indicating whether the function should return messages about potential assumptions when expected data are not specified.
# @examples
# data_count(
#   x = y %>%
#     filter(sampling_type == "ENS",
#            gear == "BottomTrawls",
#            sex == "F",
#            !is.na(age),
#            year >= 1996),
#   params = list(
#     age = mfdb_interval(
#       "age", stock_params$minage:stock_params$maxage,
#       open_ended = c("upper")
#     ),
#     length = mfdb_interval(
#       "len",
#       seq(stock_params$minlength, stock_params$maxlength,
#           by = 5),
#       open_ended = c("upper","lower")
#     )
#   )
# )
# Debugging params
# x <- y %>% filter(sampling_type == "ENS", gear == "BottomTrawls", year >= 1996, sex == "F", !is.na(age))
# params = list(); column_names = c("year" = "year", "step" = "month", "area" = "areacell"); floating_point_correction = TRUE; verbose = TRUE
data_count <- function(x, params = list(), column_names = c("year" = "year", "step" = "month", "area" = "areacell"), floating_point_correction = TRUE, verbose = TRUE) {

  ## Functions

  # To find the number of decimal places to correct for floating point problems while aggregating. From https://stackoverflow.com/a/59022366/1082004
  # decimalplaces <- function(x) {
  #   ifelse(abs(x - round(x)) > .Machine$double.eps^0.5,
  #          nchar(sub('^\\d+\\.', '', sub('0+$', '', as.character(x)))),
  #          0)
  # }

  ## Assign x to new object for easier debugging

  dt <- x

  ### Correct inconsistency in naming timestep

  if("timestep" %in% names(params) & !"step" %in% names(params)) {
    params$step <- params$timestep
    params$timestep <- NULL
  }

  ## Rename column names

  names(dt)[match(column_names, names(dt))] <- names(column_names)

  ## Subset years

  if("year" %in% names(params))
    if(inherits(params[["year"]], "integer"))
      dt <- dt[dt$year %in% unique(params[["year"]]),]

  ## Add step and area

  if(!"step" %in% names(column_names) & !"step" %in% colnames(dt)) {
    if(verbose) message("The (time)step column not found. Assuming step = 1")
    dt$step <- 1
  }

  if(!"step" %in% names(params)) {
    if(verbose) message("(Time)step not specified. Assuming yearly aggregation. Change by defining step = mfdb_timestep_*")
    params$step$all <- sort(unique(dt$step))
  }

  if(!"area" %in% names(params)) {
    if(verbose) message("area not defined. Assuming no areal aggregation.")
    dt$area <- "all"
    if("area" %in% names(column_names)) {
      tmp <- as.character(sort(unique(x[[unname(column_names[names(column_names) == "area"])]])))
    } else {
      tmp <- "all"
    }
    params$area$all <- tmp
  }

  # if(!"age" %in% names(params)) {
  #   if(verbose) message("age not defined. Assuming no age aggregation.")
  #   dt$age <- "all"
  # }

  ## Tests

  if (!is.list(params)) {
    stop("params argument must be a list")
  }

  # if(any(!cols %in% names(params))) {
  #   stop(paste(setdiff(cols, names(params)), collapse = ", "), " are not specified in params. Tell how they should be aggregated")
  # }

  if(any(!c("year", names(params)) %in% colnames(dt))) stop(paste(setdiff(c("year", params), colnames(dt)), collapse = ", "), " is not a column in x. Specify the expected column names using column_names")

  ## Select relevant columns for easier debugging

  cols <- names(params)[!names(params) %in% c("year", "step", "area")]
  dt <- dt[c("year", "step", "area", cols)]

  ## Grouping

  dt[names(params)] <-
    lapply(names(params), function(k) {
      tmp <- params[[k]]

      if(inherits(tmp, "mfdb_group")) {
        if(all(sapply(tmp, is.numeric))) {
          tmp <- sapply(tmp, max)

          if(floating_point_correction & k %in% cols) {
            dec_places <- as.integer(names(sort(table(decimalplaces(tmp)), decreasing = TRUE))[1])
            cut(round(dt[[k]] + 1e-6, dec_places), c(-Inf, tmp), labels = names(tmp))
          } else {
            cut(dt[[k]], c(-Inf, tmp), labels = names(tmp))
          }
        } else if (all(sapply(tmp, is.character)) | all(sapply(tmp, is.factor))) {
          dplyr::recode(dt[[k]], !!!setNames(names(tmp), unlist(tmp)))
        }
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

        if(floating_point_correction & k %in% cols) {
          out <- cut(dt[[k]] + 1e-6, cut_seq, labels = labs, right = FALSE)
        } else {
          out <- cut(dt[[k]], cut_seq, labels = labs, right = FALSE)
        }

        ## Fix a special case where missing ages get aggregated as NAs even though no age aggregation is requested
        if(k == "age" & all(cut_seq %in% c(-Inf, Inf)) & nlevels(out) & any(is.na(out)))
          out[is.na(out)] <- levels(out)

        # Return
        out

      } else if(inherits(tmp, "list")) { # area and step aggregation case. Used for attributes later
        dt[[k]] <- "all"
      } else if(inherits(tmp, "integer")) { # year case, returns the column unchanged
        dt[[k]]
      } else {
        stop(class(tmp)[1], " not implemented yet.")
      }
    })



  ## Aggregation

  out <- dt %>%
    dplyr::group_by(year, step, area, dplyr::across(tidyselect::all_of(cols))) %>%
    dplyr::summarise(number = dplyr::n(), .groups = 'drop') %>%
    as.data.frame()

  out <- rapply(out, as.character, classes = "factor", how = "replace")

  ## Add stupid attributes

  if("year" %in% names(params)) {
    attributes(out)$year <-
      setNames(
        lapply(unique(params$year), function(k) k),
        lapply(unique(params$year), function(k) k)
      )
    params <- params[!names(params) %in% "year"] # Remove year from params to avoid duplicates
  } else {
    attributes(out)$year <-
      setNames(
        lapply(unique(out$year), function(k) k),
        lapply(unique(out$year), function(k) k)
      )
  }

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

  out
}

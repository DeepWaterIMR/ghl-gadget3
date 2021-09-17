#' @param survey_data output from the 1-6-3 survey data for strata analysis.R file
#' @param tokt cruise series as character. See \code{prepare_survey_data} for alternatives

prepare_strata_analysis <- function(survey_data, tokt) {

  ## Turn off s2 silently (see: https://github.com/r-spatial/sf/issues/1780)

  s2_mode <- sf::sf_use_s2()
  sink <- capture.output(sf::sf_use_s2(FALSE))
  on.exit({sink <- capture.output(sf::sf_use_s2(s2_mode))})

  ## Station based data

  x <- survey_data %>% filter(cruiseseries == tokt)

  x$interval <- cut(x$bottomdepthstart, depths.vec,
                    labels = paste(depths.vec[1:(length(depths.vec)-1)],
                                   depths.vec[2:(length(depths.vec))], sep = "-"))

  x$interval <- factor(x$interval,
                       levels = rev(paste(depths.vec[1:(length(depths.vec)-1)],
                                          depths.vec[2:(length(depths.vec))], sep = "-")))

  suppressMessages({
    x <- x %>% st_as_sf(coords = c("longitudestart", "latitudestart"), crs = 4326, remove = FALSE) %>%
      st_join(strata.poly[, c("interval", "geostrata.name", "area.nm2")]) %>%
      rename("depth.interval" = "interval.x", "interval" = "interval.y") %>%
      st_set_geometry(NULL) %>%
      add_column(strata = paste0(.$geostrata.name, ": ", .$interval), .before = "geostrata.name") %>%
      relocate(distance, biomass, cpue, .after = last_col())
  })

  suppressWarnings({
    y <- x %>%
      filter(!is.na(area.nm2)) %>%
      group_by(startyear, strata, geostrata.name, interval, area.nm2) %>%
      mutate(area.nm2 = as.numeric(area.nm2)) %>%
      summarise(mean = mean(cpue), sd = sd(cpue), n = n()) %>%
      mutate(index = area.nm2*mean/1e6,
             index.sd = area.nm2*sd/1e6,
             ci.min.t = index - qt(1 - 0.025, (n - 1))*index.sd/sqrt(n),
             ci.max.t = index + qt(1 - 0.025, (n - 1))*index.sd/sqrt(n))
  })

  list(station = x, year = y)
}

# Overview map
#' @param x list from \code{prepare_strata_analysis}
#' @param strata.poly strata polygons from \code{RstoxStrata::strataPolygon}
#' @param geostrata.df data frame containing the boundaries for geostrata. See \code{RstoxStrata::strataPolygon}
strata_station_map <- function(x, strata.poly, geostrata.df) {

  col.names <- levels(strata.poly$interval)[!grepl("Inf", levels(strata.poly$interval))]
  cols <- hcl.colors(length(col.names))
  names(cols) <- col.names

  p <- suppressMessages(basemap(data = x$station)) +
    annotation_spatial(data = strata.poly, aes(fill = interval, color = interval), alpha = 0.3, size = LS(0.1))

  reorder_layers(p) +
    geom_spatial_rect(data = geostrata.df, aes(xmin = lon.min, xmax = lon.max, ymin = lat.min, ymax = lat.max), fill = NA, color = "black", crs = 4326) +
    geom_spatial_text(data =
                        data.frame(name = LETTERS[1:nrow(geostrata.df)], lon = rowMeans(geostrata.df[c("lon.min", "lon.max")]), lat = rowMeans(geostrata.df[c("lat.min", "lat.max")])),
                      aes(x = lon, y = lat, label = name), fontface = 2, size = FS(12)) +
    geom_spatial_point(data = x$station, aes(x = longitudestart, y = latitudestart, color = interval), crs = 4326, size = 0.5) +
    scale_fill_manual("Depth strata (m)", values = cols) + scale_color_manual("Depth strata (m)", values = cols)
}

# Strata identification based on position compared to the old depth based way
#' @param x list from \code{prepare_strata_analysis}
strata_identification_plot <- function(x) {

  x$station$correct <- ifelse(is.na(x$station$interval), "Outside strata",
                             ifelse(is.na(x$station$depth.interval), "Depth missing",
                                    ifelse(as.character(x$station$depth.interval) == as.character(x$station$interval), "Identical", "Different")))

  z <- x$station %>% group_by(strata, geostrata.name, interval) %>% count(correct)
  z <- z %>% group_by(strata, geostrata.name, interval) %>% mutate(sum = sum(n), pr = n/sum)

  g <- z %>% group_by(strata, geostrata.name, interval) %>%
    summarise(sum = unique(sum), pr.correct = 100*sum(n[correct == "Identical"])/(sum(n[correct == "Different"]) + sum(n[correct == "Identical"]))) %>%
    replace_na(list(pr.correct = 0))

  ggplot() +
    geom_col(data = z, aes(x = 1, y = pr, fill = correct)) +
    coord_polar(theta = "y") +
    geom_text(data = g, aes(x = 1, y = 0, label = paste0("n = ", sum, "\n", round(pr.correct, 0), "%\ncorrect")), vjust = 0.8) +
    facet_grid(interval ~ geostrata.name) + theme_void() +
    scale_fill_manual("Type", values = c("Identical" = "palegreen3", "Different" = "indianred", "Depth missing" = "lightskyblue3", "Outside strata" = "orchid")) +
    theme(legend.position = "bottom")
}

# Number of stations / strata / year
#' @param x list from \code{prepare_strata_analysis}
stations_per_strata_plot <- function(x) {
  ggplot(x$year, aes(x = startyear, y = n, fill = area.nm2)) +
    geom_col() +
    facet_grid(interval ~ geostrata.name) +
    scale_fill_viridis_c() +
    labs(x = "Year", y = "Number of trawl stations")
}

# Overview map with average annual n / strata
#' @param x list from \code{prepare_strata_analysis}
#' @param strata.poly strata polygons from \code{RstoxStrata::strataPolygon}
#' @param geostrata.df data frame containing the boundaries for geostrata. See \code{RstoxStrata::strataPolygon}
stations_per_strata_map <- function(x, strata.poly, geostrata.df) {

  tmp <- x$year %>% group_by(geostrata.name, interval) %>%
    summarise(n = mean(n))

  p <- suppressMessages(basemap(data = x$station)) +
    annotation_spatial(data = strata.poly %>% left_join(tmp, by = c("interval", "geostrata.name")),
                       aes(fill = n), size = LS(0.1))

  reorder_layers(p) +
    geom_spatial_rect(data = geostrata.df, aes(xmin = lon.min, xmax = lon.max, ymin = lat.min, ymax = lat.max), fill = NA, color = "red", crs = 4326) +
    geom_spatial_text(data =
                        data.frame(name = LETTERS[1:nrow(geostrata.df)], lon = rowMeans(geostrata.df[c("lon.min", "lon.max")]), lat = rowMeans(geostrata.df[c("lat.min", "lat.max")])),
                      aes(x = lon, y = lat, label = name), fontface = 2, size = FS(12), color = "red") +
    scale_fill_viridis_c("Average\nnumber of\nstations\nper year", na.value = "white")

}

# Stock index
#' @param x list from \code{prepare_strata_analysis}
stock_index_plot <- function(x, type = "overview") {

  if(type == "overview") {
    ggplot(x$year, aes(x = startyear, y = index, fill = strata)) + geom_col() +
      scale_fill_viridis_d(option = "H") +
      scale_x_continuous(breaks = seq(1980, 2020, 5),
                         expand = c(0.01,0)) +
      scale_y_continuous(expand = c(0, 0)) +
      labs(x = "Year", y = "Biomass (1000 tons)", fill = "Strata")
  } else {
    ggplot(x$year, aes(x = startyear, y = index, fill = strata)) + geom_col() +
      scale_fill_viridis_d(option = "H") +
      facet_grid(interval ~ geostrata.name) +
      scale_y_continuous(expand = c(0, 0)) +
      labs(x = "Year", y = "Biomass (1000 tons)", fill = "Strata") +
      theme(legend.position = "none")
  }
}

## Settisäätöä

fit=optim_fit

## Catch distributions

x <- split(fit$catchdist.fleets, fit$catchdist.fleets$name, drop = TRUE)

lapply(x, function(k) {

  age_data <- length(unique(k$age)) == 1
  stock_re <- !any(is.na(k$stock_re))
  stock <- !any(is.na(k$stock)) & !stock_re


  if(age_data) {

    k %>%
      dplyr::group_by(.data$stock_re, .data$length) %>%
      dplyr::summarise(
      name = unique(name),
      type = "total",
      cor = cor(observed, predicted)
    )


  } else {
    out <- k %>% dplyr::summarise(
      name = unique(name),
      type = "total",
      cor = cor(observed, predicted)
      ) %>%
      dplyr::bind_rows(
        k %>%
          dplyr::group_by(.data$year, .data$step) %>%
          dplyr::summarise(
          name = unique(name),
          type = "time",
          cor = cor(observed, predicted),
          .groups = "drop"
        )
      ) %>%
      dplyr::relocate(name, type, year, step, cor) %>%
      dplyr::mutate(r2 = cor^2)
  }
})

nrows <- out %>% filter(type == "time") %>% nrow() %>% sqrt() %>% ceiling()

p2 <- ggplot(data =
out %>%
  filter(type == "time") %>%
  mutate(id = 1:nrow(.),
         row = ceiling(id/nrows)) %>%
  group_by(row) %>%
  mutate(col = seq_along(row))
) +
  geom_tile(aes(x = col, y = row, fill = r2)) +
  geom_text(aes(x = col, y = row, label = year)) +
  scale_y_reverse() +
  scale_fill_distiller(palette = "Spectral", na.value = "white", limits = c(0,1),
                       direction = 1)  +
  theme_bw() +
  labs(title = unique(out$name)) +
  theme(axis.line = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank())

p1 <- ggplot(data = out %>% filter(type == "total")) +
  geom_col(aes(x = 1, y = r2, fill = r2)) +
  scale_y_continuous(limits = c(0,1), expand = c(0,0)) +
  scale_fill_distiller(palette = "Spectral",
                       na.value = "white", limits = c(0,1),
                       direction = 1) +
  theme(legend.position = "none",
        axis.line.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        panel.border = element_rect()
        )

cowplot::plot_grid(p1,p2, rel_widths = c(1,9), align = "h")

ggplot2::ggplot() +
  ggplot2::geom_tile(
    data = out %>% dplyr::filter(type == "total"),
    ggplot2::aes(x = 0, y = 0, fill = r2)) +



ggplot(out %>%
         filter(type == "time") %>%
         mutate(angle = scales::rescale(r2)*2*pi),
       aes(x = 0, y = 0, label = year, angle = angle, radius = r2, color = r2)) +
  geom_spoke()

  geom_text() +
  coord_polar()


k <- x[[5]]
cor(k$observed, k$predicted)^2
ggplot(k, aes(x = observed, y = predicted, color = length)) + geom_smooth(method = "lm", se = F) + geom_point() + facet_wrap(~stock_re)

x <- split(fit$stockdist, fit$stockdist$name, drop = TRUE)

k <- x[[1]] %>% filter(stock_re == "female")
cor(k$obs.ratio, k$pred.ratio)^2
ggplot(k, aes(x = obs.ratio, y = pred.ratio)) + geom_point()


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

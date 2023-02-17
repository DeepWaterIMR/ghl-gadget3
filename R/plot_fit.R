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

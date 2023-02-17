

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

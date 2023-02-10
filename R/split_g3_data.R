
x = TrawlNor_ldist %>% filter(year %in% c(2020, 2021))
y = TrawlNor_split_sexratio %>% filter(year %in% c(2020, 2021))
split_column = "sex"


split_g3_data <- function(x, y, split_column) {

  length_groups <- names(attributes(y)$length) %>%
    gsub("[^0-9.-]", "", .) %>%
    as.numeric()

  first_length_group <- attributes(y)$length[1]
  last_length_group <- attributes(y)$length[length(length_groups)]

  y_avg <- y %>%
    dplyr::group_by(!!as.symbol(split_column), length) %>%
    dplyr::summarise(number = sum(number)) %>%
    dplyr::group_by(length) %>%
    dplyr::mutate(total = sum(number),
                  ratio = number/total) %>%
    dplyr::select(-total, -number) %>%
    dplyr::ungroup()

  y <- y %>%
    dplyr::group_by(year, step, area, length) %>%
    dplyr::mutate(total = sum(number),
                  ratio = number/total) %>%
    dplyr::select(-total, -number) %>%
    dplyr::ungroup()

  out <- full_join(x,y, by = c('year', 'step', 'area', 'length'), multiple = "all")

  out %>%
    dplyr::mutate(
      number = ifelse(is.na(ratio), number, round(number * ratio, 0)),
      length = factor(length, names(attributes(x)$length))) %>%
    dplyr::arrange(year, step, length) %>%
    dplyr::mutate(length = as.character(length)) %>%
    dplyr::relocate(number, .after = dplyr::last_col()) %>%
    dplyr::select(-ratio)

  TrawlNor_sexratio <-
    bind_rows(TrawlNor_sexratio,
              TrawlNor_sexratio %>%
                group_by(area, sex, length) %>%
                summarise(number = mean(number)) %>%
                ungroup() %>%
                tidyr::expand(
                  year = setdiff(1980:2021, unique(TrawlNor_sexratio$year)),
                  tidyr::nesting(sex, length, number)) %>%
                mutate(step = "1", area = "all", .after = "year")
    ) %>%
    mutate(number = floor(number*1000)) %>%
    arrange(year, step, sex, length)


  x %>% mutate(len = as.numeric(gsub("len", "", length))) %>% pull(len) %>% unique %>% sort
  y %>% mutate(len = as.numeric(gsub("len", "", length))) %>% pull(len) %>% unique %>% sort

  out <-
    full_join(
      y %>% dplyr::rename("ratio" = "number"),
      x)

  x %>%
    dplyr::left_join(
      y
    )

}

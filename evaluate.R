library(tidyverse)

nerdle <- nanoparquet::read_parquet("solutions.parquet") |>
  mutate(values = unname(as.matrix(pick(p1:p8))), .keep = "unused") |>
  mutate(ndistinct = apply(values, 1, n_distinct))

nerdle_longer <- function(nerdle) {
  with(
    nerdle,
    tibble(
      string = vctrs::vec_rep_each(string, ncol(values)),
      position = vctrs::vec_rep(seq_len(ncol(values)), nrow(values)),
      value = as.vector(t(values))
    )
  )
}

drop_symbols <- function(nerdle, x) {
  nerdle_longer(nerdle) |>
    filter(any(x %in% value), .by = string) |>
    anti_join(x = nerdle, join_by(string))
}

keep_symbols <- function(nerdle, y) {
  nerdle_longer(nerdle) |>
    filter(all(y %in% value), .by = string) |>
    semi_join(x = nerdle, join_by(string))
}

# expected number of green symbols
estimate_green <- function(nerdle) {
  nerdle_longer(nerdle) |>
    add_count(position, value) |>
    mutate(prob = n / n(), .by = position) |>
    summarise(egreen = sum(prob), .by = string) |>
    left_join(x = nerdle, join_by(string))
}

# expected number of black symbols
estimate_black <- function(nerdle) {
  rhs <- tibble(
    value = unique(as.vector(nerdle$values)),
    prob = map_dbl(value, function(a) {
      mean(apply(nerdle$values, 1, function(b) !a %in% b))
    })
  )
  nerdle_longer(nerdle) |>
    distinct(string, value) |>
    left_join(rhs, join_by(value)) |>
    summarise(eblack = sum(prob), .by = string) |>
    left_join(x = nerdle, join_by(string))
}

filter_black <- function(nerdle, x) {
  drop_symbols(nerdle, str_split_1(x, ""))
}

filter_green <- function(nerdle, x) {
  enframe(str_split_1(x, "")) |>
    filter(value %in% as.vector(nerdle$values)) |>
    pmap(function(name, value) str_sub(nerdle$string, name, name) == value) |>
    reduce(`&`) |>
    filter(.data = nerdle)
}

filter_red <- function(nerdle, x) {
  enframe(str_split_1(x, "")) |>
    filter(value %in% as.vector(nerdle$values)) |>
    pmap(function(name, value) str_sub(nerdle$string, name, name) != value) |>
    reduce(`&`) |>
    filter(.data = nerdle) |>
    keep_symbols(str_split_1(x, "")[
      str_split_1(x, "") %in% as.vector(nerdle$values)
    ])
}

# find best first guess
nerdle |>
  estimate_green() |>
  estimate_black() |>
  arrange(desc(ndistinct), desc(egreen))

# example to find best second guess
nerdle |>
  filter_black("4-37") |>
  filter_green("x0xxx=xx") |>
  filter_red("xxx2xx1x") |>
  estimate_green() |>
  estimate_black() |>
  arrange(desc(eblack), desc(egreen))

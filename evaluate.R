library(tidyverse)

nerdle <- nanoparquet::read_parquet("solutions.parquet") |>
  mutate(ndistinct = apply(pick(p1:p8), 1, n_distinct))

drop_symbols <- function(nerdle, x) {
  nerdle |> filter(!if_any(p1:p8, ~ . %in% x))
}

keep_symbols <- function(nerdle, y) {
  reduce(y, function(a, b) filter(a, if_any(p1:p8, ~ . %in% b)), .init = nerdle)
}

# expected number of green symbols
estimate_green <- function(nerdle) {
  nerdle |>
    mutate(
      egreen = rowSums(across(p1:p8, function(p) {
        add_count(tibble(p), p)$n / length(p)
      }))
    )
}

# expected number of black symbols
estimate_black <- function(nerdle) {
  nerdle_longer <- nerdle |> pivot_longer(p1:p8)
  rhs <- nerdle_longer |>
    distinct(value) |>
    mutate(
      prob = map_dbl(value, function(a) {
        mean(apply(nerdle |> select(p1:p8), 1, function(b) !a %in% b))
      })
    )
  nerdle_longer |>
    distinct(string, value) |>
    left_join(rhs, join_by(value)) |>
    summarise(eblack = sum(prob), .by = string) |>
    left_join(x = nerdle, join_by(string))
}

# example to find best second guess
nerdle |>
  drop_symbols(c(9, "-", 5, 7, 1)) |>
  keep_symbols(c(6, 2)) |>
  filter(p1 == 6, p6 != "=", p8 != 2) |>
  estimate_green() |>
  estimate_black() |>
  arrange(desc(ndistinct), desc(egreen), desc(eblack))

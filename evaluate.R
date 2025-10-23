library(tidyverse)

nerdle <- nanoparquet::read_parquet("solutions.parquet") |>
  mutate(ndistinct = apply(pick(p1:p8), 1, n_distinct))

nerdle_longer <- function(nerdle) {
  nerdle |> select(string, p1:p8) |> pivot_longer(-string)
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
    add_count(name, value) |>
    mutate(prob = n / n(), .by = name) |>
    summarise(egreen = sum(prob), .by = string) |>
    left_join(x = nerdle, join_by(string))
}

# expected number of black symbols
estimate_black <- function(nerdle) {
  rhs <- nerdle_longer(nerdle) |>
    distinct(value) |>
    mutate(
      prob = map_dbl(value, function(a) {
        mean(apply(nerdle |> select(p1:p8), 1, function(b) !a %in% b))
      })
    )
  nerdle_longer(nerdle) |>
    distinct(string, value) |>
    left_join(rhs, join_by(value)) |>
    summarise(eblack = sum(prob), .by = string) |>
    left_join(x = nerdle, join_by(string))
}

# find best first guess
nerdle |>
  estimate_green() |>
  estimate_black() |>
  arrange(desc(ndistinct), desc(egreen))

# example to find best second guess
nerdle |>
  drop_symbols(c(4, "-", 3, 7)) |>
  keep_symbols(c(2, 1)) |>
  filter(p2 == 0, p4 != 2, p6 == "=", p7 != 1) |>
  estimate_green() |>
  estimate_black() |>
  arrange(desc(eblack), desc(egreen))

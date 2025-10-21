library(tidyverse)

nerdle <- nanoparquet::read_parquet("solutions.parquet") |>
  mutate(ndistinct = apply(pick(p1:p8), 1, n_distinct))

drop_symbols <- function(nerdle, x) {
  nerdle |> filter(!if_any(p1:p8, ~ . %in% x))
}

keep_symbols <- function(nerdle, y) {
  reduce(y, function(a, b) filter(a, if_any(p1:p8, ~ . %in% b)), .init = nerdle)
}

# expected number of correct symbols
estimate_correct <- function(nerdle) {
  nerdle |>
    mutate(
      ecorrect = rowSums(across(p1:p8, function(p) {
        add_count(tibble(p), p)$n / length(p)
      }))
    )
}

# example to find best second guess
nerdle |>
  drop_symbols(c(9, "-", 5, 7, 1)) |>
  keep_symbols(c(6, 2)) |>
  filter(p1 == 6, p6 != "=", p8 != 2) |>
  estimate_correct() |>
  arrange(desc(ndistinct), desc(ecorrect))

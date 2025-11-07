library(tidyverse)

data <- nanoparquet::read_parquet("solutions.parquet") |>
  select(string) |>
  mutate(
    left = str_extract(string, "[^=]+"),
    right = str_extract(string, "=.+$"),
    plus = str_split(str_replace_all(left, fixed("-"), "+-"), fixed("+"))
  ) |>
  unnest(plus) |>
  mutate(
    times = str_replace_all(plus, fixed("/"), "*/") |>
      str_replace_all(fixed("-"), "-*") |>
      str_split(fixed("*"))
  ) |>
  unnest(times) |>
  arrange(times, plus) |>
  nest(times = times) |>
  select(-plus) |>
  nest(times = times)

commutations <- left_join(
  data,
  data,
  join_by(right),
  suffix = c("", "c"),
  relationship = "many-to-many"
) |>
  filter(map2_lgl(times, timesc, identical)) |>
  select(string, stringc)

nanoparquet::write_parquet(commutations, "commutations.parquet")

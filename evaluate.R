library(tidyverse)

duckplyr::db_exec("PRAGMA memory_limit = '5GB'")

nerdle <- nanoparquet::read_parquet("solutions.parquet") |>
  mutate(values = unname(as.matrix(pick(p1:p8))), .keep = "unused") |>
  mutate(ndistinct = apply(values, 1, n_distinct))

key <- nanoparquet::read_parquet("commutations.parquet") |>
  slice_min(stringc, by = string)

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

# expected number of remaining possible solutions
estimate_remaining <- function(rest, guesses = rest) {
  join <- duckplyr::as_duckdb_tibble(
    expand_grid(guess = guesses$string, solution = rest$string),
    prudence = "stingy"
  ) |>
    left_join(
      nerdle_longer(guesses) |> count(string, value, name = "count_guess"),
      join_by(guess == string),
      relationship = "many-to-many"
    ) |>
    left_join(
      nerdle_longer(rest) |> count(string, value, name = "count_solution"),
      join_by(solution == string, value)
    ) |>
    mutate(across(count_solution, ~ if_else(is.na(.), 0, .)))
  known_exact <- join |> filter(count_guess > count_solution)
  known_lower <- join |> filter(count_guess <= count_solution)
  step0 <- duckplyr::as_duckdb_tibble(
    expand_grid(
      guess = guesses$string,
      solution = rest$string,
      string = rest$string
    ),
    prudence = "stingy"
  )
  step1 <- nerdle_longer(rest) |>
    count(string, value) |>
    duckplyr::as_duckdb_tibble(prudence = "stingy") |>
    left_join(known_exact, join_by(value), relationship = "many-to-many") |>
    filter(n != count_solution) |>
    anti_join(x = step0, join_by(guess, solution, string))
  step2 <- nerdle_longer(rest) |>
    count(string, value) |>
    complete(string, value, fill = list(n = 0)) |>
    duckplyr::as_duckdb_tibble(prudence = "stingy") |>
    left_join(known_lower, join_by(value), relationship = "many-to-many") |>
    filter(n < count_guess) |>
    anti_join(x = step1, join_by(guess, solution, string))
  step3 <- step2 |>
    left_join(
      nerdle_longer(rest),
      join_by(string),
      relationship = "many-to-many"
    ) |>
    left_join(
      nerdle_longer(guesses) |> rename(guess = string, value_guess = value),
      join_by(guess, position)
    ) |>
    left_join(
      nerdle_longer(rest) |> rename(solution = string, value_solution = value),
      join_by(solution, position)
    ) |>
    mutate(green = value_guess == value_solution)
  drop1 <- step3 |>
    filter(green) |>
    summarise(
      drop = !all(value == value_guess),
      .by = c(guess, solution, string)
    ) |>
    filter(drop)
  drop2 <- step3 |>
    filter(!green) |>
    summarise(
      drop = any(value == value_guess),
      .by = c(guess, solution, string)
    ) |>
    filter(drop)
  step3 |>
    distinct(guess, solution, string) |>
    anti_join(drop1, join_by(guess, solution, string)) |>
    anti_join(drop2, join_by(guess, solution, string)) |>
    left_join(
      duckplyr::as_duckdb_tibble(key, prudence = "stingy"),
      join_by(string)
    ) |>
    distinct(guess, solution, stringc) |>
    count(guess, solution) |>
    summarise(eremain = mean(n), .by = guess) |>
    left_join(x = guesses, join_by(string == guess))
}

# find best first guess
nerdle |>
  estimate_green() |>
  estimate_black() |>
  arrange(desc(ndistinct), desc(egreen))

# example to find best second guess
rest <- nerdle |>
  filter_black("4-37") |>
  filter_green("x0xxx=xx") |>
  filter_red("xxx2xx1x")

rest |>
  estimate_remaining() |>
  estimate_green() |>
  estimate_black() |>
  arrange(eremain, desc(egreen), desc(eblack))

rest |> estimate_remaining(nerdle) |> arrange(eremain)

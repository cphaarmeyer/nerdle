library(tidyverse)

duckplyr::db_exec("PRAGMA memory_limit = '10GB'")

nerdle <- nanoparquet::read_parquet("solutions.parquet") |>
  mutate(values = unname(as.matrix(pick(p1:p8))), .keep = "unused")

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

add_ndistinct <- function(nerdle) {
  nerdle |> mutate(ndistinct = apply(values, 1, n_distinct))
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

filter_nerdle <- function(nerdle, guess, result) {
  df <- tibble(
    guess = str_split_1(guess, ""),
    result = str_split_1(result, "")
  ) |>
    mutate(position = row_number())
  b <- df |> filter(result == "b")
  known_exact <- union(b, semi_join(df, b, join_by(guess))) |>
    summarise(exact = sum(result != "b"), .by = guess)
  known_lower <- df |> filter(result != "b") |> count(guess, name = "lower")
  step1 <- nerdle_longer(nerdle) |>
    count(string, value) |>
    inner_join(known_exact, join_by(value == guess)) |>
    filter(n != exact) |>
    anti_join(x = nerdle, join_by(string))
  step2 <- nerdle_longer(step1) |>
    count(string, value) |>
    complete(string, value, fill = list(n = 0)) |>
    inner_join(known_lower, join_by(value == guess)) |>
    filter(n < lower) |>
    anti_join(x = step1, join_by(string))
  step3 <- nerdle_longer(step2) |> left_join(df, join_by(position))
  drop1 <- step3 |>
    filter(result == "g") |>
    summarise(drop = !all(value == guess), .by = string) |>
    filter(drop)
  drop2 <- step3 |>
    filter(result != "g") |>
    summarise(drop = any(value == guess), .by = string) |>
    filter(drop)
  step2 |>
    anti_join(drop1, join_by(string)) |>
    anti_join(drop2, join_by(string))
}

list_remaining <- function(guesses, solutions, set = solutions) {
  guesses_longer <- nerdle_longer(guesses)
  solutions_longer <- nerdle_longer(solutions)
  set_longer <- nerdle_longer(set)
  join <- duckplyr::as_duckdb_tibble(
    expand_grid(guess = guesses$string, solution = solutions$string),
    prudence = "stingy"
  ) |>
    left_join(
      guesses_longer |> count(string, value, name = "count_guess"),
      join_by(guess == string),
      relationship = "many-to-many"
    ) |>
    left_join(
      solutions_longer |> count(string, value, name = "count_solution"),
      join_by(solution == string, value)
    ) |>
    mutate(across(count_solution, ~ if_else(is.na(.), 0, .)))
  known_exact <- join |> filter(count_guess > count_solution)
  known_lower <- join |> filter(count_guess <= count_solution)
  step0 <- duckplyr::as_duckdb_tibble(
    expand_grid(
      guess = guesses$string,
      solution = solutions$string,
      string = set$string
    ),
    prudence = "stingy"
  )
  step1 <- set_longer |>
    count(string, value) |>
    duckplyr::as_duckdb_tibble(prudence = "stingy") |>
    left_join(known_exact, join_by(value), relationship = "many-to-many") |>
    filter(n != count_solution) |>
    anti_join(x = step0, join_by(guess, solution, string))
  step2 <- set_longer |>
    count(string, value) |>
    complete(string, value, fill = list(n = 0)) |>
    duckplyr::as_duckdb_tibble(prudence = "stingy") |>
    left_join(known_lower, join_by(value), relationship = "many-to-many") |>
    filter(n < count_guess) |>
    anti_join(x = step1, join_by(guess, solution, string))
  step3 <- step2 |>
    left_join(set_longer, join_by(string), relationship = "many-to-many") |>
    left_join(
      guesses_longer |> rename(guess = string, value_guess = value),
      join_by(guess, position)
    ) |>
    left_join(
      solutions_longer |> rename(solution = string, value_solution = value),
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
    anti_join(drop2, join_by(guess, solution, string))
}

# expected number of remaining possible solutions
# expected chance to guess correct
estimate_remaining <- function(rest, guesses = rest) {
  remaining <- list_remaining(guesses, rest)
  remaining |>
    left_join(
      duckplyr::as_duckdb_tibble(key, prudence = "stingy") |>
        rename(solution = string, solutionc = stringc),
      join_by(solution)
    ) |>
    left_join(
      duckplyr::as_duckdb_tibble(key, prudence = "stingy"),
      join_by(string)
    ) |>
    summarise(
      n = n_distinct(stringc),
      p = mean(as.integer(solutionc == stringc)),
      .by = c(guess, solution)
    ) |>
    summarise(eremain = mean(n), echance = mean(p), .by = guess) |>
    left_join(x = guesses, join_by(string == guess))
}

# example to find best second guess
rest <- nerdle |> filter_nerdle("48-36=12", "brrbggbb")

add_ndistinct(rest) |>
  estimate_remaining() |>
  estimate_green() |>
  estimate_black() |>
  arrange(desc(echance), eremain, desc(egreen), desc(eblack))

ranking <- rest |> estimate_remaining(nerdle) |> add_ndistinct()
ranking |> arrange(desc(echance))
ranking |> slice_max(echance)

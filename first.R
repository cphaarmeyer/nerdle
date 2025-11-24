library(tidyverse)

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

list_remaining <- function(guesses, solutions, set = solutions) {
  guesses_longer <- nerdle_longer(guesses)
  solutions_longer <- nerdle_longer(solutions)
  set_longer <- nerdle_longer(set)
  join <- expand_grid(guess = guesses$string, solution = solutions$string) |>
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
  step0 <- expand_grid(
    guess = guesses$string,
    solution = solutions$string,
    string = set$string
  )
  step1 <- set_longer |>
    count(string, value) |>
    left_join(known_exact, join_by(value), relationship = "many-to-many") |>
    filter(n != count_solution) |>
    anti_join(x = step0, join_by(guess, solution, string))
  step2 <- set_longer |>
    count(string, value) |>
    complete(string, value, fill = list(n = 0)) |>
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
    anti_join(drop2, join_by(guess, solution, string)) |>
    left_join(key, join_by(string)) |>
    distinct(guess, solution, stringc)
}

make_chunks <- function(x, chunk_size, min_chunks) {
  size <- nrow(x)
  nchunks <- max(ceiling(size / chunk_size), min_chunks)
  if (nchunks == 1) {
    return(list(x))
  }
  grp <- cut(seq_len(size), nchunks, labels = FALSE)
  unname(split(x, grp))
}

out <- expand_grid(
  guesses = nerdle |>
    filter(
      apply(values, 1, n_distinct) == 8,
      values[, 3] == "-",
      values[, 6] == "="
    ) |>
    vctrs::vec_chop(),
  solutions = make_chunks(nerdle, 2000, 10)
) |>
  mutate(
    res = map2(
      guesses,
      solutions,
      function(guesses, solutions) {
        gc()
        list_remaining(guesses, solutions, set = nerdle) |> count(solution)
      },
      .progress = TRUE
    )
  )
ranking <- out |>
  select(-solutions) |>
  unnest(res) |>
  summarise(eremain = mean(n), sd = sd(n), .by = guesses) |>
  unnest(guesses)
ranking |> arrange(eremain)

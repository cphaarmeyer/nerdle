library(tidyverse)

numbers <- as.character(0:9)
operators <- c("+", "-", "*", "/")

chunk1 <- expand_grid(
  p1 = setdiff(numbers, "0"),
  p2 = c(numbers, operators),
  p3 = c(numbers, operators),
  p4 = numbers
) |>
  filter(xor(p2 %in% operators, p3 %in% operators)) |>
  mutate(expr = str_c(p1, p2, p3, p4)) |>
  filter(str_detect(expr, "\\D0", negate = TRUE)) |>
  mutate(res = map_dbl(expr, compose(eval, str2lang))) |>
  filter(res >= 0, map_lgl(res, rlang::is_integerish)) |>
  mutate(across(res, as.character)) |>
  filter(str_length(res) == 3) |>
  mutate(
    p5 = "=",
    p6 = str_sub(res, 1, 1),
    p7 = str_sub(res, 2, 2),
    p8 = str_sub(res, 3, 3)
  )

chunk2 <- expand_grid(
  p1 = setdiff(numbers, "0"),
  p2 = c(numbers, operators),
  p3 = c(numbers, operators),
  p4 = c(numbers, operators),
  p5 = numbers
) |>
  filter(
    p2 %in% operators | p3 %in% operators | p4 %in% operators,
    !(p2 %in% operators & p3 %in% operators),
    !(p3 %in% operators & p4 %in% operators)
  ) |>
  mutate(expr = str_c(p1, p2, p3, p4, p5)) |>
  filter(str_detect(expr, "\\D0", negate = TRUE)) |>
  mutate(res = map_dbl(expr, compose(eval, str2lang))) |>
  filter(res >= 0, map_lgl(res, rlang::is_integerish)) |>
  mutate(across(res, as.character)) |>
  filter(str_length(res) == 2) |>
  mutate(p6 = "=", p7 = str_sub(res, 1, 1), p8 = str_sub(res, 2, 2))

chunk3 <- expand_grid(
  p1 = setdiff(numbers, "0"),
  p2 = c(numbers, operators),
  p3 = c(numbers, operators),
  p4 = c(numbers, operators),
  p5 = c(numbers, operators),
  p6 = numbers
) |>
  filter(
    p2 %in%
      operators |
      p3 %in% operators |
      p4 %in% operators |
      p5 %in% operators,
    !(p2 %in% operators & p3 %in% operators),
    !(p3 %in% operators & p4 %in% operators),
    !(p4 %in% operators & p5 %in% operators)
  ) |>
  mutate(expr = str_c(p1, p2, p3, p4, p5, p6)) |>
  filter(str_detect(expr, "\\D0", negate = TRUE)) |>
  mutate(res = map_dbl(expr, compose(eval, str2lang))) |>
  filter(res >= 0, map_lgl(res, rlang::is_integerish)) |>
  mutate(across(res, as.character)) |>
  filter(str_length(res) == 1) |>
  mutate(p7 = "=", p8 = res)

bind_rows(chunk1, chunk2, chunk3) |>
  select(-expr, -res) |>
  mutate(string = str_c(p1, p2, p3, p4, p5, p6, p7, p8)) |>
  nanoparquet::write_parquet("solutions.parquet")

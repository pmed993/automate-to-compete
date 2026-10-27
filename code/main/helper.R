safe_count <- function(x) {
  sum(!is.na(x))
}


safe_mean <- function(x) {
  if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)
}


safe_sd <- function(x) {
  if (sum(!is.na(x)) <= 1) NA_real_ else sd(x, na.rm = TRUE)
}


safe_min <- function(x) {
  if (all(is.na(x))) NA_real_ else min(x, na.rm = TRUE)
}


safe_max <- function(x) {
  if (all(is.na(x))) NA_real_ else max(x, na.rm = TRUE)
}


safe_quantile <- function(x, probs) {
  if (all(is.na(x))) {
    rep(NA_real_, length(probs))
  } else {
    as.numeric(quantile(x, probs = probs, na.rm = TRUE, names = FALSE))
  }
}


summary_stats <- function(data, vars) {
  summary <- data |>
    summarise(
      across(
        all_of(vars),
        list(
          count = ~safe_count(.),
          mean = ~safe_mean(.),
          sd = ~safe_sd(.),
          min = ~safe_min(.),
          max = ~safe_max(.)
        ),
        .names = "{.col}&&{.fn}"
      )
    ) |>
    pivot_longer(
      cols = everything(),
      names_to = c("variable", "stat"),
      names_sep = "&&",
      values_to = "value"
    ) |>
    pivot_wider(names_from = stat, values_from = value) |>
    select(variable, mean, sd, min, max, count)

  return(summary)
}


quick_stats <- function(x) {
  c(
    N = safe_count(x),
    mean = safe_mean(x),
    sd = safe_sd(x),
    p01 = safe_quantile(x, 0.01),
    p05 = safe_quantile(x, 0.05),
    p50 = safe_quantile(x, 0.50),
    p95 = safe_quantile(x, 0.95),
    p99 = safe_quantile(x, 0.99)
  )
}

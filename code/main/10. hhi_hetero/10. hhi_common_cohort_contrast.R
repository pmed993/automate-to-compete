#!/usr/bin/env Rscript

# Supplement to the cohort-g-1 HHI heterogeneity analysis. Both competition
# regimes receive the same observed treated-cohort weights. Inference is
# conditional on those weights and aligns firms shared by the two regimes.

suppressPackageStartupMessages({
  library(did)
  library(dplyr)
  library(purrr)
  library(readr)
  library(tibble)
  library(tidyr)
})

root <- Sys.getenv("HHI_OUTPUT_ROOT", unset = "output")
output_dir <- file.path(root, "hhi_heterogeneity_did")
results <- readRDS(file.path(output_dir, "hhi_heterogeneity_results.rds"))
biters <- as.integer(results$design$bootstrap_iterations)

extract_group <- function(treatment_id, outcome, regime) {
  key <- paste(treatment_id, outcome, regime, sep = "::")
  model <- results$models[[key]]
  if (is.null(model)) stop("Missing model: ", key)
  group <- did::aggte(
    model, type = "group", na.rm = TRUE, bstrap = FALSE, cband = FALSE,
    clustervars = model$DIDparams$clustervars
  )
  counts <- as_tibble(model$DIDparams$data) |>
    filter(cohort > 0L) |>
    count(cohort, name = "treated_firms")
  list(
    group = group,
    counts = counts,
    firm_id = model$DIDparams$data$firm_id,
    cluster = as.character(model$DIDparams$cluster_vector)
  )
}

standardize_pair <- function(treatment_id, outcome, seed) {
  high <- extract_group(treatment_id, outcome, "high_competition")
  low <- extract_group(treatment_id, outcome, "low_competition")
  cohort_counts <- full_join(
    high$counts |> rename(high_firms = treated_firms),
    low$counts |> rename(low_firms = treated_firms),
    by = "cohort"
  ) |>
    filter(!is.na(high_firms), !is.na(low_firms)) |>
    mutate(pooled_firms = high_firms + low_firms)
  cohorts <- cohort_counts$cohort
  if (length(cohorts) == 0L) stop("No common cohorts: ", treatment_id, "/", outcome)
  weights <- cohort_counts$pooled_firms / sum(cohort_counts$pooled_firms)

  score <- function(x) {
    positions <- match(cohorts, x$group$egt)
    if (anyNA(positions)) stop("Missing group effects for a common cohort.")
    influence <- as.matrix(
      x$group$inf.function$selective.inf.func.g[, positions, drop = FALSE]
    )
    list(
      estimate = sum(weights * x$group$att.egt[positions]),
      influence = as.numeric(influence %*% weights)
    )
  }
  high_score <- score(high)
  low_score <- score(low)
  if (anyNA(high$cluster) || anyNA(low$cluster) ||
      anyDuplicated(high$firm_id) || anyDuplicated(low$firm_id)) {
    stop("Misaligned firm or cluster metadata.")
  }

  aligned <- full_join(
    tibble(firm_id = high$firm_id, high_if = high_score$influence,
           high_cluster = high$cluster),
    tibble(firm_id = low$firm_id, low_if = low_score$influence,
           low_cluster = low$cluster),
    by = "firm_id"
  )
  if (any(!is.na(aligned$high_cluster) & !is.na(aligned$low_cluster) &
          aligned$high_cluster != aligned$low_cluster)) {
    stop("A shared firm has inconsistent market clusters.")
  }
  n_union <- nrow(aligned)
  n_high <- length(high$firm_id)
  n_low <- length(low$firm_id)
  contrast_if <- n_union / n_high * coalesce(aligned$high_if, 0) -
    n_union / n_low * coalesce(aligned$low_if, 0)
  cluster <- coalesce(aligned$high_cluster, aligned$low_cluster)
  n_clusters <- n_distinct(cluster)
  params <- high$group$DIDparams
  cluster_var <- params$cluster_vector_var
  if (is.null(cluster_var) || length(cluster_var) != 1L) {
    stop("Missing stable market cluster variable.")
  }
  params$cluster_vector <- cluster
  params$cluster_vector_var <- cluster_var
  params$clustervars <- c("firm_id", cluster_var)
  params$biters <- biters
  params$cband <- FALSE
  set.seed(seed)
  bootstrap <- getFromNamespace("mboot", "did")(
    matrix(contrast_if, ncol = 1L), params, return_V = FALSE
  )
  difference <- high_score$estimate - low_score$estimate
  bootstrap_error <- as.numeric(bootstrap$bres[, 1L]) *
    sqrt(n_clusters) / n_union
  tibble(
    treatment_id, outcome,
    high_att_common_weights = high_score$estimate,
    low_att_common_weights = low_score$estimate,
    difference_common_weights = difference,
    std_error_difference = as.numeric(bootstrap$se[[1L]]),
    p_value_difference =
      (1 + sum(abs(bootstrap_error) >= abs(difference))) /
        (length(bootstrap_error) + 1),
    common_cohorts = length(cohorts),
    high_treated_firms = sum(cohort_counts$high_firms),
    low_treated_firms = sum(cohort_counts$low_firms),
    market_clusters = n_clusters,
    bootstrap_iterations = biters,
    weights = "Pooled treated-firm cohort shares, fixed at observed values"
  )
}

grid <- results$difference_results |>
  select(treatment_id, outcome) |>
  mutate(seed = seq_len(n()) + 20000L)
standardized <- pmap_dfr(grid, standardize_pair)
write_csv(
  standardized,
  file.path(output_dir, "hhi_high_minus_low_common_cohort_weights.csv")
)
print(standardized |>
  select(treatment_id, outcome, difference_common_weights,
         std_error_difference, p_value_difference))

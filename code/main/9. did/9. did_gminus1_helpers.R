# Cohort-specific pre-treatment covariates for an unbalanced C&S design.
#
# This project-side helper is intentionally separate from the installed `did`
# package.  Stock att_gt() cannot make a comparison firm's covariates depend on
# the treated cohort currently being evaluated.  The functions below therefore
# estimate each 2x2 ATT(g,t) with DRDID, put all influence functions on a common
# firm universe, and return a did::MP object that did::aggte() can aggregate.

did_capture_conditions <- function(expr) {
  warnings <- character()
  value <- tryCatch(
    withCallingHandlers(
      expr,
      warning = function(w) {
        warnings <<- c(warnings, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) {
      structure(
        list(message = conditionMessage(e)),
        class = "did_gminus1_error"
      )
    }
  )
  list(value = value, warnings = unique(warnings))
}

did_collapse_messages <- function(x) {
  x <- unique(trimws(x[nzchar(trimws(x))]))
  if (length(x) == 0L) "None" else paste(x, collapse = " | ")
}

did_build_formula <- function(controls) {
  if (length(controls) == 0L) ~1 else reformulate(controls)
}

did_independent_columns <- function(x, control_rows) {
  if (ncol(x) == 1L) return(1L)
  qr_fit <- qr(x[control_rows, , drop = FALSE], tol = 1e-10, LAPACK = FALSE)
  sort(qr_fit$pivot[seq_len(qr_fit$rank)])
}

did_estimate_gminus1_cell <- function(
    data,
    outcome,
    cohort_year,
    target_year,
    est_method,
    controls,
    min_cell_size = 5L,
    overlap_threshold = 0.995,
    eligible_filter = NULL
) {
  baseline_year <- cohort_year - 1L
  outcome_base_year <- if (target_year >= cohort_year) {
    baseline_year
  } else {
    target_year - 1L
  }

  # The not-yet-treated rule is target-period specific.  For ATT(g,t), an
  # eligible comparison firm is never treated or first treated strictly after
  # t.  Cohort-g firms are the treated group even in their placebo comparisons.
  eligible <- data |>
    distinct(firm_id, cohort) |>
    filter(
      cohort == cohort_year |
        cohort == 0L |
        cohort > target_year
    )
  if (!is.null(eligible_filter)) {
    eligible <- eligible_filter(
      eligible, data, cohort_year, target_year
    )
    if (anyDuplicated(eligible$firm_id) ||
        !all(c("firm_id", "cohort") %in% names(eligible))) {
      stop("eligible_filter must return unique firm_id/cohort rows.")
    }
  }

  eligible_treated <- eligible |>
    filter(cohort == cohort_year) |>
    pull(firm_id)
  eligible_controls <- eligible |>
    filter(cohort == 0L | cohort > target_year) |>
    pull(firm_id)

  baseline <- data |>
    filter(
      year == baseline_year,
      firm_id %in% c(eligible_treated, eligible_controls)
    ) |>
    select(firm_id, all_of(controls))
  if (length(controls) > 0L) {
    baseline <- baseline |>
      rename_with(~ paste0("x_", .x), all_of(controls))
  }

  y0 <- data |>
    filter(
      year == outcome_base_year,
      firm_id %in% c(eligible_treated, eligible_controls)
    ) |>
    select(firm_id, y0 = all_of(outcome))
  y1 <- data |>
    filter(
      year == target_year,
      firm_id %in% c(eligible_treated, eligible_controls)
    ) |>
    select(firm_id, y1 = all_of(outcome))

  cell <- eligible |>
    inner_join(y0, by = "firm_id") |>
    inner_join(y1, by = "firm_id") |>
    inner_join(baseline, by = "firm_id") |>
    mutate(D = as.integer(cohort == cohort_year)) |>
    filter(
      is.finite(y0),
      is.finite(y1),
      if_all(starts_with("x_"), ~ !is.na(.x))
    )

  n_treated <- sum(cell$D == 1L)
  n_controls <- sum(cell$D == 0L)
  base_result <- list(
    cohort_year = as.integer(cohort_year),
    target_year = as.integer(target_year),
    event_time = as.integer(target_year - cohort_year),
    baseline_year = as.integer(baseline_year),
    outcome_base_year = as.integer(outcome_base_year),
    eligible_treated = length(eligible_treated),
    eligible_controls = length(eligible_controls),
    n_treated = n_treated,
    n_controls = n_controls,
    treated_lost = length(eligible_treated) - n_treated,
    controls_lost = length(eligible_controls) - n_controls
  )

  fail <- function(message) {
    c(
      base_result,
      list(
        estimate = NA_real_,
        std_error = NA_real_,
        local_inf = NULL,
        contributor_ids = integer(),
        treated_contributor_ids = integer(),
        min_propensity = NA_real_,
        max_propensity = NA_real_,
        min_treated_propensity = NA_real_,
        max_treated_propensity = NA_real_,
        min_control_propensity = NA_real_,
        max_control_propensity = NA_real_,
        max_control_odds_weight = NA_real_,
        support_status = "FAILED",
        support_warning = message,
        propensity_model_note = "None"
      )
    )
  }

  if (n_treated < min_cell_size || n_controls < min_cell_size) {
    return(fail(
      "Fewer than the required treated or control firms after cell-specific completeness filters"
    ))
  }

  if ("naics2_f" %in% controls) {
    treated_sectors <- unique(as.character(cell$x_naics2_f[cell$D == 1L]))
    control_sectors <- unique(as.character(cell$x_naics2_f[cell$D == 0L]))
    unsupported_treated <- setdiff(treated_sectors, control_sectors)
    if (length(unsupported_treated) > 0L) {
      return(fail(paste0(
        "No eligible controls in treated NAICS2 sector(s): ",
        paste(sort(unsupported_treated), collapse = ", ")
      )))
    }
    cell$x_naics2_f <- factor(
      as.character(cell$x_naics2_f),
      levels = levels(data$naics2_f)
    )
  }

  cell_controls <- if (length(controls) == 0L) {
    character()
  } else {
    paste0("x_", controls)
  }
  x <- model.matrix(
    did_build_formula(cell_controls),
    data = cell
  )
  keep <- did_independent_columns(x, cell$D == 0L)
  x <- x[, keep, drop = FALSE]

  if (ncol(x) >= n_controls || ncol(x) >= nrow(cell) - 2L) {
    return(fail(
      "Covariate dimension is too large for the available treated/control firms"
    ))
  }

  propensity_capture <- did_capture_conditions(
    glm.fit(x = x, y = cell$D, family = binomial())
  )
  propensity <- if (inherits(propensity_capture$value, "did_gminus1_error")) {
    rep(NA_real_, nrow(cell))
  } else {
    propensity_capture$value$fitted.values
  }

  estimator <- if (est_method == "dr") {
    DRDID::drdid_panel
  } else if (est_method == "reg") {
    DRDID::reg_did_panel
  } else {
    stop("est_method must be either 'dr' or 'reg'.")
  }

  estimate_capture <- did_capture_conditions(
    estimator(
      y1 = cell$y1,
      y0 = cell$y0,
      D = cell$D,
      covariates = x,
      boot = FALSE,
      inffunc = TRUE
    )
  )
  if (inherits(estimate_capture$value, "did_gminus1_error")) {
    return(fail(did_collapse_messages(c(
      propensity_capture$warnings,
      estimate_capture$warnings,
      estimate_capture$value$message
    ))))
  }

  propensity_problem <- if (
    inherits(propensity_capture$value, "did_gminus1_error") ||
      !isTRUE(propensity_capture$value$converged) ||
      any(!is.finite(propensity_capture$value$coefficients))
  ) {
    "Propensity model did not converge to finite coefficients"
  } else {
    character()
  }
  overlap_flags <- c(
    if (any(is.finite(propensity)) &&
        max(propensity[cell$D == 1L], na.rm = TRUE) >= overlap_threshold) {
      paste0("treated propensity >= ", overlap_threshold)
    } else {
      character()
    },
    if (any(is.finite(propensity)) &&
        max(propensity[cell$D == 0L], na.rm = TRUE) >= overlap_threshold) {
      paste0("control propensity >= ", overlap_threshold)
    } else {
      character()
    },
    propensity_problem,
    estimate_capture$warnings
  )

  finite_summary <- function(z, fun) {
    z <- z[is.finite(z)]
    if (length(z) == 0L) NA_real_ else fun(z)
  }
  control_odds <- propensity[cell$D == 0L] /
    pmax(1 - propensity[cell$D == 0L], 1e-12)

  c(
    base_result,
    list(
      estimate = as.numeric(estimate_capture$value$ATT),
      std_error = as.numeric(estimate_capture$value$se),
      local_inf = as.numeric(estimate_capture$value$att.inf.func),
      contributor_ids = cell$firm_id,
      treated_contributor_ids = cell$firm_id[cell$D == 1L],
      min_propensity = finite_summary(propensity, min),
      max_propensity = finite_summary(propensity, max),
      min_treated_propensity = finite_summary(propensity[cell$D == 1L], min),
      max_treated_propensity = finite_summary(propensity[cell$D == 1L], max),
      min_control_propensity = finite_summary(propensity[cell$D == 0L], min),
      max_control_propensity = finite_summary(propensity[cell$D == 0L], max),
      max_control_odds_weight = finite_summary(control_odds, max),
      support_status = if (length(overlap_flags) == 0L) "PASS" else "WARNING",
      support_warning = did_collapse_messages(overlap_flags),
      propensity_model_note = did_collapse_messages(propensity_capture$warnings)
    )
  )
}

did_dynamic_event_wald_test <- function(
    dynamic,
    event_times
) {
  required_events <- sort(unique(as.integer(event_times)))
  if (length(required_events) == 0L || anyNA(required_events)) {
    stop("At least one valid event time is required for the dynamic Wald test.")
  }
  event_positions <- match(required_events, as.integer(dynamic$egt))
  if (anyNA(event_positions)) {
    stop(
      "Dynamic aggregation is missing required pre-treatment event time(s): ",
      paste(required_events[is.na(event_positions)], collapse = ", ")
    )
  }

  beta <- as.numeric(dynamic$att.egt[event_positions])
  influence <- as.matrix(
    dynamic$inf.function$dynamic.inf.func.e[, event_positions, drop = FALSE]
  )
  if (
    length(beta) != length(required_events) ||
      ncol(influence) != length(required_events) ||
      any(!is.finite(beta)) ||
      any(!is.finite(influence))
  ) {
    stop("Aggregated pre-treatment estimates or influence functions are invalid.")
  }

  n <- nrow(influence)
  params <- dynamic$DIDparams
  extra_cluster_vars <- params$clustervars[
    !(params$clustervars %in% c(params$idname, ""))
  ]
  covariance_scores <- influence
  if (length(extra_cluster_vars) > 0L) {
    cluster_vector <- as.character(params$cluster_vector)
    if (
      length(extra_cluster_vars) != 1L ||
        length(cluster_vector) != n ||
        anyNA(cluster_vector)
    ) {
      stop("Aggregated pre-trend test is missing aligned cluster metadata.")
    }
    covariance_scores <- rowsum(influence, cluster_vector, reorder = TRUE)
  }

  # The influence-function covariance of the aggregated event-study estimator
  # is crossprod(scores) / n^2.
  covariance <- crossprod(covariance_scores) / n^2
  covariance <- (covariance + t(covariance)) / 2
  eigen_decomposition <- eigen(covariance, symmetric = TRUE)
  eigen_scale <- max(abs(eigen_decomposition$values))
  eigen_tolerance <- if (is.finite(eigen_scale) && eigen_scale > 0) {
    length(beta) * sqrt(.Machine$double.eps) * eigen_scale
  } else {
    0
  }
  if (any(eigen_decomposition$values < -eigen_tolerance)) {
    stop("Aggregated pre-trend covariance matrix is not positive semidefinite.")
  }
  retained_eigenvalues <- eigen_decomposition$values > eigen_tolerance
  covariance_rank <- sum(retained_eigenvalues)

  if (covariance_rank == 0L) {
    statistic <- NA_real_
    p_value <- NA_real_
  } else {
    inverse_covariance <- eigen_decomposition$vectors[, retained_eigenvalues, drop = FALSE] %*%
      diag(1 / eigen_decomposition$values[retained_eigenvalues], covariance_rank) %*%
      t(eigen_decomposition$vectors[, retained_eigenvalues, drop = FALSE])
    statistic <- as.numeric(crossprod(beta, inverse_covariance %*% beta))
    p_value <- pchisq(statistic, covariance_rank, lower.tail = FALSE)
  }

  list(
    statistic = statistic,
    df = as.integer(covariance_rank),
    p_value = p_value,
    restrictions = length(required_events),
    rank_deficient = covariance_rank < length(required_events),
    covariance_rank = as.integer(covariance_rank),
    covariance_tolerance = eigen_tolerance,
    covariance = covariance,
    coefficients = tibble(
      event_time = required_events,
      estimate = beta,
      std_error = sqrt(pmax(diag(covariance), 0))
    )
  )
}

did_pretrend_tests <- function(
    mp,
    common_min_event = -4L,
    common_max_event = -1L,
    na_rm = FALSE
) {
  cell_event_times <- as.integer(mp$t - mp$group)
  finite_cells <- is.finite(mp$att) & is.finite(cell_event_times)
  if (!any(finite_cells)) {
    stop("No finite ATT(g,t) cells are available for dynamic aggregation.")
  }
  min_dynamic_event <- min(cell_event_times[finite_cells])
  max_dynamic_event <- max(cell_event_times[finite_cells])
  if (max_dynamic_event < 0L) {
    stop("Dynamic aggregation requires at least one post-treatment event time.")
  }
  dynamic <- did::aggte(
    mp,
    type = "dynamic",
    min_e = min_dynamic_event,
    max_e = max_dynamic_event,
    na.rm = na_rm,
    bstrap = FALSE,
    cband = FALSE,
    clustervars = mp$DIDparams$clustervars
  )
  available_event_times <- sort(unique(as.integer(dynamic$egt)))
  full_pre_events <- available_event_times[available_event_times < 0L]
  common_pre_events <- seq.int(common_min_event, common_max_event)
  if (length(full_pre_events) == 0L) {
    stop("Dynamic aggregation contains no pre-treatment event times.")
  }

  list(
    full = did_dynamic_event_wald_test(dynamic, full_pre_events),
    common = did_dynamic_event_wald_test(dynamic, common_pre_events),
    dynamic_event_times = available_event_times
  )
}

did_build_gminus1_mp <- function(
    data,
    outcome,
    controls,
    est_method = "dr",
    control_group = "notyettreated",
    base_period = "varying",
    min_treated_cohort = 2016L,
    alp = 0.05,
    bstrap = TRUE,
    biters = 1000L,
    cband = TRUE,
    min_cell_size = 5L,
    overlap_threshold = 0.995,
    cluster_var = NULL,
    eligible_filter = NULL,
    allow_missing_pretrend = FALSE
) {
  if (control_group != "notyettreated") {
    stop("The cohort-gminus1 implementation requires control_group = 'notyettreated'.")
  }
  if (base_period != "varying") {
    stop("The cohort-gminus1 implementation currently requires base_period = 'varying'.")
  }

  cluster_var <- unique(as.character(cluster_var))
  cluster_var <- cluster_var[!is.na(cluster_var) & nzchar(cluster_var)]
  cluster_var <- setdiff(cluster_var, "firm_id")
  if (length(cluster_var) > 1L) {
    stop("At most one cluster variable beyond firm_id is supported.")
  }

  required <- unique(c(
    "firm_id", "year", "cohort", outcome, controls, cluster_var
  ))
  missing <- setdiff(required, names(data))
  if (length(missing) > 0L) {
    stop("Missing variables for cohort-gminus1 DiD: ", paste(missing, collapse = ", "))
  }
  duplicates <- data |>
    count(firm_id, year) |>
    filter(n > 1L)
  if (nrow(duplicates) > 0L) {
    stop("Duplicate firm-year observations found in cohort-gminus1 DiD input.")
  }
  if (length(cluster_var) == 1L) {
    invalid_clusters <- data |>
      group_by(firm_id) |>
      summarise(
        n_clusters = n_distinct(.data[[cluster_var]], na.rm = TRUE),
        has_missing_cluster = any(is.na(.data[[cluster_var]])),
        .groups = "drop"
      ) |>
      filter(n_clusters != 1L | has_missing_cluster)
    if (nrow(invalid_clusters) > 0L) {
      stop(
        "Cluster variable `", cluster_var,
        "` must be non-missing and fixed within every firm."
      )
    }
  }

  cohorts <- sort(unique(data$cohort[data$cohort >= min_treated_cohort]))
  targets <- sort(unique(data$year))
  targets <- targets[targets > min(targets)]

  cells <- map(
    cohorts,
    function(g) {
      map(targets, ~ did_estimate_gminus1_cell(
        data = data,
        outcome = outcome,
        cohort_year = g,
        target_year = .x,
        est_method = est_method,
        controls = controls,
        min_cell_size = min_cell_size,
        overlap_threshold = overlap_threshold,
        eligible_filter = eligible_filter
      ))
    }
  ) |>
    list_flatten()

  contributor_ids <- sort(unique(unlist(map(cells, "contributor_ids"))))
  if (length(contributor_ids) == 0L) {
    stop("No cohort-time cells are estimable.")
  }
  n_global <- length(contributor_ids)

  att <- map_dbl(cells, "estimate")
  group <- map_dbl(cells, "cohort_year")
  target <- map_dbl(cells, "target_year")
  inf <- matrix(NA_real_, nrow = n_global, ncol = length(cells))
  for (j in seq_along(cells)) {
    ids <- cells[[j]]$contributor_ids
    local_inf <- cells[[j]]$local_inf
    if (length(ids) > 0L && length(local_inf) == length(ids)) {
      inf[match(ids, contributor_ids), j] <-
        (n_global / length(ids)) * local_inf
      inf[is.na(inf[, j]), j] <- 0
    }
  }

  finite_cells <- is.finite(att) &
    apply(inf, 2L, function(z) all(is.finite(z)))
  unacceptable <- !finite_cells &
    (!allow_missing_pretrend | target >= group)
  if (any(unacceptable)) {
    failed <- which(unacceptable)
    details <- vapply(failed, function(j) {
      x <- cells[[j]]
      paste0(
        "g=", x$cohort_year, ",t=", x$target_year,
        ",treated=", x$n_treated, ",controls=", x$n_controls,
        ": ", x$support_warning
      )
    }, character(1L))
    stop("Non-estimable ATT(g,t) cells: ", paste(details, collapse = " | "))
  }
  # Only firms that actually contribute as treated in a post-treatment cell
  # determine treated-cohort aggregation weights.  Other contributing firms
  # remain in the common influence-function universe as comparison units.
  treated_aggregation_ids <- map2(
    cells,
    group <= target,
    ~ if (.y) .x$treated_contributor_ids else integer()
  ) |>
    unlist() |>
    unique()

  firm_meta <- data |>
    select(firm_id, cohort, all_of(cluster_var)) |>
    distinct() |>
    filter(firm_id %in% contributor_ids) |>
    mutate(
      aggregation_cohort = if_else(
        firm_id %in% treated_aggregation_ids,
        cohort,
        0L
      )
    )

  aggregation_data <- firm_meta |>
    mutate(
      year = min(data$year),
      cohort = aggregation_cohort,
      .w = 1
    ) |>
    select(firm_id, year, cohort, all_of(cluster_var), .w) |>
    arrange(match(firm_id, contributor_ids))

  cluster_vector <- if (length(cluster_var) == 1L) {
    as.character(aggregation_data[[cluster_var]])
  } else {
    NULL
  }
  did_cluster_vars <- c("firm_id", cluster_var)

  # V_analytical stores the covariance of sqrt(n) * ATT.  When a market-level
  # cluster is supplied, aggregate the firm influence functions to cluster
  # sums before constructing that covariance.  This keeps the custom Wald
  # pre-tests consistent with the clustering used by aggte().
  covariance <- matrix(
    NA_real_,
    nrow = length(cells),
    ncol = length(cells)
  )
  if (any(finite_cells)) {
    finite_inf <- inf[, finite_cells, drop = FALSE]
    covariance_scores <- if (is.null(cluster_vector)) {
      finite_inf
    } else {
      rowsum(finite_inf, cluster_vector, reorder = TRUE)
    }
    covariance[finite_cells, finite_cells] <-
      crossprod(covariance_scores) / n_global
  }
  cell_standard_errors <- rep(NA_real_, length(cells))
  if (any(finite_cells)) {
    cell_standard_errors[finite_cells] <- sqrt(
      pmax(diag(covariance)[finite_cells] / n_global, 0)
    )
  }

  dp <- did::DIDparams(
    yname = outcome,
    tname = "year",
    idname = "firm_id",
    gname = "cohort",
    xformla = ~1,
    data = as.data.frame(aggregation_data),
    control_group = control_group,
    anticipation = 0,
    alp = alp,
    bstrap = bstrap,
    biters = as.integer(biters),
    clustervars = did_cluster_vars,
    cband = cband,
    print_details = FALSE,
    faster_mode = FALSE,
    pl = FALSE,
    cores = 1L,
    est_method = est_method,
    base_period = base_period,
    panel = FALSE,
    true_repeated_cross_sections = FALSE,
    n = n_global,
    nG = length(cohorts),
    nT = length(unique(data$year)),
    tlist = sort(unique(data$year)),
    glist = cohorts
  )
  dp$allow_unbalanced_panel <- TRUE
  dp$covariate_timing <- "treated cohort's calendar-year g-1"
  dp$cluster_vector <- cluster_vector
  dp$cluster_vector_var <- cluster_var

  mp <- did::MP(
    group = group,
    t = target,
    att = att,
    V_analytical = covariance,
    se = cell_standard_errors,
    c = qnorm(1 - alp / 2),
    inffunc = inf,
    n = n_global,
    W = NA_real_,
    Wpval = NA_real_,
    alp = alp,
    DIDparams = dp
  )
  pretrend_tests <- did_pretrend_tests(
    mp, na_rm = allow_missing_pretrend
  )
  full_pretrend <- pretrend_tests$full
  mp$W <- full_pretrend$statistic
  mp$Wpval <- full_pretrend$p_value

  cell_table <- map_dfr(seq_along(cells), function(cell_index) {
    x <- cells[[cell_index]]
    tibble(
      cohort_year = x$cohort_year,
      target_year = x$target_year,
      event_time = x$event_time,
      baseline_year = x$baseline_year,
      treated_covariate_year = x$baseline_year,
      control_covariate_year = x$baseline_year,
      latest_covariate_year = x$baseline_year,
      post_treatment_covariate_rows = 0L,
      outcome_base_year = x$outcome_base_year,
      eligible_treated = x$eligible_treated,
      eligible_controls = x$eligible_controls,
      n_treated = x$n_treated,
      n_controls = x$n_controls,
      treated_lost = x$treated_lost,
      controls_lost = x$controls_lost,
      estimate = x$estimate,
      std_error = cell_standard_errors[[cell_index]],
      estimable = is.finite(x$estimate) &
        is.finite(cell_standard_errors[[cell_index]]),
      min_propensity = x$min_propensity,
      max_propensity = x$max_propensity,
      min_treated_propensity = x$min_treated_propensity,
      max_treated_propensity = x$max_treated_propensity,
      min_control_propensity = x$min_control_propensity,
      max_control_propensity = x$max_control_propensity,
      max_control_odds_weight = x$max_control_odds_weight,
      support_status = x$support_status,
      support_warning = x$support_warning,
      propensity_model_note = x$propensity_model_note
    )
  })

  used_outcome_rows <- map_dfr(cells, function(x) {
    if (length(x$contributor_ids) == 0L) return(tibble())
    crossing(
      firm_id = x$contributor_ids,
      year = unique(c(x$outcome_base_year, x$target_year))
    )
  }) |>
    distinct(firm_id, year)
  used_covariate_rows <- map_dfr(cells, function(x) {
    if (length(x$contributor_ids) == 0L) return(tibble())
    tibble(firm_id = x$contributor_ids, year = x$baseline_year)
  }) |>
    distinct(firm_id, year)
  used_rows <- bind_rows(used_outcome_rows, used_covariate_rows) |>
    distinct(firm_id, year)

  list(
    mp = mp,
    cells = cell_table,
    contributor_ids = contributor_ids,
    used_rows = used_rows,
    used_outcome_rows = used_outcome_rows,
    used_covariate_rows = used_covariate_rows,
    n_obs = nrow(used_outcome_rows),
    n_covariate_rows = nrow(used_covariate_rows),
    n_any_used_rows = nrow(used_rows),
    n_firms = n_global,
    n_clusters = if (is.null(cluster_vector)) {
      n_global
    } else {
      n_distinct(cluster_vector)
    },
    cluster_var = if (length(cluster_var) == 0L) "firm_id" else cluster_var,
    firm_meta = firm_meta,
    full_pretrend = full_pretrend,
    common_pretrend = pretrend_tests$common
  )
}

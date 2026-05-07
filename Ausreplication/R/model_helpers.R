download_abs_workbook <- function(raw_dir, catalogue, file_name) {
  target_path <- file.path(raw_dir, file_name)

  if (!file.exists(target_path)) {
    message("Downloading ABS workbook: ", file_name)
    download_abs_data_cube(
      catalogue_string = catalogue,
      cube = file_name,
      path = raw_dir
    )
  }

  target_path
}


download_file_if_missing <- function(url, target_path) {
  if (!file.exists(target_path)) {
    message("Downloading file: ", basename(target_path))
    download.file(url = url, destfile = target_path, mode = "wb", quiet = TRUE)
  }

  target_path
}


read_abs_ts_workbook <- function(path, sheet = "Data1") {
  raw <- read_excel(path, sheet = sheet, col_names = FALSE, .name_repair = "minimal")

  if (ncol(raw) < 2L) {
    stop("Workbook ", basename(path), " appears to have no time-series columns.")
  }

  meta_rows <- list(
    item = 1L,
    unit = 2L,
    series_type = 3L,
    data_type = 4L,
    frequency = 5L,
    collection_month = 6L,
    series_start = 7L,
    series_end = 8L,
    no_obs = 9L,
    series_id = 10L
  )

  data_block <- raw[-seq_len(10L), , drop = FALSE]
  names(data_block) <- paste0("col_", seq_len(ncol(data_block)))

  out <- vector("list", length = ncol(data_block) - 1L)

  for (j in 2:ncol(raw)) {
    series_name <- as.character(raw[[j]][meta_rows$item])
    if (is.na(series_name) || !nzchar(series_name)) {
      next
    }

    tmp <- tibble(
      date_serial = suppressWarnings(as.numeric(data_block[[1]])),
      value = suppressWarnings(as.numeric(data_block[[j]]))
    ) %>%
      mutate(
        date = as.Date(date_serial, origin = "1899-12-30"),
        series_name = str_squish(series_name),
        unit = str_squish(as.character(raw[[j]][meta_rows$unit])),
        series_type = str_squish(as.character(raw[[j]][meta_rows$series_type])),
        data_type = str_squish(as.character(raw[[j]][meta_rows$data_type])),
        frequency = str_squish(as.character(raw[[j]][meta_rows$frequency])),
        collection_month = str_squish(as.character(raw[[j]][meta_rows$collection_month])),
        series_id = str_squish(as.character(raw[[j]][meta_rows$series_id]))
      ) %>%
      select(date, value, series_name, unit, series_type, data_type, frequency, collection_month, series_id) %>%
      filter(!is.na(date))

    out[[j - 1L]] <- tmp
  }

  bind_rows(out) %>%
    filter(!is.na(value))
}


pick_preferred_series <- function(data, pattern, preferred_types = c("Seasonally adjusted", "Seasonally Adjusted", "Trend", "Original")) {
  candidates <- data %>%
    filter(str_detect(series_name, pattern))

  if (nrow(candidates) == 0L) {
    stop("No series matched pattern: ", pattern)
  }

  for (type_name in preferred_types) {
    subset_data <- candidates %>% filter(series_type == type_name)
    if (nrow(subset_data) > 0L) {
      best_series_id <- subset_data %>%
        count(series_id, sort = TRUE) %>%
        slice(1L) %>%
        pull(series_id)

      return(
        subset_data %>%
          filter(series_id == best_series_id) %>%
          arrange(date) %>%
          distinct(date, .keep_all = TRUE) %>%
          select(date, value, series_name, series_type, unit, series_id)
      )
    }
  }

  best_series_id <- candidates %>%
    count(series_id, sort = TRUE) %>%
    slice(1L) %>%
    pull(series_id)

  candidates %>%
    filter(series_id == best_series_id) %>%
    arrange(date) %>%
    distinct(date, .keep_all = TRUE) %>%
    select(date, value, series_name, series_type, unit, series_id)
}


pick_series_by_id <- function(data, series_id) {
  out <- data %>%
    filter(.data$series_id == series_id) %>%
    arrange(date) %>%
    distinct(date, .keep_all = TRUE) %>%
    select(date, value, series_name, series_type, unit, series_id)

  if (nrow(out) == 0L) {
    stop("No series matched series_id: ", series_id)
  }

  out
}


safe_log <- function(x) {
  ifelse(is.na(x) | x <= 0, NA_real_, log(x))
}


rescale_to_millions <- function(x, unit) {
  unit_clean <- str_to_lower(str_squish(unit))

  dplyr::case_when(
    unit_clean %in% c("$ billions", "billions") ~ x * 1000,
    unit_clean %in% c("$ millions", "millions") ~ x,
    TRUE ~ x
  )
}


parse_quarter_label_date <- function(x) {
  x <- str_trim(x)
  month_map <- c(
    Mar = "03",
    Jun = "06",
    Sep = "09",
    Dec = "12"
  )

  month_abbr <- str_sub(x, 1L, 3L)
  year_part <- str_sub(x, -4L, -1L)
  month_num <- unname(month_map[month_abbr])

  as.Date(ifelse(is.na(month_num), NA_character_, paste0(year_part, "-", month_num, "-01")))
}


standardise <- function(x) {
  mu <- mean(x, na.rm = TRUE)
  sigma <- sd(x, na.rm = TRUE)
  if (is.na(sigma) || sigma == 0) {
    return(rep(NA_real_, length(x)))
  }
  (x - mu) / sigma
}


lead_lag_diff <- function(x, k = 1L) {
  x - dplyr::lag(x, k)
}


extract_state <- function(fit, model) {
  kfs <- KFS(model, filtering = "state", smoothing = "state")
  as.numeric(kfs$alphahat[, 1L])
}


write_section <- function(con, title) {
  writeLines(c("", title, strrep("-", nchar(title))), con = con)
}


read_legacy_house_price_series <- function(path) {
  read.csv(path, stringsAsFactors = FALSE) %>%
    transmute(
      date = parse_quarter_label_date(Date),
      house_price_old = as.numeric(HousePriceOld)
    ) %>%
    filter(!is.na(date), !is.na(house_price_old)) %>%
    arrange(date) %>%
    distinct(date, .keep_all = TRUE)
}


splice_house_price_series <- function(current_series, legacy_series) {
  overlap <- legacy_series %>%
    inner_join(current_series, by = "date")

  if (nrow(overlap) < 2L) {
    stop("Need at least two overlapping observations to splice the house price series.")
  }

  log_gap <- mean(log(overlap$house_price_index) - log(overlap$house_price_old), na.rm = TRUE)
  splice_start <- min(current_series$date, na.rm = TRUE)

  legacy_scaled <- legacy_series %>%
    mutate(house_price_spliced = house_price_old * exp(log_gap)) %>%
    filter(date < splice_start) %>%
    select(date, house_price_spliced)

  current_scaled <- current_series %>%
    transmute(date, house_price_spliced = house_price_index)

  bind_rows(legacy_scaled, current_scaled) %>%
    arrange(date) %>%
    distinct(date, .keep_all = TRUE)
}


run_adf_drift <- function(x, lags = 4L) {
  tryCatch(
    {
      if (!requireNamespace("urca", quietly = TRUE)) {
        return(tibble(
          adf_stat = NA_real_, adf_1pct = NA_real_,
          adf_5pct = NA_real_, adf_10pct = NA_real_
        ))
      }
      fit <- urca::ur.df(x, type = "drift", lags = lags)
      tau_name <- grep("^tau", colnames(fit@teststat), value = TRUE)[1]

      tibble(
        adf_stat = unname(fit@teststat[1, tau_name]),
        adf_1pct = unname(fit@cval[tau_name, "1pct"]),
        adf_5pct = unname(fit@cval[tau_name, "5pct"]),
        adf_10pct = unname(fit@cval[tau_name, "10pct"])
      )
    },
    error = function(e) {
      tibble(
        adf_stat = NA_real_,
        adf_1pct = NA_real_,
        adf_5pct = NA_real_,
        adf_10pct = NA_real_
      )
    }
  )
}


fit_long_run_spec <- function(data, spec_name, rhs_vars, response_var = "lcons_income_ratio") {
  required_vars <- c(response_var, rhs_vars)
  sample <- data %>%
    filter(complete.cases(across(all_of(required_vars))))

  formula <- reformulate(rhs_vars, response = response_var)
  fit <- lm(formula, data = sample)
  adf_info <- run_adf_drift(resid(fit), lags = 4L)
  fit_summary <- summary(fit)

  list(
    spec_name = spec_name,
    rhs_vars = rhs_vars,
    formula = formula,
    sample = sample,
    fit = fit,
    diagnostics = tibble(
      specification = spec_name,
      n_obs = nobs(fit),
      aic = AIC(fit),
      bic = BIC(fit),
      adj_r2 = fit_summary$adj.r.squared,
      sigma = fit_summary$sigma
    ) %>%
      bind_cols(adf_info) %>%
      mutate(adf_reject_5pct = adf_stat < adf_5pct)
  )
}


# Dynamic OLS (Stock & Watson 1993) long-run estimator.
#
# Augments the static cointegrating regression with leads and lags of
# first-differences of each I(1) RHS variable. This corrects for endogeneity
# bias that arises when there are multiple cointegrating vectors among the
# system variables (exactly the situation Johansen tests reveal here).
#
# The static-part residuals (y - intercept - beta_static * X) are the
# appropriate ECM correction term; they are placed in the returned list as
# `static_resid` for downstream use.
#
# Standard errors use Newey-West HAC (bandwidth = floor(4*(T/100)^(2/9))),
# consistent with the recommendation in Andrews (1991).
fit_dols_spec <- function(data, spec_name, rhs_vars, response_var = "lcons_income_ratio",
                          leads_lags = 2L) {
  required_vars <- c(response_var, rhs_vars)
  sample <- data %>%
    filter(complete.cases(across(all_of(required_vars))))

  n <- nrow(sample)

  # Build first-difference leads/lags columns for each RHS variable.
  aug_data <- sample
  aug_cols <- character(0)
  for (v in rhs_vars) {
    dx <- c(NA_real_, diff(sample[[v]]))
    for (j in seq(-leads_lags, leads_lags)) {
      # Use "lag" / "lead" prefix so names are valid R formula tokens (no hyphens).
      direction <- if (j < 0L) paste0("lag", abs(j)) else if (j > 0L) paste0("lead", j) else "l0"
      col_name <- paste0("d_", v, "_", direction)
      aug_cols <- c(aug_cols, col_name)
      if (j < 0L) {
        aug_data[[col_name]] <- c(rep(NA_real_, abs(j)), dx[seq_len(n - abs(j))])
      } else if (j > 0L) {
        aug_data[[col_name]] <- c(dx[(j + 1L):n], rep(NA_real_, j))
      } else {
        aug_data[[col_name]] <- dx
      }
    }
  }

  # Drop rows with any NA in augmentation columns.
  aug_sample <- aug_data %>%
    filter(complete.cases(across(all_of(aug_cols))))

  aug_formula <- reformulate(c(rhs_vars, aug_cols), response = response_var)
  aug_fit <- lm(aug_formula, data = aug_sample)

  # Newey-West bandwidth: Andrews (1991) rule of thumb.
  bw <- floor(4L * (nrow(aug_sample) / 100)^(2/9))
  hac_vcov <- tryCatch(
    sandwich::NeweyWest(aug_fit, lag = bw, prewhite = FALSE, adjust = TRUE),
    error = function(e) vcov(aug_fit)
  )

  # Static-part coefficients and their HAC standard errors.
  static_terms <- c("(Intercept)", rhs_vars)
  coef_all   <- coef(aug_fit)
  se_all     <- sqrt(diag(hac_vcov))
  static_coefs <- coef_all[static_terms]
  static_se    <- se_all[static_terms]

  # Static residuals: y - Xbeta (no augmentation terms).
  X_static <- model.matrix(reformulate(rhs_vars, response = response_var),
                            data = aug_sample)
  static_resid <- aug_sample[[response_var]] - X_static %*% static_coefs

  adf_info <- run_adf_drift(as.numeric(static_resid), lags = 4L)

  # Tidy coefficient table for output.
  dols_tidy <- tibble(
    term      = names(static_coefs),
    estimate  = unname(static_coefs),
    std.error = unname(static_se),
    statistic = estimate / std.error,
    p.value   = 2 * pnorm(-abs(statistic))
  )

  list(
    spec_name    = spec_name,
    rhs_vars     = rhs_vars,
    aug_fit      = aug_fit,
    aug_sample   = aug_sample,
    static_coefs = static_coefs,
    static_se    = static_se,
    static_resid = tibble(date = aug_sample$date, dols_resid = as.numeric(static_resid)),
    dols_tidy    = dols_tidy,
    diagnostics  = tibble(
      specification = spec_name,
      n_obs         = nrow(aug_sample),
      adj_r2        = summary(aug_fit)$adj.r.squared,
      sigma         = summary(aug_fit)$sigma
    ) %>%
      bind_cols(adf_info) %>%
      mutate(adf_reject_5pct = adf_stat < adf_5pct)
  )
}


build_credit_ssm <- function(y_matrix, log_h, log_q, lambda_2, lambda_3) {
  z_mat <- matrix(c(
    1, lambda_2, lambda_3
  ), nrow = 3L, ncol = 1L)

  h_diag <- diag(exp(log_h), nrow = 3L)
  q_mat <- matrix(exp(log_q), nrow = 1L, ncol = 1L)

  SSModel(
    y_matrix ~ -1 +
      SSMcustom(
        Z = z_mat,
        T = matrix(1),
        R = matrix(1),
        Q = q_mat,
        a1 = matrix(0),
        P1 = matrix(10)
      ),
    H = h_diag
  )
}


# ==============================================================================
# Kalman state-space CCI extractor
# ==============================================================================
#
# Estimates a single common latent factor across a set of credit indicators
# via maximum likelihood on the linear Gaussian state-space model:
#
#   y_t = Z * alpha_t + epsilon_t,   epsilon_t ~ N(0, H)
#   alpha_t = T * alpha_{t-1} + R * eta_t,   eta_t ~ N(0, Q)
#
# where alpha_t is the scalar latent CCI, Z is the n_series × 1 vector of
# loadings, H is the diagonal n_series × n_series indicator-noise covariance
# and Q is the scalar factor-innovation variance. T = 1 (random walk) and
# R = 1 by convention.
#
# Identification: the factor has scale + sign indeterminacy. We fix the
# loading on the first (anchor) indicator at +1, and orient the resulting
# smoothed factor such that its correlation with the anchor is positive.
#
# Indicators are passed via a tibble of `model_data` plus a vector of column
# names. Each is standardised (mean 0, sd 1) within the function. Missing
# observations (e.g. housing_loan_flow before 2002Q3) are passed through to
# the Kalman filter, which handles them by extending uncertainty during
# unobserved periods.
fit_kalman_cci <- function(model_data,
                            indicator_cols = c("log_housing_loan_flow",
                                                "log_debt_y",
                                                "fhb_share",
                                                "mortgage_burden",
                                                "mortgage_rate_spread"),
                            anchor_col = "log_housing_loan_flow",
                            initial_loadings = NULL,
                            verbose = TRUE) {
  if (!requireNamespace("KFAS", quietly = TRUE)) {
    stop("KFAS package required for fit_kalman_cci(); install with ",
         "install.packages('KFAS').")
  }
  # Local bindings so formulas inside SSModel() resolve via lexical scoping.
  # KFAS::SSMcustom and KFAS::SSModel inside a formula don't bind correctly;
  # bare names do.
  SSModel    <- KFAS::SSModel
  SSMcustom  <- KFAS::SSMcustom

  # Construct indicator series we can derive on the fly from master columns
  d <- model_data %>% arrange(date)
  if ("housing_loan_flow" %in% names(d) && !"log_housing_loan_flow" %in% names(d)) {
    d$log_housing_loan_flow <- log(pmax(d$housing_loan_flow, 1e-9))
  }
  if ("debt_y" %in% names(d) && !"log_debt_y" %in% names(d)) {
    d$log_debt_y <- log(pmax(d$debt_y, 1e-9))
  }
  if (!"mortgage_rate_spread" %in% names(d) && all(c("mortgage_rate") %in% names(d))) {
    # Use the change in mortgage rate as a credit-tightness proxy (price not
    # non-price; included as one component for a balanced indicator set).
    d$mortgage_rate_spread <- c(NA, diff(d$mortgage_rate))
  }

  available <- intersect(indicator_cols, names(d))
  if (length(available) < 2L) {
    stop("Need at least 2 available indicators; have: ",
         paste(available, collapse = ", "))
  }
  if (!anchor_col %in% available) {
    anchor_col <- available[1L]
    if (verbose) message("[fit_kalman_cci] anchor switched to ", anchor_col)
  }

  # Pull out the indicator matrix and standardise each column on its own
  # non-NA values (so missing periods do not bias the standardisation).
  Y_raw <- as.matrix(d[, available, drop = FALSE])
  Y_std <- apply(Y_raw, 2L, function(x) {
    if (sum(!is.na(x)) < 4L) return(rep(NA_real_, length(x)))
    (x - mean(x, na.rm = TRUE)) / stats::sd(x, na.rm = TRUE)
  })
  colnames(Y_std) <- available

  n_series <- ncol(Y_std)
  if (verbose) {
    message("[fit_kalman_cci] indicators (", n_series, "): ",
            paste(available, collapse = ", "))
    message("[fit_kalman_cci] anchor: ", anchor_col)
  }

  # Initial values for ML
  if (is.null(initial_loadings)) {
    initial_loadings <- rep(1, n_series)
    initial_loadings[match(anchor_col, available)] <- 1
  }
  init_log_h <- rep(log(0.5), n_series)
  init_log_q <- log(0.1)
  free_loading_idx <- setdiff(seq_len(n_series), match(anchor_col, available))

  pars0 <- c(init_log_h, init_log_q, initial_loadings[free_loading_idx])

  # Update function: modifies the components of the existing model in place
  # (as the KFAS API expects). Z is stored as a (p x m x 1) array.
  update_fn <- function(pars, model) {
    log_h_p <- pars[seq_len(n_series)]
    log_q_p <- pars[n_series + 1L]
    free_l  <- pars[(n_series + 2L):length(pars)]
    loadings <- numeric(n_series)
    loadings[match(anchor_col, available)] <- 1  # anchor fixed at +1
    loadings[free_loading_idx] <- free_l
    model$Z[, , 1L] <- loadings
    model$Q[1L, 1L, 1L] <- exp(log_q_p)
    diag(model$H[, , 1L]) <- exp(log_h_p)
    model
  }

  # Build initial fully-specified model directly (no template route — that
  # caused 'Misspecified Z' because the empty template had no SSMcustom).
  initial_loadings_all <- numeric(n_series)
  initial_loadings_all[match(anchor_col, available)] <- 1
  initial_loadings_all[free_loading_idx] <- initial_loadings[free_loading_idx]

  model0 <- SSModel(
    Y_std ~ -1 + SSMcustom(
      Z = matrix(initial_loadings_all, nrow = n_series, ncol = 1L),
      T = matrix(1),
      R = matrix(1),
      Q = matrix(exp(init_log_q), nrow = 1L, ncol = 1L),
      a1 = matrix(0),
      P1 = matrix(10)
    ),
    H = diag(exp(init_log_h), nrow = n_series)
  )

  fit <- tryCatch(
    KFAS::fitSSM(model0, inits = pars0, updatefn = update_fn,
                 method = "BFGS"),
    error = function(e) {
      message("[fit_kalman_cci] BFGS failed: ", conditionMessage(e),
              "; trying Nelder-Mead fallback")
      KFAS::fitSSM(model0, inits = pars0, updatefn = update_fn,
                   method = "Nelder-Mead")
    }
  )

  # Extract smoothed state
  kfs <- KFAS::KFS(fit$model, filtering = "state", smoothing = "state")
  cci_smoothed <- as.numeric(kfs$alphahat[, 1L])

  # Recover estimated loadings/variances
  pars_hat   <- fit$optim.out$par
  log_h_hat  <- pars_hat[seq_len(n_series)]
  log_q_hat  <- pars_hat[n_series + 1L]
  loadings_hat <- numeric(n_series)
  loadings_hat[match(anchor_col, available)] <- 1
  loadings_hat[free_loading_idx] <- pars_hat[(n_series + 2L):length(pars_hat)]
  names(loadings_hat) <- available

  # Final orientation: positive correlation with the anchor on its observed
  # subsample (already implied by anchor loading = +1, but cross-check).
  anc <- Y_std[, anchor_col]
  ok  <- !is.na(anc) & !is.na(cci_smoothed)
  if (sum(ok) > 4L) {
    rho <- stats::cor(anc[ok], cci_smoothed[ok])
    if (is.finite(rho) && rho < 0) {
      cci_smoothed <- -cci_smoothed
      loadings_hat <- -loadings_hat
    }
  }

  # Peak-normalise to unity for comparability with cci_williams
  mx <- max(abs(cci_smoothed), na.rm = TRUE)
  if (is.finite(mx) && mx > 0) cci_smoothed <- cci_smoothed / mx

  out <- tibble::tibble(date = d$date, cci_kalman = cci_smoothed)

  attr(out, "loadings") <- loadings_hat
  attr(out, "log_h")    <- setNames(log_h_hat, available)
  attr(out, "log_q")    <- log_q_hat
  attr(out, "convergence") <- fit$optim.out$convergence
  attr(out, "logLik")   <- if (!is.null(fit$model)) {
    -fit$optim.out$value
  } else NA_real_

  if (verbose) {
    message("[fit_kalman_cci] estimated loadings:")
    for (nm in names(loadings_hat)) {
      message(sprintf("  %-30s % .3f", nm, loadings_hat[nm]))
    }
    message(sprintf("[fit_kalman_cci] log-Q = %.3f, mean log-H = %.3f, convergence = %d",
                    log_q_hat, mean(log_h_hat), fit$optim.out$convergence))
  }
  out
}


build_credit_ssm_factor <- function(y_matrix, log_h, log_q, loadings) {
  n_series <- ncol(y_matrix)

  if (length(log_h) != n_series) {
    stop("log_h length must match number of indicator series.")
  }

  if (length(loadings) != n_series) {
    stop("loadings length must match number of indicator series.")
  }

  SSModel(
    y_matrix ~ -1 +
      SSMcustom(
        Z = matrix(loadings, nrow = n_series, ncol = 1L),
        T = matrix(1),
        R = matrix(1),
        Q = matrix(exp(log_q), nrow = 1L, ncol = 1L),
        a1 = matrix(0),
        P1 = matrix(10)
      ),
    H = diag(exp(log_h), nrow = n_series)
  )
}


build_credit_ssm_local_trend <- function(y_matrix, log_h, log_q_level, log_q_slope, loadings) {
  n_series <- ncol(y_matrix)

  if (length(log_h) != n_series) {
    stop("log_h length must match number of indicator series.")
  }

  if (length(loadings) != n_series) {
    stop("loadings length must match number of indicator series.")
  }

  z_array <- array(0, dim = c(n_series, 2L, 1L))
  z_array[, 1L, 1L] <- loadings

  SSModel(
    y_matrix ~ -1 +
      SSMcustom(
        Z = z_array,
        T = array(matrix(c(1, 1, 0, 1), nrow = 2L, byrow = TRUE), dim = c(2L, 2L, 1L)),
        R = array(diag(2L), dim = c(2L, 2L, 1L)),
        Q = array(diag(c(exp(log_q_level), exp(log_q_slope))), dim = c(2L, 2L, 1L)),
        a1 = matrix(c(0, 0), ncol = 1L),
        P1 = diag(c(10, 1), nrow = 2L)
      ),
    H = diag(exp(log_h), nrow = n_series)
  )
}


build_local_trend_ssm <- function(y, log_h, log_q_level, log_q_slope) {
  SSModel(
    y ~ SSMtrend(
      degree = 2L,
      Q = list(matrix(exp(log_q_level)), matrix(exp(log_q_slope)))
    ),
    H = matrix(exp(log_h))
  )
}


read_rba_xlsx_table <- function(path, sheet = "Data") {
  raw <- read_excel(path, sheet = sheet, col_names = FALSE, .name_repair = "minimal")

  if (ncol(raw) < 2L) {
    stop("RBA workbook ", basename(path), " appears to have no data columns.")
  }

  meta_rows <- list(
    title = 2L,
    description = 3L,
    frequency = 4L,
    series_type = 5L,
    unit = 6L,
    source = 9L,
    publication_date = 10L,
    series_id = 11L
  )

  data_block <- raw[-seq_len(11L), , drop = FALSE]
  names(data_block) <- paste0("col_", seq_len(ncol(data_block)))

  out <- vector("list", length = ncol(data_block) - 1L)

  for (j in 2:ncol(raw)) {
    title <- as.character(raw[[j]][meta_rows$title])
    if (is.na(title) || !nzchar(title)) {
      next
    }

    tmp <- tibble(
      date_serial = suppressWarnings(as.numeric(data_block[[1]])),
      value = suppressWarnings(as.numeric(data_block[[j]]))
    ) %>%
      mutate(
        date = as.Date(date_serial, origin = "1899-12-30"),
        series_name = str_squish(title),
        description = str_squish(as.character(raw[[j]][meta_rows$description])),
        frequency = str_squish(as.character(raw[[j]][meta_rows$frequency])),
        series_type = str_squish(as.character(raw[[j]][meta_rows$series_type])),
        unit = str_squish(as.character(raw[[j]][meta_rows$unit])),
        source = str_squish(as.character(raw[[j]][meta_rows$source])),
        publication_date = as.character(raw[[j]][meta_rows$publication_date]),
        series_id = str_squish(as.character(raw[[j]][meta_rows$series_id]))
      ) %>%
      select(date, value, series_name, description, frequency, series_type, unit, source, publication_date, series_id) %>%
      filter(!is.na(date))

    out[[j - 1L]] <- tmp
  }

  bind_rows(out) %>%
    filter(!is.na(value))
}


read_rba_csv_table <- function(path) {
  raw <- read.csv(path, skip = 1L, header = FALSE, fill = TRUE, check.names = FALSE, stringsAsFactors = FALSE)

  if (ncol(raw) < 2L || nrow(raw) < 9L) {
    stop("RBA CSV ", basename(path), " appears to have no data columns.")
  }

  meta_rows <- list(
    title = 1L,
    description = 2L,
    frequency = 3L,
    series_type = 4L,
    unit = 5L,
    source = 6L,
    publication_date = 7L,
    series_id = 8L
  )

  out <- vector("list", length = ncol(raw) - 1L)

  for (j in 2:ncol(raw)) {
    title <- raw[[j]][meta_rows$title]
    if (is.na(title) || !nzchar(title)) {
      next
    }

    tmp <- tibble(
      date = as.Date(raw[-seq_len(8L), 1], format = "%d-%b-%Y"),
      value = suppressWarnings(as.numeric(raw[-seq_len(8L), j])),
      series_name = str_squish(title),
      description = str_squish(raw[[j]][meta_rows$description]),
      frequency = str_squish(raw[[j]][meta_rows$frequency]),
      series_type = str_squish(raw[[j]][meta_rows$series_type]),
      unit = str_squish(raw[[j]][meta_rows$unit]),
      source = str_squish(raw[[j]][meta_rows$source]),
      publication_date = raw[[j]][meta_rows$publication_date],
      series_id = str_squish(raw[[j]][meta_rows$series_id])
    ) %>%
      filter(!is.na(date), !is.na(value))

    out[[j - 1L]] <- tmp
  }

  bind_rows(out)
}


to_quarter_start_date <- function(x) {
  as.Date(sprintf("%d-%02d-01", lubridate::year(x), lubridate::quarter(x) * 3L))
}


interpolate_annual_to_quarterly <- function(annual_data, quarter_dates, value_col) {
  annual_dates <- sort(unique(annual_data$date))
  quarter_frame <- tibble(date = sort(unique(as.Date(quarter_dates))))

  merged <- quarter_frame %>%
    left_join(
      annual_data %>% select(date, value = all_of(value_col)),
      by = "date"
    ) %>%
    arrange(date)

  merged %>%
    mutate(value = zoo::na.approx(value, x = date, na.rm = FALSE, rule = 2L)) %>%
    rename(!!value_col := value)
}


build_spline_credit_index <- function(indicator_data, indicator_cols, spline_df = 6L) {
  smooth_components <- lapply(indicator_cols, function(col_name) {
    valid <- which(!is.na(indicator_data[[col_name]]))
    out <- rep(NA_real_, nrow(indicator_data))

    if (length(valid) < 8L) {
      return(out)
    }

    df_use <- max(3L, min(spline_df, length(valid) - 1L))
    time_index <- seq_along(valid)
    fit <- lm(indicator_data[[col_name]][valid] ~ splines::ns(time_index, df = df_use))
    out[valid] <- fitted(fit)
    out
  })

  component_matrix <- do.call(cbind, smooth_components)
  standardise(rowMeans(component_matrix, na.rm = TRUE))
}


extract_signal <- function(fit, model) {
  kfs <- KFS(model, filtering = "signal", smoothing = "signal")
  as.numeric(kfs$muhat)
}


extract_trend_level <- function(fit, model) {
  kfs <- KFS(model, filtering = "state", smoothing = "state")
  as.numeric(kfs$alphahat[, 1L])
}


# Smoothed step dummy: 5-quarter MA of 4-quarter MA of a 0/1 step.
# Williams (2010) construction: SDMMA_s. Leading NAs (from MA windows that do
# not yet have enough history) are filled as 0, consistent with the step being
# 0 prior to its onset.
smoothed_step <- function(date_vec, knot_date) {
  raw <- as.integer(as.Date(date_vec) >= as.Date(knot_date))
  ma4 <- zoo::rollmean(raw, k = 4L, fill = NA, align = "right")
  ma5 <- zoo::rollmean(ma4, k = 5L, fill = NA, align = "right")
  ifelse(is.na(ma5), 0, ma5)
}


# Maximal-GETS Australian institutional CCI basis. Each column is a smoothed
# step dummy at one of the major Australian financial-policy turning points.
# `sign_priors` follow institutional history (deregulation/loosening = +1,
# retrenchment/tightening = −1). The Hendry-Krolzig drop-on-violation
# reduction in fit_consumption_with_williams_cci() prunes knots that violate
# their sign prior or are aliased.
#
# Switched from Williams' canonical 4-knot set (1979/1992/1998/2007) to this
# 15-knot maximal candidate set after the May 2026 knot experiment showed
# that on the 1988+ sample only one of Williams' four knots survives sign-
# prior reduction (2007Q1; 1979Q1 is aliased; 1992Q1 and 1998Q1 are sign-
# violators). The maximal set lets the surviving knots emerge from the data
# rather than from authorial choice; on our sample it identifies five
# surviving knots: 1992Q1 (banking distress), 2007Q3 (GFC), 2019Q1 (Hayne
# Royal Commission), 2020Q2 (COVID/JobKeeper), 2021Q4 (APRA buffer hike).
# See Ausreplication/docs/knot_experiment_findings.md for details.
#
# Williams' original 4-knot set is retained as a robustness benchmark via the
# explicit knots/sign_priors arguments.
build_williams_cci_basis <- function(dates,
                                      knots = c("1979-01-01", "1986-01-01",
                                                "1990-09-01", "1992-01-01",
                                                "1993-01-01", "1998-09-01",
                                                "2007-09-01", "2008-12-01",
                                                "2009-01-01", "2014-12-01",
                                                "2017-03-01", "2019-01-01",
                                                "2019-09-01", "2020-04-01",
                                                "2021-12-01"),
                                      sign_priors = c(1, 1, -1, -1, -1,
                                                       1, -1, -1, 1, -1,
                                                      -1, -1, 1, 1, -1)) {
  basis <- vapply(knots, function(k) smoothed_step(dates, k),
                  numeric(length(dates)))
  colnames(basis) <- paste0("sdmma_", gsub("-", "_", substr(knots, 1, 7)))
  attr(basis, "sign_priors") <- sign_priors
  attr(basis, "knots")       <- knots
  basis
}

# Williams (2010) original 4-knot set — kept as an explicit alternative for
# replication-of-published-results robustness columns.
build_williams_cci_basis_canonical <- function(dates) {
  build_williams_cci_basis(
    dates,
    knots       = c("1979-01-01", "1992-01-01", "1998-01-01", "2007-01-01"),
    sign_priors = c(1, -1, 1, -1)
  )
}


build_credit_regime_basis <- function(dates) {
  dates <- as.Date(dates)
  time_num <- as.numeric(dates)

  tibble(
    date = dates,
    cci_step_1983 = pmax(0, time_num - as.numeric(as.Date("1982-03-01"))) /
      (as.numeric(as.Date("1990-03-01")) - as.numeric(as.Date("1982-03-01"))),
    cci_step_1992 = pmax(0, time_num - as.numeric(as.Date("1991-09-01"))) /
      (as.numeric(as.Date("1994-03-01")) - as.numeric(as.Date("1991-09-01"))),
    cci_step_1998 = pmax(0, time_num - as.numeric(as.Date("1997-03-01"))) /
      (as.numeric(as.Date("2006-12-01")) - as.numeric(as.Date("1997-03-01"))),
    cci_step_2007 = pmax(0, time_num - as.numeric(as.Date("2007-03-01"))) /
      (as.numeric(as.Date("2009-12-01")) - as.numeric(as.Date("2007-03-01")))
  ) %>%
    mutate(
      cci_step_1983 = pmin(cci_step_1983, 1),
      cci_step_1992 = pmin(cci_step_1992, 1),
      cci_step_1998 = pmin(cci_step_1998, 1),
      cci_step_2007 = pmin(cci_step_2007, 1)
    )
}


construct_institutional_cci <- function(data, basis_data) {
  work <- data %>%
    select(date, housing_loan_flow, house_price_index, debt_income_ratio, first_home_buyer_share, real_mortgage_rate) %>%
    left_join(basis_data, by = "date") %>%
    mutate(
      loan_flow_z = standardise(safe_log(housing_loan_flow)),
      house_price_growth_z = standardise(lead_lag_diff(safe_log(house_price_index), 4L)),
      leverage_z = standardise(-debt_income_ratio),
      first_home_buyer_share_z = standardise(first_home_buyer_share),
      mortgage_rate_headwind_z = standardise(-real_mortgage_rate)
    )

  regime_component <- with(
    work,
    0.9 * cci_step_1983 -
      0.6 * cci_step_1992 +
      0.8 * cci_step_1998 -
      0.9 * cci_step_2007
  )

  indicator_component <- rowMeans(
    cbind(
      work$loan_flow_z,
      work$house_price_growth_z,
      work$leverage_z,
      work$first_home_buyer_share_z,
      work$mortgage_rate_headwind_z
    ),
    na.rm = TRUE
  )

  out <- work %>%
    transmute(
      date,
      cci_regime_component = standardise(regime_component),
      cci_indicator_component = standardise(indicator_component),
      cci_institutional_raw = 0.65 * standardise(regime_component) + 0.35 * standardise(indicator_component)
    )

  out %>%
    mutate(
      cci_regime_component = standardise(cci_regime_component),
      cci_indicator_component = standardise(cci_indicator_component),
      cci_institutional_raw = standardise(cci_institutional_raw)
    )
}


orient_credit_index <- function(data, cci_col, easier_credit_targets = c("housing_loan_flow", "house_price_index"), tighter_credit_targets = c("real_mortgage_rate")) {
  cci <- data[[cci_col]]

  easier_score <- sum(vapply(easier_credit_targets, function(col_name) {
    suppressWarnings(cor(cci, data[[col_name]], use = "pairwise.complete.obs"))
  }, numeric(1L)), na.rm = TRUE)

  tighter_score <- sum(vapply(tighter_credit_targets, function(col_name) {
    -suppressWarnings(cor(cci, data[[col_name]], use = "pairwise.complete.obs"))
  }, numeric(1L)), na.rm = TRUE)

  orientation <- if ((easier_score + tighter_score) < 0) -1 else 1

  tibble(
    cci = standardise(orientation * cci),
    orientation = orientation,
    easier_score = easier_score,
    tighter_score = tighter_score
  )
}


adaptive_permanent_income_log <- function(log_income, lambda = 0.97) {
  out <- rep(NA_real_, length(log_income))
  valid_index <- which(!is.na(log_income))

  if (length(valid_index) == 0L) {
    return(out)
  }

  first_idx <- valid_index[1]
  out[first_idx] <- log_income[first_idx]

  if (first_idx < length(log_income)) {
    for (i in seq.int(first_idx + 1L, length(log_income))) {
      if (is.na(log_income[i])) {
        out[i] <- out[i - 1L]
      } else {
        out[i] <- lambda * out[i - 1L] + (1 - lambda) * log_income[i]
      }
    }
  }

  out
}


fit_income_expectations_model <- function(data, income_col = "lgdi", unemployment_col = "unemployment_rate", real_rate_col = "real_mortgage_rate") {
  work <- tibble(
    income = data[[income_col]],
    unemployment_rate = data[[unemployment_col]],
    real_mortgage_rate = data[[real_rate_col]]
  ) %>%
    mutate(
      d_income = lead_lag_diff(income, 1L),
      d_income_lag1 = lag(d_income, 1L),
      d_income_lag2 = lag(d_income, 2L),
      d_u = lead_lag_diff(unemployment_rate, 1L),
      real_rate_lag1 = lag(real_mortgage_rate, 1L)
    )

  fit_sample <- work %>% filter(complete.cases(.))

  if (nrow(fit_sample) < 20L) {
    stop("Insufficient observations to fit the income expectations model.")
  }

  lm(
    d_income ~ d_income_lag1 + d_income_lag2 + d_u + real_rate_lag1,
    data = fit_sample
  )
}


compute_expected_log_income_path <- function(data, fit, horizon = 40L, income_col = "lgdi", unemployment_col = "unemployment_rate", real_rate_col = "real_mortgage_rate") {
  n <- nrow(data)
  log_income <- data[[income_col]]
  unemployment_rate <- data[[unemployment_col]]
  real_mortgage_rate <- data[[real_rate_col]]

  d_income_hist <- c(NA_real_, diff(log_income))
  d_u_hist <- c(NA_real_, diff(unemployment_rate))
  coef_vec <- coef(fit)

  expected_log_income <- rep(NA_real_, n)

  for (i in seq_len(n)) {
    if (is.na(log_income[i])) {
      next
    }

    level_now <- log_income[i]
    g1 <- if (i >= 1L) d_income_hist[i] else NA_real_
    g2 <- if (i >= 2L) d_income_hist[i - 1L] else NA_real_
    du_assumption <- if (i >= 1L) d_u_hist[i] else NA_real_
    rate_assumption <- real_mortgage_rate[i]

    if (is.na(g1)) {
      g1 <- 0
    }
    if (is.na(g2)) {
      g2 <- g1
    }
    if (is.na(du_assumption)) {
      du_assumption <- 0
    }
    if (is.na(rate_assumption)) {
      rate_assumption <- 0
    }

    future_levels <- rep(NA_real_, horizon)
    prev1 <- g1
    prev2 <- g2
    current_level <- level_now

    for (h in seq_len(horizon)) {
      growth_forecast <- coef_vec[["(Intercept)"]] +
        coef_vec[["d_income_lag1"]] * prev1 +
        coef_vec[["d_income_lag2"]] * prev2 +
        coef_vec[["d_u"]] * du_assumption +
        coef_vec[["real_rate_lag1"]] * rate_assumption

      current_level <- current_level + growth_forecast
      future_levels[h] <- current_level
      prev2 <- prev1
      prev1 <- growth_forecast
    }

    expected_log_income[i] <- mean(future_levels, na.rm = TRUE)
  }

  expected_log_income
}


compute_log_yp_over_y <- function(log_income, expected_log_income,
                                   discount = 0.05, horizon = 40L,
                                   weights = NULL, denom = NULL) {
  if (length(expected_log_income) == 0L) {
    return(numeric(0))
  }

  if (is.null(denom)) denom <- log_income

  is_matrixlike <- is.matrix(expected_log_income) ||
                   is.data.frame(expected_log_income)

  if (is_matrixlike) {
    M <- as.matrix(expected_log_income)
    if (is.null(weights)) {
      h <- if (!is.null(horizon)) as.integer(horizon) else ncol(M)
      if (ncol(M) != h) {
        stop(sprintf(
          "[compute_log_yp_over_y] expected_log_income has %d columns but horizon = %d.",
          ncol(M), h
        ))
      }
      w <- (1 - discount) ^ (seq_len(h) - 1L)
      w <- w / sum(w)
    } else {
      w <- weights
      if (length(w) != ncol(M)) {
        stop(sprintf(
          "[compute_log_yp_over_y] weights length (%d) != expected_log_income ncol (%d).",
          length(w), ncol(M)
        ))
      }
      if (abs(sum(w) - 1) > 1e-8) w <- w / sum(w)
    }
    return(as.numeric(M %*% w) - denom)
  }

  expected_log_income - denom
}


compute_expected_log_income_path_rolling <- function(data, horizon = 40L, min_obs = 40L, income_col = "lgdi", unemployment_col = "unemployment_rate", real_rate_col = "real_mortgage_rate") {
  n <- nrow(data)
  out <- rep(NA_real_, n)

  for (i in seq_len(n)) {
    if (i <= min_obs) {
      next
    }

    train <- data[seq_len(i - 1L), , drop = FALSE]

    fit <- tryCatch(
      fit_income_expectations_model(
        train,
        income_col = income_col,
        unemployment_col = unemployment_col,
        real_rate_col = real_rate_col
      ),
      error = function(e) NULL
    )

    if (is.null(fit)) {
      next
    }

    path <- compute_expected_log_income_path(
      data = data[i, , drop = FALSE],
      fit = fit,
      horizon = horizon,
      income_col = income_col,
      unemployment_col = unemployment_col,
      real_rate_col = real_rate_col
    )

    out[i] <- path[[1]]
  }

  out
}

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
  fit <- ur.df(x, type = "drift", lags = lags)
  tau_name <- grep("^tau", colnames(fit@teststat), value = TRUE)[1]

  tibble(
    adf_stat = unname(fit@teststat[1, tau_name]),
    adf_1pct = unname(fit@cval[tau_name, "1pct"]),
    adf_5pct = unname(fit@cval[tau_name, "5pct"]),
    adf_10pct = unname(fit@cval[tau_name, "10pct"])
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


build_local_trend_ssm <- function(y, log_h, log_q_level, log_q_slope) {
  SSModel(
    y ~ SSMtrend(
      degree = 2L,
      Q = list(matrix(exp(log_q_level)), matrix(exp(log_q_slope)))
    ),
    H = matrix(exp(log_h))
  )
}


extract_signal <- function(fit, model) {
  kfs <- KFS(model, filtering = "signal", smoothing = "signal")
  as.numeric(kfs$muhat)
}


extract_trend_level <- function(fit, model) {
  kfs <- KFS(model, filtering = "state", smoothing = "state")
  as.numeric(kfs$alphahat[, 1L])
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

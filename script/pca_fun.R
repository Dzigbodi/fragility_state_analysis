#' Run PCA and return eigenvalues, variable loadings/contributions, and individual scores
#'
#' @description
#' **Edict:** Compute a Principal Component Analysis (PCA) on a numeric dataset
#' and produce three tidy outputs:
#' 1) **eig** — percentage of variance explained by PC1 and PC2 for the given `year`;
#' 2) **var** — loadings (coordinates) and contributions for variables on PC1/PC2;
#' 3) **ind** — individual/component scores, normalized scores (via `translateFunc()`),
#'    and contributions, with country names and year attached.
#'
#' Designed for streamlined reporting and plotting (e.g., correlation circle and biplots).
#'
#' @param x A numeric data frame / matrix (observations in rows, variables in columns).
#'          Row names should identify units (e.g., country names).
#' @param year A scalar label (numeric or character) attached to all outputs.
#' @param ncp Number of principal components to compute (default: 2). Must be >= 2
#'            for this function since it extracts PC1 and PC2.
#'
#' @return
#' A list with three tibbles:
#' \itemize{
#'   \item `eig`: columns `Year`, `eig1`, `eig2` (variance explained in \% for PC1/PC2).
#'   \item `var`: columns `PCAVAR`, `Factor1`, `Factor2`, `CFactor1`, `CFactor2`, `Year`.
#'   \item `ind`: original columns of `x` plus `Country.Territory`, `Year`,
#'                `Factor1`, `Factor2`, normalized scores `NFactor1`, `NFactor2`,
#'                and contributions `CFactor1`, `CFactor2`.
#' }
#'
#' @details
#' - Uses `FactoMineR::PCA()` with `graph = FALSE`.  
#' - `translateFunc()` must be available in the environment; it rescales PC scores
#'   to \([-2.5, 2.5]\) for easier visualization/aggregation.
#' - Input `x` should be entirely numeric (non-numeric columns should be removed before calling).
#' - Row names of `x` are carried to `Country.Territory` in the `ind` output.
#'
#' @importFrom tibble as_tibble tibble
#' @importFrom dplyr bind_cols mutate relocate
#' @examples
#' \dontrun{
#'   library(FactoMineR)
#'   # x: numeric data with rownames as country names
#'   out <- PCA_function(x, year = 2024, ncp = 3)
#'   out$eig
#'   out$var
#'   out$ind
#' }
#' @export
PCA_function <- function(x, year, ncp = 2) {
  
  # ---- Basic validation -------------------------------------------------------
  if (!is.data.frame(x) && !is.matrix(x)) {
    stop("`x` must be a data.frame or matrix of numeric variables.")
  }
  if (!is.numeric(as.matrix(x))) {
    stop("`x` must be numeric. Remove/convert non-numeric columns before calling.")
  }
  if (length(ncp) != 1 || !is.numeric(ncp) || ncp < 2) {
    stop("`ncp` must be a numeric scalar >= 2 (this function extracts PC1 and PC2).")
  }
  if (ncp > min(nrow(x), ncol(x))) {
    warning("`ncp` exceeds data rank; reducing to min(nrow(x), ncol(x)).")
    ncp <- min(nrow(x), ncol(x))
  }
  if (is.null(rownames(x))) {
    warning("`x` has no row names; `Country.Territory` will be generic indices.")
    rownames(x) <- as.character(seq_len(nrow(x)))
  }
  
  # ---- PCA --------------------------------------------------------------------
  # Uses FactoMineR::PCA; graph = FALSE to avoid interactive plots
  res <- FactoMineR::PCA(x, ncp = ncp, graph = FALSE)
  
  # ---- Variables: coordinates (loadings) and contributions --------------------
  varcoord <- res$var$coord[, 1:2, drop = FALSE]
  colnames(varcoord) <- c("Factor1", "Factor2")
  
  varctr  <- res$var$contrib[, 1:2, drop = FALSE]
  colnames(varctr) <- c("CFactor1", "CFactor2")
  
  # ---- Eigenvalues (percentage of variance explained) -------------------------
  # res$eig[,2] holds % of variance for each component
  eig_tbl <- tibble::tibble(
    Year = year,
    eig1 = res$eig[, 2][1],
    eig2 = res$eig[, 2][2]
  )
  
  # ---- Assemble 'var' table ---------------------------------------------------
  # Keep variable names in a column PCAVAR, add Year
  var_tbl <- dplyr::bind_cols(varcoord, varctr) |>
    dplyr::mutate(Year = year, PCAVAR = rownames(varctr)) |>
    dplyr::relocate(PCAVAR)
  
  # ---- Individuals: scores, normalized scores, contributions ------------------
  # PC scores (coordinates) and contributions for first two components
  ind_scores <- res$ind$coord[, 1:2, drop = FALSE]
  ind_contrib <- res$ind$contrib[, 1:2, drop = FALSE]
  
  # Build tibble: original x + identifiers + scores + normalized scores + contribs
  ind_tbl <- x |>
    tibble::as_tibble() |>
    dplyr::mutate(
      `Country.Territory` = rownames(x),
      Year    = year,
      Factor1 = as.vector(ind_scores[, 1]),
      Factor2 = as.vector(ind_scores[, 2]),
      NFactor1 = translateFunc(Factor1),  # requires translateFunc() in env
      NFactor2 = translateFunc(Factor2),
      CFactor1 = as.vector(ind_contrib[, 1]),
      CFactor2 = as.vector(ind_contrib[, 2])
    ) |>
    dplyr::relocate(`Country.Territory`, Year)
  
  # ---- Return -----------------------------------------------------------------
  obj <- list(
    eig = eig_tbl,
    var = var_tbl,
    ind = ind_tbl
  )
  return(obj)
}



#' Pooled PCA over a range of years (wrapper around `PCA_function`)
#'
#' @description
#' **Edict:** For each year in `Yearrange`, subset the data, run PCA on the
#' selected variables, and stack the results. Returns three tidy tables pooled
#' over time:
#' 1) `var` — variable loadings (Factor1/2) and contributions (CFactor1/2),
#' 2) `ind` — individual scores (Factor1/2), normalized scores (NFactor1/2),
#'    and contributions (CFactor1/2) with `countryvar` and `Year`,
#' 3) `eig` — % variance explained (eig1/eig2) by PC1/PC2 for each year.
#'
#' Uses your `PCA_function()` under the hood (which calls `FactoMineR::PCA`).
#'
#' @param data A data frame containing at least:
#'   - a `Year` column,
#'   - an identifier column named by `countryvar` (default `"Country.Territory"`),
#'   - the numeric PCA variables listed in `pcavar`.
#' @param Yearrange A vector of years to iterate over (e.g., `2000:2024`).
#' @param pcavar Character vector of column names to include in the PCA.
#' @param countryvar Name of the identifier column containing unit names
#'   (default: `"Country.Territory"`). Will be used as row names before PCA.
#'
#' @return A list with three tibbles:
#' \itemize{
#'   \item `var`: pooled variable loadings/contributions with `Year` and `PCAVAR`,
#'   \item `ind`: pooled individual results with `countryvar`, `Year`, scores, etc.,
#'   \item `eig`: pooled eigenvalue (% variance explained) table by `Year`.
#' }
#'
#' @details
#' - Rows for which any of the `pcavar` are missing are dropped prior to PCA using
#'   `remove_missing()` (must exist in your environment). If unavailable, replace with
#'   `tidyr::drop_na(all_of(pcavar))`.
#' - A year is **skipped** (with a warning) if after dropping missing values the matrix
#'   has fewer than 2 rows or fewer than 2 columns.
#' - `PCA_function()` is expected to return a list with `ind`, `var`, and `eig`.
#'
#' @examples
#' \dontrun{
#'   # Suppose `df` has columns: Year, Country.Territory, X1, X2, X3
#'   out <- PCA_pooled_func(
#'     data = df,
#'     Yearrange = 2015:2024,
#'     pcavar = c("X1", "X2", "X3"),
#'     countryvar = "Country.Territory"
#'   )
#'   out$eig
#'   out$var
#'   out$ind
#' }
#'
#' @importFrom dplyr filter select bind_rows all_of
#' @export
PCA_pooled_func <- function(data, Yearrange, pcavar, countryvar = "Country.Territory") {
  ind_df <- NULL
  var_df <- NULL
  eig_df <- NULL
  
  # ---- Basic validation -------------------------------------------------------
  if (!is.data.frame(data)) stop("`data` must be a data.frame.")
  if (!("Year" %in% names(data))) stop("`data` must contain a `Year` column.")
  if (!(countryvar %in% names(data))) stop("`data` must contain the `countryvar` column: ", countryvar)
  if (!all(pcavar %in% names(data))) {
    missing_cols <- setdiff(pcavar, names(data))
    stop("Missing PCA variables in `data`: ", paste(missing_cols, collapse = ", "))
  }
  
  # Iterate over each requested year
  for (year in Yearrange) {
    # Subset to a single year and ensure a plain data.frame
    temp <- data |>
      dplyr::filter(Year == year) |>
      as.data.frame()
    
    # If no rows for this year, skip
    if (nrow(temp) == 0) {
      warning(sprintf("Year %s has no rows; skipping.", year))
      next
    }
    
    # Use the country identifier as row names
    row.names(temp) <- unlist(temp |>
                                dplyr::select(dplyr::all_of(countryvar)))
    
    # Select PCA variables and drop rows with missing values in any of them.
    # NOTE: `remove_missing()` must exist; otherwise replace with tidyr::drop_na():
    # mat <- temp |> dplyr::select(dplyr::all_of(pcavar)) |> tidyr::drop_na() |> as.matrix()
    mat <- temp |>
      dplyr::select(dplyr::all_of(pcavar)) |>
      remove_missing() |>
      as.matrix()
    
    # Check matrix dimensions (need at least 2 obs and 2 vars for 2 PCs)
    if (nrow(mat) < 2 || ncol(mat) < 2) {
      warning(sprintf("Year %s: insufficient data after dropping NAs (rows=%d, cols=%d); skipping.",
                      year, nrow(mat), ncol(mat)))
      next
    }
    
    # ---- Run PCA for this year ------------------------------------------------
    res <- PCA_function(x = mat, year = year, ncp = 2)
    
    # ---- Pool results ---------------------------------------------------------
    ind_df <- dplyr::bind_rows(ind_df, res[["ind"]])
    var_df <- dplyr::bind_rows(var_df, res[["var"]])
    eig_df <- dplyr::bind_rows(eig_df, res[["eig"]])
  }
  
  # Final list
  result <- list(
    var = var_df,
    ind = ind_df,
    eig = eig_df
  )
  return(result)
}




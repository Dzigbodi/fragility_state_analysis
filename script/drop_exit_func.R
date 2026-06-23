
#' Forward-fill missing indicators conditional on `below_start_p`
#'
#' @description
#' Completes a country-year row block by **forward-filling** the indicator
#' variables (`below_start_p`, `above_exit_p`, `below_exit_p`) *only when*
#' `below_start_p` is `NA` at time *t* **and** non-`NA` at time *t-1*. In that
#' case, the function copies all three indicators from *t-1* to *t*.  
#' This preserves your intended behavior: the existence of a prior, known
#' `below_start_p` acts as the gate to borrow last period’s full indicator state.
#'
#' @param year Integer/numeric vector of years (assumed sorted ascending and
#'   contiguous by the caller).
#' @param below_start_p,above_exit_p,below_exit_p Numeric/logical 0/1 vectors of
#'   equal length with possible `NA`s.
#'
#' @return A data frame with columns
#'   `year`, `below_start_p`, `above_exit_p`, `below_exit_p`, where some `NA`s
#'   may have been forward-filled according to the rule above.
#'
#' @details
#' - Only rows with `is.na(below_start_p[t]) && !is.na(below_start_p[t-1])`
#'   are filled. All three indicators at *t* are set to their values at *t-1*.
#' - The first row is never modified (no t-1).
#' - No general forward-fill is applied; the rule is **conditional** on
#'   `below_start_p` being the trigger.
#'
#' @examples
#' # fill_missing(year, below_start_p, above_exit_p, below_exit_p)
#'
#' @export
fill_missing <- function(year,
                         below_start_p,
                         above_exit_p,
                         below_exit_p) {
  # Iterate from the second observation onward
  for (i in 2:length(year)) {
    # If the current below_start_p is missing but the previous is known,
    # copy the entire indicator state from t-1 to t.
    if (is.na(below_start_p[i]) & !is.na(below_start_p[i - 1])) {
      below_start_p[i] <- below_start_p[i - 1]
      below_exit_p[i]  <- below_exit_p[i - 1]
      above_exit_p[i]  <- above_exit_p[i - 1]
    }
  }
  
  # Return as a data.frame (kept to match your downstream usage)
  res <- data.frame(
    year          = year,
    below_start_p = below_start_p,
    above_exit_p  = above_exit_p,
    below_exit_p  = below_exit_p
  )
  return(res)
}




#' Detect "exit" and "relapse" episodes by country over time
#'
#' @description
#' Scans country-year indicator series to detect three types of episodes:
#' - **Fast Exit** ("Breakthrough"): exactly `TIME_THRESHOLD` years **below_start_p == 1**
#'   immediately followed by `TIME_THRESHOLD` years **above_exit_p == 1**.
#' - **Gradual Exit** ("Soft Breakthrough"): `TIME_THRESHOLD` years **below_start_p == 1**
#'   followed (not necessarily immediately) at some later point by `TIME_THRESHOLD` years
#'   **above_exit_p == 1** (only counted if the country does **not** already have a Fast Exit).
#' - **Relapse** ("Drop"): `TIME_THRESHOLD` years **above_exit_p == 1** followed later by
#'   `TIME_THRESHOLD` years **below_start_p == 1**.
#'
#' Missing years in each country's panel are completed to a full sequence and
#' then filled using `fill_missing()` (assumed available in your environment),
#' so the detection logic always runs on a contiguous, year-by-year series.
#'
#' @param df A data frame/tibble with columns:
#'   - `Country` (factor/character),
#'   - `year` (integer/numeric),
#'   - `below_start_p`, `above_exit_p`, `below_exit_p` (binary 0/1 indicators).
#'     Only `below_start_p` and `above_exit_p` are used in the episode definitions here.
#' @param TIME_THRESHOLD Integer window length (e.g., 2, 3, …) used for run detection
#'   (consecutive years required to flag an episode).
#'
#' @return A tibble with columns:
#'   - `Year`: the first year of the detected **exit** or **relapse** window described in `Type`,
#'   - `Type`: one of `"Fast Exit"`, `"Gradual Exit"`, or `"Relapse"`,
#'   - `Country`: country identifier.
#'
#' @details
#' Logic is evaluated country-by-country on completed/filled yearly series.
#' - **Fast Exit**: detect a block `[i, i+T-1]` where `below_start_p == 1`, immediately followed by
#'   `[i+T, i+2T-1]` where `above_exit_p == 1`. The event is recorded at year `year[i+T]`.
#' - **Gradual Exit**: after a `[i, i+T-1]` block of `below_start_p == 1`, search forward for *any*
#'   later contiguous `[j, j+T-1]` block with `above_exit_p == 1`. If the country does not already
#'   have a Fast Exit, record at `year[j]`.
#' - **Relapse**: after a `[i, i+T-1]` block of `above_exit_p == 1`, search forward for *any* later
#'   contiguous `[j, j+T-1]` block with `below_start_p == 1`. Record at `year[j]`.
#'
#' If a country has fewer than `T-1` observations, Fast Exit cannot occur; with fewer than `T`
#' observations, nothing is detectable and the country is skipped.
#'
#' @examples
#' # drop_exit_func(my_panel_df, TIME_THRESHOLD = 3)
#'
#' @export
drop_exit_func <- function(df, TIME_THRESHOLD) {
  # ---- Basic validation -------------------------------------------------------
  stopifnot(is.data.frame(df))
  if (!all(c("Country","year","below_start_p","above_exit_p","below_exit_p") %in% names(df))) {
    stop("`df` must include columns: Country, year, below_start_p, above_exit_p, below_exit_p.")
  }
  if (!is.numeric(TIME_THRESHOLD) || length(TIME_THRESHOLD) != 1 || TIME_THRESHOLD < 1) {
    stop("`TIME_THRESHOLD` must be a single positive integer.")
  }
  
  # ---- Containers to store results -------------------------------------------
  breakthrough_list <- c()      # Countries with a Fast Exit (to avoid double-tagging as Gradual)
  soft_breakthrough_list <- c() # Countries with a Gradual Exit
  drop_list <- c()              # Countries with a Relapse
  
  breakthrough_df <- NULL
  soft_breakthrough_df <- NULL
  drop_df <- NULL
  
  # Get unique countries
  country_list <- df |>
    dplyr::ungroup() |>
    dplyr::distinct(Country) |>
    dplyr::pull(Country)
  
  # ============================================================================
  # PASS 1: Detect FAST EXIT (Below for T, then immediately Above for T)
  # ============================================================================
  for (country_name in country_list) {
    
    # Subset this country's data and keep required columns
    df_country <- df |>
      dplyr::ungroup() |>
      dplyr::filter(Country == country_name) |>
      dplyr::select(year, below_start_p, above_exit_p, below_exit_p)
    
    # Complete yearly sequence and fill missing indicators
    df_country <- df_country |>
      dplyr::reframe(year = seq(from = min(year), to = max(year), by = 1)) |>
      dplyr::left_join(df_country, by = "year") |>
      dplyr::arrange(year) |>
      dplyr::reframe(fill_missing(year, below_start_p, above_exit_p, below_exit_p))
    
    # Extract vectors for speed/convenience
    year          <- df_country |> dplyr::pull(year)
    below_start_p <- df_country |> dplyr::pull(below_start_p)
    above_exit_p  <- df_country |> dplyr::pull(above_exit_p)
    # below_exit_p is not used in the current episode definitions
    N <- length(year)
    
    # Need at least T-1 observations to detect an immediate  T run
    if(N< TIME_THRESHOLD - 1) next
    
    # Sliding window starts: i .. i+T-1 must be valid within N
    seq_th <- seq_len(N - TIME_THRESHOLD + 1)
    
    for (i in seq_th) {
      # Check T consecutive years "below_start_p == 1"
      if (all(below_start_p[i:(i + TIME_THRESHOLD - 1)] == 1)) {
        # Immediately after, check T consecutive years "above_exit_p == 1"
        if ((i - 1 + TIME_THRESHOLD * 2) <= N &&
            all(above_exit_p[(i + TIME_THRESHOLD):(i + TIME_THRESHOLD * 2 - 1)] == 1)) {
          # Record only once per country
          if (!country_name %in% breakthrough_list) {
            breakthrough_list <- c(breakthrough_list, country_name)
            breakthrough_df <- dplyr::bind_rows(
              breakthrough_df,
              data.frame(Year = year[i + TIME_THRESHOLD], Type = "Fast Exit", Country = country_name)
            )
          }
        }
      }
    }
  }
  
  # ============================================================================
  # PASS 2: Detect GRADUAL EXIT (Below for T, then later Above for T)
  #          — only if not already flagged as Fast Exit.
  # ============================================================================
  for (country_name in country_list) {
    
    df_country <- df |>
      dplyr::ungroup() |>
      dplyr::filter(Country == country_name) |>
      dplyr::select(year, below_start_p, above_exit_p, below_exit_p)
    
    df_country <- df_country |>
      dplyr::reframe(year = seq(from = min(year), to = max(year), by = 1)) |>
      dplyr::left_join(df_country, by = "year") |>
      dplyr::arrange(year) |>
      dplyr::reframe(fill_missing(year, below_start_p, above_exit_p, below_exit_p))
    
    year          <- df_country |> dplyr::pull(year)
    below_start_p <- df_country |> dplyr::pull(below_start_p)
    above_exit_p  <- df_country |> dplyr::pull(above_exit_p)
    N <- length(year)
    
    if (N < TIME_THRESHOLD - 1) next
    seq_th <- seq_len(N - TIME_THRESHOLD + 1)
    
    for (i in seq_th) {
      # First block: T consecutive "below_start_p == 1"
      if (all(below_start_p[i:(i + TIME_THRESHOLD - 1)] == 1)) {
        # Search forward for ANY later T-block of "above_exit_p == 1"
        j <- i + TIME_THRESHOLD
        while (j <= N - TIME_THRESHOLD + 1) {
          if (all(above_exit_p[j:(j + TIME_THRESHOLD - 1)] == 1)) {
            if (!country_name %in% soft_breakthrough_list &&
                !country_name %in% breakthrough_list) {
              soft_breakthrough_list <- c(soft_breakthrough_list, country_name)
              soft_breakthrough_df <- dplyr::bind_rows(
                soft_breakthrough_df,
                data.frame(Year = year[j], Type = "Gradual Exit", Country = country_name)
              )
            }
          }
          j <- j + 1
        }
      }
    }
  }
  
  # ============================================================================
  # PASS 3: Detect RELAPSE (Above for T, then later Below for T)
  # ============================================================================
  for (country_name in country_list) {
    
    df_country <- df |>
      dplyr::ungroup() |>
      dplyr::filter(Country == country_name) |>
      dplyr::select(year, below_start_p, above_exit_p, below_exit_p)
    
    df_country <- df_country |>
      dplyr::reframe(year = seq(from = min(year), to = max(year), by = 1)) |>
      dplyr::left_join(df_country, by = "year") |>
      dplyr::arrange(year) |>
      dplyr::reframe(fill_missing(year, below_start_p, above_exit_p, below_exit_p))
    
    year          <- df_country |> dplyr::pull(year)
    below_start_p <- df_country |> dplyr::pull(below_start_p)
    above_exit_p  <- df_country |> dplyr::pull(above_exit_p)
    N <- length(year)
    
    if (N < TIME_THRESHOLD - 1) next
    seq_th <- seq_len(N - TIME_THRESHOLD + 1)
    
    for (i in seq_th) {
      # First block: T consecutive "above_exit_p == 1"
      if (all(above_exit_p[i:(i + TIME_THRESHOLD - 1)] == 1)) {
        # Search forward for ANY later T-block of "below_start_p == 1"
        j <- i + TIME_THRESHOLD
        while (j <= N - TIME_THRESHOLD + 1) {
          if (all(below_start_p[j:(j + TIME_THRESHOLD - 1)] == 1)) {
            if (!country_name %in% drop_list) {
              drop_list <- c(drop_list, country_name)
              drop_df <- dplyr::bind_rows(
                drop_df,
                data.frame(Year = year[j], Type = "Relapse", Country = country_name)
              )
            }
          }
          j <- j + 1
        }
      }
    }
  }
  
  # ---- Combine all episode types into one tibble ------------------------------
  res <- dplyr::bind_rows(breakthrough_df, soft_breakthrough_df, drop_df)
  
  return(res)
}

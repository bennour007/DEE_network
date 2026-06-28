# =====================================================================
# pipeline_functions.R
#
# Refactor of the digital-trade / DEE analysis script into reusable,
# pipeline-friendly functions, organised into three stages:
#
#   1. RAW DATA PREP      — load + label, no metric construction
#   2. DATA CONSTRUCTION  — all the country/tech_group/category measures
#   3. REGRESSION         — model fitting on the assembled table
#
# KEY DESIGN CHANGE (highlighted throughout with `## CHANGE:`):
#   Every original metric was hard-coded to one grouping set, which forced
#   you to maintain duplicate `_c` (country-only) versions of each block.
#   Here the grouping columns are a PARAMETER (`by`), so a SINGLE function
#   serves every aggregation level:
#       by = c("country", "year")                  # country
#       by = c("country", "tech_group", "year")    # country + tech_group
#       by = c("country", "category", "year")       # country + category
#   The numeric formulas themselves are kept BYTE-FOR-BYTE from your code.
# =====================================================================

# NOTE: package loading belongs in the pipeline/_targets setup, not inside
# functions. Functions below assume these are attached:
#   dplyr, tidyr, tibble, purrr, fixest, countrycode, WDI
# `here` and `readxl`/`readr` only needed by the raw-prep loaders.


# =====================================================================
# 1. RAW DATA PREP
# =====================================================================

#' Load the high-quality bilateral trade file
#'
#' @param path Path to the trade CSV (origin/destination/year/category/value).
#' @return A tibble of raw trade rows, unmodified except for being read in.
load_trade <- function(path) {
  # just read the file; no transformation here so raw load stays isolated
  readr::read_csv(path, show_col_types = FALSE)
}


#' Load the DEE index workbook
#'
#' @param path Path to the DEE .xlsx file.
#' @param sheet Sheet index or name (your script used sheet 3).
#' @return A tibble of the raw DEE data as stored.
load_dee <- function(path, sheet = 3) {
  # raw load only; renaming/iso conversion happens in the construction stage
  readxl::read_excel(path, sheet = sheet)
}


#' Attach the tech_group labelling to raw trade rows
#'
#' Identical case_when mapping to the original script; isolated into its own
#' function so the category->group crosswalk lives in one place.
#'
#' @param trade Raw trade tibble (must contain `category`).
#' @return `trade` with an added `tech_group` column.
label_tech_group <- function(trade) {
  trade %>%
    dplyr::mutate(
      tech_group = dplyr::case_when(
        category %in% c(
          "cloud-computing", "operating-system", "web-hosting",
          "file-hosting-service", "Cybersecurity", "payment-service",
          "data-licensing"
        ) ~ "infrastructure",
        category %in% c(
          "online-marketplace", "digital-advertising", "online-ride-hailing",
          "online-travel-market", "online-food-ordering"
        ) ~ "platform",
        category %in% c(
          "software_enterprise-software_business-intelligence-software",
          "software_enterprise-software_customer-relationship-management-software",
          "software_enterprise-software_enterprise-resource-planning-software",
          "software_enterprise-software_supply-chain-management-software",
          "software_enterprise-software_other-enterprise-software",
          "software_productivity-software_creative-software",
          "software_productivity-software_office-software",
          "software_productivity-software_administrative-software",
          "software_productivity-software_collaboration-software"
        ) ~ "productive",
        category %in% c(
          "digital-media_digital-music_music-streaming",
          "digital-media_video-games_gaming-networks",
          "digital-media_video-games_pc-games",
          "digital-media_video-on-demand_video-streaming-svod",
          "games", "apps", "online-gambling",
          "digital-media_epublishing_ebooks",
          "eservices_dating-services_online-dating",
          "eservices_online-education"
        ) ~ "user_facing",
        TRUE ~ "other"
      )
    )
}


# =====================================================================
# 2. DATA CONSTRUCTION
# =====================================================================
# Helper convention used below:
#   `by`        = full grouping for a metric row, ALWAYS includes "year" and
#                 the node id "country" (named per the metric's perspective).
#   `slice_by`  = the grouping that defines one NETWORK/matrix (drops the node),
#                 e.g. c("tech_group","year") or c("year").
# The original `_c` duplicates collapse into a single call with a different `by`.


## -------------------- 2a. Concentration / diversity --------------------

#' Supplier-concentration metrics (HHI, entropy + n-cleaned versions)
#'
#' ## CHANGE: grouping is now parameterised via `extra_by` instead of being
#'  vs `extra_by = character(0)` when we dont want any groupping
#' hard-coded to `tech_group`. Formulas (HHI, HHI_clean, entropy, entropy_clean)
#' are unchanged from the original. 
#'
#' @param trade Trade tibble with tech_group (from `label_tech_group`).
#' @param extra_by Character vector of grouping cols BEYOND country+year,
#'   e.g. "tech_group", "category", or character(0) for country-only.
#' @return Tibble keyed by country [+ extra_by] + year with concentration cols.
compute_concentration <- function(trade, extra_by = "tech_group") {
  # importer is the focal "country"; supplier is the partner whose share we square
  trade %>%
    # group data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABIAAAASCAYAAABWzo5XAAAAbElEQVR4Xs2RQQrAMAgEfZgf7W9LAguybljJpR3wEse5JOL3ZObDb4x1loDhHbBOFU6i2Ddnw2KNiXcdAXygJlwE8OFVBHDgKrLgSInN4WMe9iXiqIVsTMjH7z/GhNTEibOxQswcYIWYOR/zAjBJfiXh3jZ6AAAAAElFTkSuQmCCto supplier level first: country = importer (iso_d), supplier = iso_o
    dplyr::group_by(
      dplyr::across(
        dplyr::all_of(
          c("country" = "iso_d", extra_by, "year", "supplier" = "iso_o")
        )
      )
    ) %>%
    # collapse categories within the grouping (sum trade per supplier)
    dplyr::summarise(M = sum(trade_value_pred), .groups = "drop") %>%
    # now regroup to the focal node level (drop supplier) to form shares
    dplyr::group_by(dplyr::across(dplyr::all_of(c("country", extra_by, "year")))) %>%
    # supplier share s and supplier count n, exactly as in the original
    dplyr::mutate(
      s = M / sum(M),
      n_suppliers = dplyr::n()
    ) %>%
    # collapse to one row per node: same HHI/entropy formulas as before
    dplyr::summarise(
      n_suppliers   = dplyr::first(n_suppliers),
      HHI           = sum(s^2),
      HHI_clean     = if (dplyr::first(n_suppliers) > 1)
        (sum(s^2) - 1 / dplyr::first(n_suppliers)) / (1 - 1 / dplyr::first(n_suppliers)) else 1,
      entropy       = -sum(s * log(s)),
      entropy_clean = if (dplyr::first(n_suppliers) > 1)
        (-sum(s * log(s))) / log(dplyr::first(n_suppliers)) else 0,
      .groups = "drop"
    )
}


## -------------------- 2b. Net import reliance --------------------

#' Net import reliance (dep_m)
#'
#' ## CHANGE: parameterised grouping. NOTE one behaviour is preserved exactly
#' from your original, including a quirk: the import/export stacking always
#' carries `tech_group`, but in your country-only `_c` version you then grouped
#' by `country, year` only. To reproduce both, `extra_by` controls the FINAL
#' grouping; the stacking keeps tech_group only when it is in `extra_by`.
#'
#' @param trade Trade tibble with tech_group.
#' @param extra_by Grouping beyond country+year (e.g. "tech_group" or none).
#' @return Tibble keyed by country [+ extra_by] + year with M, X, dep_m.
compute_net_reliance <- function(trade, extra_by = "tech_group") {
  # build the column set that the stacked frame should carry
  keep_cols <- c("year", extra_by)
  
  dplyr::bind_rows(
    # rows where the focal country is the IMPORTER: M = value, X = 0
    trade %>% dplyr::transmute(
      country = iso_d,
      dplyr::across(dplyr::all_of(extra_by)),
      year, M = trade_value_pred, X = 0
    ),
    # rows where the focal country is the EXPORTER: M = 0, X = value
    trade %>% dplyr::transmute(
      country = iso_o,
      dplyr::across(dplyr::all_of(extra_by)),
      year, M = 0, X = trade_value_pred
    )
  ) %>%
    # group to the focal node level and form the import share of total trade
    dplyr::group_by(dplyr::across(dplyr::all_of(c("country", keep_cols)))) %>%
    dplyr::summarise(
      M = sum(M), X = sum(X),
      # dep_m = imports / (imports + exports) — identical formula to original
      dep_m = sum(M) / (sum(M) + sum(X)),
      .groups = "drop"
    )
}


## -------------------- 2c. Bilateral asymmetry --------------------

#' Import-weighted net relational exposure (asym)
#'
#' ## CHANGE: parameterised grouping. Formula `sum(s * (s - s_rev))` unchanged.
#' ## BUGFIX (highlighted): your country-only `asymmetry_c` joined `s_rev` from
#' the tech_group-level `shares` object (wrong granularity). Here both sides use
#' the SAME parameterised shares, so the reverse-share join is always consistent.
#'
#' @param trade Trade tibble with tech_group.
#' @param extra_by Grouping beyond country+year.
#' @return Tibble keyed by country [+ extra_by] + year with `asym`.
compute_asymmetry <- function(trade, extra_by = "tech_group") {
  # importer-perspective import shares s[importer <- supplier], parameterised
  shares <- trade %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(
      c("iso_d", extra_by, "year", "iso_o")
    ))) %>%
    dplyr::summarise(M = sum(trade_value_pred), .groups = "drop_last") %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(c("iso_d", extra_by, "year")))) %>%
    dplyr::mutate(s = M / sum(M)) %>%
    dplyr::ungroup() %>%
    dplyr::select(importer = iso_d, supplier = iso_o,
                  dplyr::all_of(extra_by), year, s)
  
  # reverse-share lookup keyed on swapped importer/supplier (+ extra_by, year)
  rev_lookup <- shares %>%
    dplyr::select(importer2 = importer, supplier2 = supplier,
                  dplyr::all_of(extra_by), year, s_rev = s)
  
  shares %>%
    # join each pair to its reverse share (how much the partner relies on us)
    dplyr::left_join(
      rev_lookup,
      by = c("importer" = "supplier2", "supplier" = "importer2",
             extra_by, "year")
    ) %>%
    # pairs with no reverse flow get 0 (partner imports nothing from us)
    dplyr::mutate(s_rev = dplyr::coalesce(s_rev, 0)) %>%
    # aggregate to focal node: import-weighted net exposure
    dplyr::group_by(dplyr::across(dplyr::all_of(c("country" = "importer", extra_by, "year")))) %>%
    dplyr::summarise(asym = sum(s * (s - s_rev)), .groups = "drop")
}


## -------------------- 2d. Cycling / DCI (Leontief diagonal) --------------------

#' Leontief diagonal cycling vector for ONE flow matrix
#'
#' Unchanged from the original `leon_diag`. Operates on a row-normalised
#' square matrix and returns (L_ii - 1)/L_ii per node.
#'
#' @param mat Square, row-normalised flow matrix (rownames = node ids).
#' @return Named numeric vector of cycling scores.
leon_diag <- function(mat) {
  # Leontief inverse of (I - A)
  L <- solve(diag(nrow(mat)) - mat)
  # normalise the diagonal: (L_ii - 1) / L_ii
  (diag(L) - 1) / diag(L)
}


#' Build a square, row-normalised flow matrix for one network slice
#'
#' ## CHANGE: extracted the inline matrix-building map() into a named helper so
#' it is reusable across aggregation levels and testable on its own.
#' ## BUGFIX (highlighted): the original used `replace_values(... NaN -> 0)`
#' (not a base/standard function) AND threw away the normalised result on one
#' path. Here the normalised matrix is assigned and NaN rows (pure importers,
#' rowSums == 0) are zeroed explicitly with base R.
#'
#' @param df Long edges for ONE slice: columns iso_o, iso_d, value.
#' @return Square matrix, rows = origins, cols = destinations, row-normalised.
build_flow_matrix <- function(df) {
  # union of all countries appearing as origin OR destination (square + ordered)
  cs <- sort(unique(c(df$iso_o, df$iso_d)))
  
  mat <- df %>%
    # complete the full country x country grid, missing edges = 0
    tidyr::complete(iso_o = cs, iso_d = cs, fill = list(value = 0)) %>%
    # wide form: destinations become columns
    tidyr::pivot_wider(names_from = "iso_d", values_from = "value", values_fill = 0) %>%
    tibble::column_to_rownames(var = "iso_o") %>%
    as.matrix()
  
  # reorder columns to match row order so the diagonal = self-loops
  mat <- mat[cs, cs, drop = FALSE]
  
  # row-normalise by total out-flow (origin's total exports in this slice)
  rs <- rowSums(mat, na.rm = TRUE)
  # guard zero-out-flow rows: leave them as zeros instead of 0/0 = NaN
  rs[rs == 0] <- 1
  mat / rs
}


#' Cycling index (DCI) at a chosen aggregation level
#'
#' ## CHANGE: replaces BOTH the tech_group `dci` block and the country-only
#' `dci_c` block with one parameterised function.
#'
#' @param trade Trade tibble with tech_group.
#' @param slice_by Cols that define one network besides the node, e.g.
#'   c("tech_group","year") or c("year"). The node ("country") is implicit.
#' @return Tibble keyed by country + slice_by with column `DCI`.
compute_cycling <- function(trade, slice_by = c("tech_group", "year")) {
  trade %>%
    # collapse categories to origin/destination edges within each slice
    dplyr::group_by(dplyr::across(dplyr::all_of(c(slice_by, "iso_o", "iso_d")))) %>%
    dplyr::summarise(value = sum(trade_value_pred), .groups = "drop") %>%
    # nest one edge-list per network slice
    dplyr::group_by(dplyr::across(dplyr::all_of(slice_by))) %>%
    tidyr::nest() %>%
    dplyr::mutate(
      # build matrix then take the Leontief cycling diagonal, returning a tibble
      L = purrr::map(data, function(x) {
        mat <- build_flow_matrix(x)
        tibble::tibble(country = rownames(mat), DCI = as.numeric(leon_diag(mat)))
      })
    ) %>%
    dplyr::select(-data) %>%
    tidyr::unnest("L") %>%
    dplyr::ungroup()
}


## -------------------- 2e. External covariates --------------------

#' Download + clean GDP per capita (constant) from WDI
#'
#' Unchanged indicator (NY.GDP.PCAP.KD) and cleaning from the original.
#'
#' @param years Numeric vector; min/max used as WDI start/end.
#' @return Tibble of country (iso3c), year, GDP.
load_gdp <- function(years) {
  WDI::WDI(
    country = "all",
    indicator = "NY.GDP.PCAP.KD",
    start = min(years, na.rm = TRUE),
    end   = max(years, na.rm = TRUE),
    extra = FALSE
  ) %>%
    dplyr::transmute(country = iso3c, year = year, GDP = NY.GDP.PCAP.KD) %>%
    dplyr::filter(!is.na(country), !is.na(year))
}


#' Standardise the DEE workbook to iso3c + selected pillars
#'
#' @param dee Raw DEE tibble from `load_dee`.
#' @return Tibble of country (iso3c), year, DEE + four pillars.
prep_dee <- function(dee) {
  dee %>%
    dplyr::mutate(
      # convert free-text country name to iso3c to match trade data
      country = countrycode::countryname(Country, destination = "iso3c"),
      year = Year
    ) %>%
    dplyr::select(country, year, DEE, DTE, DMSP, DUC, DTI)
}


## -------------------- 2f. Assembly + standardisation --------------------

#' Z-score helper that returns a plain vector (drops scale() attributes)
#'
#' @param x Numeric vector.
#' @return Standardised numeric vector.
z <- function(x) as.numeric(scale(x))


#' Assemble the modelling table at one aggregation level
#'
#' ## CHANGE: collapses the entire `full_data` / `full_data_c` duplication into
#' one function. `extra_by` drives the trade-side metrics; cycling uses
#' slice_by = c(extra_by, "year"). Standardisation is parameterised too.
#'
#' @param trade Labelled trade tibble.
#' @param dee_clean Output of `prep_dee`.
#' @param gdp Output of `load_gdp`.
#' @param extra_by Grouping beyond country+year ("tech_group", "category", or none).
#' @param norm_within Cols to z-score WITHIN (defaults to c(extra_by,"year")).
#' @return One assembled, standardised modelling tibble.
build_model_table <- function(trade, dee_clean, gdp,
                              extra_by = "tech_group",
                              norm_within = c(extra_by, "year")) {
  # --- trade-side metric blocks, all at the same aggregation level ---
  conc  <- compute_concentration(trade, extra_by = extra_by)
  netr  <- compute_net_reliance(trade,  extra_by = extra_by)
  cyc   <- compute_cycling(trade, slice_by = c(extra_by, "year"))
  asym  <- compute_asymmetry(trade, extra_by = extra_by)
  
  # keys used to join the metric blocks together
  join_keys <- c("country", extra_by, "year")
  
  # --- merge trade metrics, then standardise the trade predictors ---
  metrics <- netr %>%
    dplyr::left_join(conc, by = join_keys) %>%
    dplyr::left_join(cyc,  by = join_keys) %>%
    # z-score the trade predictors WITHIN the chosen grouping (e.g. tech_group+year)
    dplyr::group_by(dplyr::across(dplyr::all_of(norm_within))) %>%
    dplyr::mutate(dplyr::across(
      c(dep_m, n_suppliers, HHI, entropy, DCI, entropy_clean, HHI_clean),
      z, .names = "{.col}_norm"
    )) %>%
    dplyr::ungroup()
  
  # --- bring in GDP, DEE pillars, asymmetry, then standardise outcomes ---
  metrics %>%
    dplyr::left_join(gdp, by = c("country", "year")) %>%
    # full_join keeps DEE rows even where trade metrics are missing (as original)
    dplyr::full_join(dee_clean, by = c("country", "year")) %>%
    dplyr::left_join(asym, by = join_keys) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(norm_within))) %>%
    dplyr::mutate(dplyr::across(
      c(DEE, DUC, DTE, DMSP, DTI, asym),
      z, .names = "{.col}_norm"
    )) %>%
    dplyr::ungroup()
}


# =====================================================================
# 3. REGRESSION
# =====================================================================

#' Fit one fixest model with a parameterised RHS and fixed-effects
#'
#' ## CHANGE: the inline feols calls become one function so the pipeline can
#' map over outcomes / FE specs without copy-paste.
#'
#' @param data Modelling table from `build_model_table`.
#' @param lhs Outcome column name (string), e.g. "DEE".
#' @param rhs Character vector of predictors, e.g. c("DCI_norm","asym_norm").
#' @param fe Character vector of fixed-effect columns, e.g. c("year","tech_group").
#' @param cluster One-sided formula or string for clustering, default ~country.
#' @param subset Optional logical expression (as string) to filter rows.
#' @return A fixest model object.
fit_model <- function(data, lhs, rhs,
                      fe = c("year", "tech_group"),
                      cluster = ~country,
                      subset = NULL) {
  # assemble the formula: lhs ~ rhs | fe
  fml <- stats::as.formula(
    paste0(lhs, " ~ ", paste(rhs, collapse = " + "), " | ", paste(fe, collapse = " + "))
  )
  # optionally restrict rows (e.g. one tech_group for the matched-pillar models)
  if (!is.null(subset)) {
    data <- dplyr::filter(data, !!rlang::parse_expr(subset))
  }
  fixest::feols(fml, data = data, cluster = cluster)
}


#' Fit the matched-pillar set (one pillar per tech_group) in one call
#'
#' ## CHANGE: encodes the pillar<->tech_group matching as data, replacing the
#' four near-identical m_infra/m_plat/m_prod/m_user blocks. Edit `mapping` to
#' change which pillar is paired with which group.
#'
#' @param data Modelling table (must be at country+tech_group level).
#' @param rhs Predictors shared across the pillar models.
#' @param fe Fixed effects (default just year, since each model is one group).
#' @param mapping Named chr: names = tech_group, values = pillar column.
#' @return Named list of fixest models.
fit_matched_pillars <- function(data, rhs,
                                fe = "year",
                                cluster = ~country,
                                mapping = c(infrastructure = "DTI",
                                            platform       = "DMSP",
                                            productive     = "DTE",
                                            user_facing    = "DUC")) {
  # iterate over the group->pillar pairs, fitting one model each
  purrr::imap(mapping, function(pillar, grp) {
    fit_model(
      data, lhs = pillar, rhs = rhs, fe = fe, cluster = cluster,
      subset = paste0("tech_group == '", grp, "'")
    )
  })
}
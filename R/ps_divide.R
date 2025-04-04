#' Stratify patients by propensity score
#'
#' @description
#' Divides patients into equally sized strata based on their propensity scores (PS),
#' using current patients (or current controls only) as reference. Optionally trims
#' external patients whose PS fall outside the reference range.
#'
#' @param object A `combined_trials` object containing estimated propensity scores.
#' @param num_strata Number of strata to divide the reference PS distribution into.
#' @param trim Logical. If `TRUE`, trims external patients whose PS fall outside the range defined by the strata.
#' @param control_only Logical. If `TRUE`, defines strata based only on current controls; otherwise, uses all current patients.
#' @param ... Reserved for future use.
#'
#' @return An object of class `ps_stratified`, which is a modified `combined_trials` object with a new `stratum` column and stratification metadata in the attributes.
#'
#' @export
ps_stratify <- function(object, num_strata, trim = TRUE, control_only = FALSE, ...) {
    UseMethod("ps_stratify")
}

#' @exportS3Method ps_stratify combined_trials
ps_stratify.combined_trials <- function(
        object, num_strata = 5L, trim = TRUE, control_only = FALSE
) {
    stopifnot(inherits(object, "combined_trials"))

    group_var <- attr(object, "group")
    code_var <- attr(object, "current_indicator")
    ps_var <- attr(object, "ps")

    # Choose reference rows (PS's used to define strata)
    ref <- if (control_only) {
        dplyr::filter(object, !!rlang::sym(group_var) == "Control")
    } else {
        dplyr::filter(object, !!rlang::sym(code_var) == 1L)
    }

    sorted_ps <- sort(ref[[ps_var]])
    n_ref <- length(sorted_ps)

    if (n_ref < num_strata) {
        stop("Fewer control patients than number of strata.")
    }

    .stratum_cut <- seq(from = 0, to = 1, length.out = num_strata + 1)[-1]
    cut_points <- c(1L, floor(n_ref * .stratum_cut))
    cut_values <- sorted_ps[cut_points]
    min_ps <- cut_values[1]
    max_ps <- cut_values[length(cut_values)]

    object$stratum <- findInterval(object[[ps_var]], vec = cut_values,
                                   rightmost.closed = TRUE, left.open = TRUE)

    # Trim only external patients outside range
    if (trim) {
        object <- object[!(object[[code_var]] == 0L & (object[[ps_var]] < min_ps | object[[ps_var]] > max_ps)), , drop = FALSE]
    }

    structure(
        object,
        class = c("ps_stratified", setdiff(class(object), "ps_stratified")),
        response = attr(object, "response"),
        current_indicator = code_var,
        covariates = attr(object, "covariates"),
        group = group_var,
        ps = ps_var,
        stratum = "stratum",
        stratification = list(
            num_strata = as.integer(num_strata), trim = trim,
            control_only = control_only, cut_values = cut_values
        )
    )
}

#' @exportS3Method print ps_stratified
print.ps_stratified <- function(x, ...) {
    cat("<ps_stratified> object\n")
    cat("  Strata: ", attr(x, "stratification")$num_strata, "\n")
    cat("  Trim applied: ", attr(x, "stratification")$trim, "\n")
    cat("  Control only: ", attr(x, "stratification")$control_only, "\n")
    cat("  Cut values: ", paste(round(attr(x, "stratification")$cut_values, 4),
                                collapse = ", "), "\n")
    NextMethod("print", x, ...)
}

#' Compute the Stratum-Specific Summary Statistics
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' @param combined_trials An object of class `combined_trials`.
#'
#' @return A data frame with stratum-specific statistics.
#' @exportS3Method summary ps_stratified
summary.ps_stratified <- function(object) {
    stopifnot(inherits(object, "ps_stratified"))

    stratum_col <- rlang::sym(attr(object, "stratum"))
    response_col <- rlang::sym(attr(object, "response"))
    group_col <- rlang::sym(attr(object, "group"))

    no_strat_summarized <- object %>%
        dplyr::group_by(!!group_col) %>%
        dplyr::summarize(
            Y_bar = mean(!!response_col),
            S_sq = dplyr::if_else(
                length(!!response_col)>=2L,
                var(!!response_col), 0),
            n = dplyr::n()
        ) %>%
        dplyr::mutate(stratum = "overall", .before = everything())

    summ <- object %>%
        dplyr::group_by(!!stratum_col, !!group_col) %>%
        dplyr::summarize(
            Y_bar = mean(!!response_col),
            S_sq = dplyr::if_else(
                length(!!response_col) >= 2L,
                var(!!response_col),
                0
            ),
            n = dplyr::n(),
            .groups = "drop_last"
        ) %>%
        dplyr::ungroup() %>%
        tidyr::complete(!!stratum_col, !!group_col,
                        fill = list(Y_bar = 0, S_sq = 0, n = 0)) %>%
        dplyr::mutate(stratum = as.character(!!stratum_col))
    summ <- dplyr::bind_rows(no_strat_summarized, summ) %>%
        dplyr::mutate(stratum = forcats::as_factor(stratum)) %>%
        dplyr::arrange(stratum, !!group_col)

    return(summ)
}

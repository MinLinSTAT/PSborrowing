#' The constructor for the `combined_trials` S3 class
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' @param current_trial The current trial data frame.
#' @param external_trial The external trial data frame.
#' @param response Tidy-select expression for the response variable.
#' @param covariates Tidy-select expression for the covariates.
#' @param group Tidy-select expression for the group variable.
#'
#' @return An object of class `combined_trials`.
#' @export
#'
#' @examples
#' tb <- combine_trials(dat1$trial_data, dat1$external_data,
#'     response = Y,
#'     covariates = c(x1, x2, x3),
#'     group = group)
combine_trials <- function(
    current_trial,
    external_trial,
    response,
    covariates,
    group,
    group_levels = c("Treatment", "Control", "External")
) {
    response <- rlang::enquo(response)
    covariates <- rlang::enquos(covariates)
    group <- rlang::enquo(group)

    # Ensure data frames
    if (!is.data.frame(current_trial) || !is.data.frame(external_trial)) {
        stop("current_trial and external_trial must be data frames.")
    }

    if (length(group_levels) != 3) {
        stop("group_levels must be a character vector of length 3.")
    }


    # Evaluate selections on current_trial
    selected_response <- names(tidyselect::eval_select(response, current_trial))
    selected_covariates <- names(tidyselect::eval_select(rlang::expr(c(!!!covariates)), current_trial))
    selected_group <- names(tidyselect::eval_select(group, current_trial))

    required_columns <- c(selected_response, selected_covariates, selected_group)

    missing_cols_current <- setdiff(required_columns, colnames(current_trial))
    missing_cols_external <- setdiff(required_columns, colnames(external_trial))

    if (length(missing_cols_current)) {
        stop("current_trial is missing columns: ", paste(missing_cols_current, collapse = ", "))
    }
    if (length(missing_cols_external)) {
        stop("external_trial is missing columns: ", paste(missing_cols_external, collapse = ", "))
    }

    combined_trials <- dplyr::bind_rows(
        dplyr::mutate(current_trial, code = 1L),
        dplyr::mutate(external_trial, code = 0L)
    ) %>%
      dplyr::mutate(
        group = factor(
          .data[[selected_group]],
          levels = group_levels,
          labels = c("Treatment", "Control", "External")
        )
      )

    structure(
        combined_trials,
        class = c("combined_trials", class(combined_trials)),
        response = selected_response,
        current_indicator = "code",
        covariates = selected_covariates,
        group = "group"
    )
}

#' @rawNamespace S3method("[", combined_trials)
`[.combined_trials` <- function(x, i, j, drop = FALSE) {
    out <- NextMethod("[")
    attrs <- attributes(x)
    keep <- setdiff(names(attrs), c("names", "row.names", "class"))
    for (a in keep) {
        attr(out, a) <- attrs[[a]]
    }
    class(out) <- class(x)
    out
}

#' @exportS3Method print combined_trials
print.combined_trials <- function(x, ...) {
    cat("<combined_trials> object\n")
    cat("  Rows:", nrow(x), "  Columns:", ncol(x), "\n")
    cat("  Response: ", attr(x, "response"), "\n")
    cat("  Group:    ", attr(x, "group"), "\n")
    cat("  Covariates: ", paste(attr(x, "covariates"), collapse = ", "), "\n")
    cat("  Current indicator: ", attr(x, "current_indicator"), "\n\n")
    NextMethod("print", x, ...)
}


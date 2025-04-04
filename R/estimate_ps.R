#' Estimate propensity scores
#'
#' @param object A combined_trials object.
#' @param formula A formula specifying the PS model.
#' @param method The estimation method ("glm", "cbps", "gbm", "superlearner").
#' @param ... Additional arguments passed to the underlying estimation function.
#'
#' @return The updated object with propensity scores stored in `ps`.
#' @export
estimate_ps <- function(object, formula, method = "glm", ...) {
    UseMethod("estimate_ps")
}

#' @exportS3Method estimate_ps combined_trials
estimate_ps.combined_trials <- function(
        object, formula, method = c("glm"), ...
) {
    stopifnot(inherits(object, "combined_trials"))
    method <- match.arg(method)

    # Extract lhs and rhs
    lhs <- all.vars(formula[[2]])
    rhs <- all.vars(formula[[3]])

    if (!(lhs == attr(object, "current_indicator"))) {
        stop("The left-hand side (lhs) variable '", lhs,
             "' must be ", attr(object, "current_indicator"), ".")
    }

    if (!all(rhs %in% attr(object, "covariates"))) {
        stop("Some right-hand side (rhs) variables are",
             " not found in the covariates.")
    }

    ps <- switch(
        method,
        glm = {
            fit <- glm(formula, data = object, family = binomial(), ...)
            fitted(fit)
        },
        stop("Unknown method: ", method)
    )

    object$ps <- as.numeric(ps)
    attr(object, "ps") <- "ps"
    object
}

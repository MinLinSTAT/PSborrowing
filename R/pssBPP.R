#' @export
pssBPP <- function(object, ...) {
    UseMethod("pssBPP")
}

#' @exportS3Method pssBPP ps_stratified
pssBPP.ps_stratified <- function(object, details = FALSE) {
    # browser()
    summ <- summary(object)
    alpha <- find_alpha(summ, details = details)
    res <- find_posterior(summ, alpha, power = FALSE)
    list(
        summ = summ,
        alpha = alpha,
        posterior = res$posterior,
        combined = res$combined
    )
}

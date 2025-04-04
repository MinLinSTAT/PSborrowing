#' Title
#'
#' @param object a combined_trials object
#' @param A the nominal number of patients to be borrowed
#' @param times the number of samples used to find the template
#' @param ...
#'
#' @returns The result of class maPP_result
#' @export
maPP <- function(object, A, times, ...) {
    UseMethod("maPP")
}

get_template <- function(obj, A = 100L, times = 100L) {
    # browser()
    stopifnot(inherits(obj, "combined_trials"))
    ps_var <- attr(obj, "ps")

    ps_current <- obj[[ps_var]][obj$code == 1L]

    template_lst <- purrr::map(
        1:times, \(i) sample(ps_current, size = A, replace = FALSE)
    )
    ovl <- purrr::map2_dbl(list(ps_current), template_lst, metric_ovl)
    best <- which.max(ovl)
    return(template_lst[[best]])
}

perform_matching_without_replacement <- function(template_ps, external_dat) {
    n_t <- length(template_ps)
    n_e <- nrow(external_dat)
    K <- floor(n_e / n_t)
    matched_ext <- vector("list", K)  # To store results

    mean_t <- mean(template_ps)
    rule1_denom <- sqrt(0.5 * (var(template_ps) + var(external_dat$ps)))
    rule2_left <- log(sd(template_ps))

    # Initial control pool
    remaining_externals <- external_dat

    # browser()

    for (k in 1:K) {
        # Create the data frame for matching
        ps <- c(template_ps, remaining_externals$ps)
        group_int <- rep(c(1L, 0L), c(n_t, nrow(remaining_externals)))
        data <- data.frame(ps = ps, group_int = group_int)

        dist <- optmatch::match_on(group_int ~ ps, data = data)
        matched <- optmatch::pairmatch(dist, data = data)

        # Extract matched external IDs
        selected_idx <- which(!is.na(matched))[-seq_len(n_t)]
        selected_id <- as.integer(names(matched[selected_idx])) - n_t
        selected_ext <- remaining_externals[selected_id, , drop = FALSE]

        # Validate rules before storing the matched externals
        ps_selected <- selected_ext$ps
        rule1 <- abs(mean(ps_selected) - mean_t) / rule1_denom
        rule2 <- abs(rule2_left - log(sd(ps_selected)))

        if (rule1 > 0.1 || rule2 > 0.5 * log(2)) {
            message("Breaking due to rule violation: Rule1=", rule1, ", Rule2=", rule2)
            break
        }

        # Store the selected external matches
        matched_ext[[k]] <- selected_ext %>%
            dplyr::mutate(stratum = k)
        # Update remaining externals by excluding the matched ones
        remaining_externals <- remaining_externals[-selected_id, , drop = FALSE]
    }

    # Remove NULL elements from matched_ext
    matched_ext <- matched_ext %>%
        dplyr::bind_rows()
    matched_ext
}

#' @exportS3Method maPP combined_trials
maPP.combined_trials <- function(object, A, times = 100L, ...) {

    # browser()
    response_col <- rlang::sym(attr(object, "response"))
    ps_col <- rlang::sym(attr(object, "ps"))

    template_ps <- get_template(object, A, times)
    current <- object %>% dplyr::filter(code == 1L)

    summ_trt <- current %>%
        dplyr::filter(group == "Treatment") %>%
        dplyr::summarize(
            mean = mean(!!response_col),
            var = var(!!response_col),
            n = dplyr::n()
        )
    mean_t <- summ_trt$mean; var_t <- summ_trt$var; n_t <- summ_trt$n

    summ_ctl <- current %>%
        dplyr::filter(group == "Control") %>%
        dplyr::summarize(
            mean = mean(!!response_col),
            var = var(!!response_col),
            n = dplyr::n()
        )
    mean_c <- summ_ctl$mean; var_c <- summ_ctl$var; n_c <- summ_ctl$n
    sigma2_current <- ((n_t - 1) * var_t + (n_c - 1) * var_c) / (n_t + n_c - 2)

    current_ps <- current %>% dplyr::pull(!!ps_col)

    ext <- object %>% dplyr::filter(code == 0L)
    matched_ext <- perform_matching_without_replacement(template_ps, ext)
    if (nrow(matched_ext) == 0L) {
        res <- tibble::tibble(
            mean = mean_t - mean_c,
            sd = sqrt(sigma2_current * (1 / n_t + 1 / n_c)),
            alpha = list(NULL),
            lower_bd = qnorm(0.025) * sd + mean,
            upper_bd = qnorm(0.975) * sd + mean
        )
        return(structure(
            res,
            class = c("maPP_result", class(res))
        ))
    }

    ratio_c <- n_c / sigma2_current

    summ_ext <- matched_ext %>%
        dplyr::group_by(stratum) %>%
        dplyr::summarize(
            mean = mean(!!response_col),
            var = var(!!response_col),
            n = dplyr::n(),
            OVL = metric_ovl(current_ps, !!ps_col)
        ) %>%
        dplyr::mutate(
            alpha = OVL / sum(OVL),
            ratio = n * alpha / var
        )
    weights <- c(ratio_c, summ_ext$ratio)
    Deltas <- mean_t - c(mean_c, summ_ext$mean)

    res <- tibble::tibble(
        mean = weighted.mean(Deltas, weights),
        sd = sqrt((sigma2_current / n_t) + 1 / (sum(weights))),
        alpha = list(summ_ext$alpha),
        lower_bd = stats::qnorm(0.025) * sd + mean,
        upper_bd = stats::qnorm(0.975) * sd + mean
    )
    return(structure(
        res,
        class = c("maPP_result", class(res))
    ))
}

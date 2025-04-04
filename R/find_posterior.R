.find_weights <- function(.summ, .post) {
    # Validate input data frames
    required_cols_summ <- c("group", "n", "S_sq")
    if (!all(required_cols_summ %in% colnames(.summ))) {
        stop("`.summ` must contain columns: group, n, S_sq.")
    }

    required_cols_post <- c("variable", "var")
    if (!all(required_cols_post %in% colnames(.post))) {
        stop("`.post` must contain columns: variable, var.")
    }

    .n_tk <- .summ %>% dplyr::filter(group == "Treatment") %>% dplyr::pull(n)
    .n_ck <- .summ %>% dplyr::filter(group == "Control") %>% dplyr::pull(n)
    .n_ek <- .summ %>% dplyr::filter(group == "External") %>% dplyr::pull(n)
    K <- length(.n_tk)

    # Weight 1: Equal weighting
    weight1 <- rep(1 / K, K)

    # Weight 2: Proportional to treatment sample size
    weight2 <- .n_tk / sum(.n_tk)

    .S_sq_tk <- .summ %>% dplyr::filter(group == "Treatment") %>%
        dplyr::pull(S_sq)
    .S_sq_ck <- .summ %>% dplyr::filter(group == "Control") %>%
        dplyr::pull(S_sq)
    .S_sq_ek <- .summ %>% dplyr::filter(group == "External") %>%
        dplyr::pull(S_sq)

    # Weight 7: Inverse variance weighting across groups
    inv_s <- 1 / (.S_sq_tk + .S_sq_ck + .S_sq_ek)
    weight7 <- inv_s / sum(inv_s)

    # Weight 8: Inverse posterior variance weighting for delta_k
    inv_Delta_var <- 1 / (.post %>% dplyr::filter(variable == "delta_k") %>%
                              dplyr::pull(var))
    weight8 <- inv_Delta_var / sum(inv_Delta_var)

    return(list("Equal" = weight1,
                "Treatment ratio" = weight2,
                "Inverse sum of sample variances" = weight7,
                "Inverse posterior probability" = weight8))
}

.combining <- function(.dat, common_weights) {
    # browser()
    # Validate inputs
    if (!"distr" %in% colnames(.dat)) {
        stop("`.dat` must contain a 'distr' column with distribution objects.")
    }
    variable <- unique(as.character(.dat$variable))
    if (length(variable) != 1) {
        stop("`.dat` must contain a 'variable' column with exactly one value.")
    }
    d <- .dat$distr
    K <- length(d)

    if (K < 2) stop("Number of distributions (K) must be at least 2.")

    # Function to combine distributions dynamically
    combine_distributions <- function(weights, weight_label, means, vars,
                                      distributions) {
        # browser()
        combined_distr <- Reduce(`+`, Map(`*`, weights, distributions))
        tibble::tibble(
            weight = weight_label,
            mean = as.vector(weights %*% means),
            var = as.vector((weights^2) %*% vars),
            lower_bound = distr::q.l(combined_distr)(.025),
            upper_bound = distr::q.l(combined_distr)(.975),
            distr = list(combined_distr)
        )
    }

    # Apply combining logic for each weight type
    .res <- purrr::map2(
        common_weights,
        names(common_weights),
        ~ combine_distributions(.x, .y, .dat$mean, .dat$var, d)
    ) %>%
        purrr::list_rbind() %>%
        dplyr::mutate(sd = sqrt(var), .after = var) %>%
        dplyr::mutate(variable = variable, .before = tidyselect::everything())
    return(.res)
}

# .combining with different weights
# compute IPG by distr
.get_combined <- function(.summ, .post, wt) {
    #browser()
    # Process non-stratified (overall) data
    ns <- .post %>%
        dplyr::filter(stratum == "overall") %>%
        dplyr::mutate(weight = "ns", .after = variable) %>%
        dplyr::select(-stratum) %>%
        dplyr::mutate(
            borrow = !stringr::str_detect(variable, "_(nb|t)$"),
            variable = factor(
                variable,
                levels = c("delta_nb", "delta", "theta_c_nb", "theta_c", "theta_t"),
                labels = c("delta", "delta", "theta_c", "theta_c", "theta_t")
            ),
            .before = tidyselect::everything()
        ) %>%
        dplyr::arrange(variable, borrow) %>%
        dplyr::mutate(variable = as.character(variable))

    # Process stratified data and exclude non-borrowed terms
    .post_strat <- .post %>%
        dplyr::filter(stratum != "overall", !stringr::str_detect(variable, "_nb$")) %>%
        dplyr::select(-c(sd:upper_bound))

    # to calculate n_eff
    nt <- .summ %>%
        dplyr::filter(stratum == "overall", group == "Treatment") %>%
        dplyr::pull(n)
    nc <- .summ %>%
        dplyr::filter(stratum == "overall", group == "Control") %>%
        dplyr::pull(n)
    v_nsnb <- ns %>%
        dplyr::filter(!borrow) %>%
        dplyr::mutate(
            variable = stringr::str_remove(variable, "_nb$"),
            var_nsnb = var,
            n = c(NA_integer_, nc, nt),
            .keep = "none"
        )

    # Combine stratified distributions using specified weights
    a <- .post_strat %>%
        dplyr::mutate(variable = as.character(variable)) %>%
        dplyr::group_split(variable) %>%
        purrr::map(~ .combining(.x, common_weights = wt)) %>%
        purrr::list_rbind() %>%
        dplyr::mutate(variable = stringr::str_remove(variable, "(k|_k)$")) %>%
        dplyr::mutate(borrow = TRUE, .before = tidyselect::everything())

    .res <- dplyr::bind_rows(ns, a) %>%
        dplyr::left_join(v_nsnb, by = dplyr::join_by(variable)) %>%
        dplyr::mutate(
            var_ratio = var_nsnb / var,
            n_eff = var_ratio * n
        ) %>%
        dplyr::select(-(var_nsnb:var_ratio))

    # Reshape and calculate effective sample sizes
    .res <- .res %>%
        dplyr::select(borrow:weight, n_eff) %>%
        tidyr::pivot_wider(names_from = variable, values_from = n_eff) %>%
        dplyr::group_by(weight) %>%
        tidyr::fill(theta_t, .direction = "downup") %>%
        dplyr::mutate(
            borrow, weight, variable = "delta",
            n_eff_fill = theta_c + theta_t,
            .keep = "none"
        ) %>%
        dplyr::right_join(
            .res, by = dplyr::join_by(borrow, weight, variable)
        ) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(
            n_eff = dplyr::if_else(is.na(n_eff), n_eff_fill, n_eff),
            weight = forcats::as_factor(weight)
        ) %>%
        dplyr::select(-n_eff_fill) %>%
        dplyr::arrange(variable, weight)
}

# Helper function to create distributions and bounds
create_distribution <- function(df, scale, mean) {
    distr_obj <- distr::Td(df = df) * sqrt(scale) + mean
}
find_bd <- function(distr_obj) {
    bounds <- distr::q.l(distr_obj)(c(.025, .975))
    names(bounds) <- c("lower_bound", "upper_bound")
    bounds
}

.get_posterior <- function(.data, .alpha) {
    #browser()
    # Validate input
    required_cols <- c("group", "n", "Y_bar", "S_sq", "stratum")
    if (!all(required_cols %in% colnames(.data))) {
        stop("`.data` must contain the columns: group, n, Y_bar, S_sq, stratum.")
    }
    if (!all(c("mPI_mean", "mPI_variance") %in% colnames(.alpha))) {
        stop("`.alpha` must contain the columns: mPI_mean, mPI_variance")
    }

    # Filter groups
    trt <- .data %>% dplyr::filter(group == "Treatment")
    ctr <- .data %>% dplyr::filter(group == "Control")
    ext <- .data %>% dplyr::filter(group == "External")

    # Extract variables
    ntk <- trt$n; nck <- ctr$n; nek <- ext$n
    a1k <- .alpha$mPI_mean; a2k <- .alpha$mPI_variance

    # Posterior calculations
    mk <- nck + a1k * nek
    v1k <- (nck * ctr$Y_bar + a1k * nek * ext$Y_bar) / mk
    v2k <- a1k + (nek - 1) * a2k + nck - 1
    v3k <- (nck - 1) * ctr$S_sq + a2k * (nek - 1) * ext$S_sq +
        nck * a1k * nek * (ctr$Y_bar - ext$Y_bar)^2 / mk

    # Summary tibble
    summ <- tibble::tibble(
        mean = c(
            trt$Y_bar,
            v1k, ctr$Y_bar,
            trt$Y_bar - v1k, trt$Y_bar - ctr$Y_bar
        ),
        var = c(
            trt$S_sq * (ntk - 1) / (ntk * (ntk - 3)),
            v3k / (mk * (v2k - 2)),
            ctr$S_sq * (nck - 1) / (nck * (nck - 3)),
            trt$S_sq * (ntk - 1) / (ntk * (ntk - 3)) + v3k / (mk * (v2k - 2)),
            trt$S_sq * (ntk - 1) / (ntk * (ntk - 3)) +
                ctr$S_sq * (nck - 1) / (nck * (nck - 3))
        )
    ) %>% dplyr::mutate(sd = sqrt(var))

    # Add stratum and variable labels
    variable_names <- if (trt$stratum == "overall") {
        c("theta_t", "theta_c", "theta_c_nb", "delta", "delta_nb")
    } else {
        c("theta_tk", "theta_ck", "theta_ck_nb", "delta_k", "delta_k_nb")
    }

    summ <- summ %>% dplyr::mutate(
        stratum = as.character(trt$stratum),
        variable = variable_names,
        .before = tidyselect::everything()
    )

    # Create distributions and bounds
    distr_tk <- create_distribution(ntk - 1, trt$S_sq / ntk, trt$Y_bar)
    distr_ck <- create_distribution(v2k, v3k / (v2k * mk), v1k)
    distr_ck_nb <- create_distribution(nck - 1, ctr$S_sq / nck, ctr$Y_bar)
    distr_k <- distr_tk - distr_ck
    distr_k_nb <- distr_tk - distr_ck_nb

    distributions <- list(
        distr_tk, distr_ck, distr_ck_nb, distr_k, distr_k_nb
    )

    # Combine results
    bdd_combined <- do.call(rbind, lapply(distributions, find_bd))

    summ %>% dplyr::bind_cols(tibble::as_tibble(bdd_combined)) %>%
        dplyr::mutate(distr = distributions)
}


#' Calculate Posterior Values for Summary Data
#'
#' This function calculates posterior values for a given summary data frame (`summ`)
#' using alpha values (`alpha`). It splits the input data by `stratum`, computes
#' posteriors for each group using a helper function `.get_posterior`, and returns
#' a combined tibble with the results.
#'
#' @param summ A data frame containing the summary data. The data frame must include
#' a `stratum` column indicating group stratifications.
#' @param alpha A data frame containing alpha values, also stratified by `stratum`.
#'
#' @param power Whether calculates the power for a design problem. Currently only calculate \eqn{\Pi(\Delta>0|D)}.
#'
#' @return A tibble containing posterior values for each `stratum` and associated
#' variables. The `stratum` and `variable` columns are converted to factors.
#'
#' @details
#' - The `summ` and `alpha` data frames are split by the `stratum` column.
#' - The helper function `.get_posterior` is applied to corresponding subsets of
#'   `summ` and `alpha` to calculate posterior values.
#' - The results are combined into a single tibble and formatted with `stratum` and
#'   `variable` columns as factors.
#' @export
find_posterior <- function(summ, alpha, power = FALSE) {
    if (!all(unique(summ$stratum) %in% unique(alpha$stratum))) {
        stop("Mismatch in `stratum` groups between `summ` and `alpha`.")
    }

    post <- summ %>% dplyr::group_split(stratum) %>%
        purrr::map2(dplyr::group_split(alpha, stratum),
                    ~ .get_posterior(.x, .y)) %>%
        purrr::list_rbind() %>%
        dplyr::mutate(
            stratum = forcats::as_factor(stratum),
            variable = forcats::as_factor(variable)
        )
    wt <- .find_weights(summ %>% dplyr::filter(stratum != "overall"), post)
    res <- .get_combined(summ, post, wt)
    if(power) {
        res <- res %>%
            dplyr::rowwise() %>%
            dplyr::mutate(tail_probability = 1 - distr::p(distr)(0)) %>%
            dplyr::ungroup() %>%
            dplyr::select(-distr)
        return(list(
            posterior = post %>% dplyr::select(-distr),
            combined = res %>% dplyr::filter(variable == "delta")
        ))
    }
    return(list(
        posterior = post %>% dplyr::select(-distr),
        weights = wt,
        combined = res %>% dplyr::select(-distr) %>%
            dplyr::filter(variable == "delta")
    ))
}

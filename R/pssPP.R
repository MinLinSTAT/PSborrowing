
# from R pacnum_strataage psrwe for overlapping coefficient

metric_ovl <- function(draws1, draws2) {
    draws <- c(draws1, draws2)

    # Discrete case
    if (length(unique(draws)) <= 10) {
        all_x <- c(rep(0, length(draws1)),
                   rep(1, length(draws2)))
        pt <- apply(prop.table(table(draws, all_x), 2), 1, min)
        return(sum(pt))
    }

    # Continuous case
    mn <- max(0, min(draws) - 1e-3)
    mx <- min(1, max(draws) + 1e-3)

    f1 <- approxfun(density(draws1, from = mn, to = mx, bw = "nrd"))
    f2 <- approxfun(density(draws2, from = mn, to = mx, bw = "nrd"))

    s <- tryCatch(
        integrate(function(x) pmin(f1(x), f2(x)),
                  lower = mn, upper = mx, subdivisions = 500)$value,
        error = function(e) NA_real_
    )

    s
}

#' Propensity-score stratified power prior (Wang et al. 2019)
#' Stratify using the whole current RCT following Chen et al. (2020) and
#' use equal weights to combine
#'
#' @param object An object of class ps_stratified
#'
#' @param A The nominal number of patients to be borrowed
#'
#' @export
pssPP <- function(object, A, ...) {
    UseMethod("pssPP")
}


#' @exportS3Method pssPP ps_stratified
pssPP.ps_stratified <- function(object, A) {
    stopifnot(inherits(object, "ps_stratified"))

    stratum_var <- attr(object, "stratum")
    code_var <- attr(object, "current_indicator")
    ps_var <- attr(object, "ps")
    num_strata <- attr(object, "stratification")$num_strata

    group_col <- rlang::sym(attr(object, "group"))

    split_st_trial = split(object,list(object[[stratum_var]],object[[code_var]]))

    summ <- summary(object)

    strat_summarized <- summ %>%
        dplyr::filter(stratum != "overall") %>%
        dplyr::mutate(stratum = forcats::fct_drop(stratum))

    no_strat_summarized <- summ %>%
        dplyr::filter(stratum == "overall")

    strat_trt <- strat_summarized %>%
        dplyr::filter(!!group_col == "Treatment")
    strat_ctl <- strat_summarized %>%
        dplyr::filter(!!group_col == "Control")
    strat_ext <- strat_summarized %>%
        dplyr::filter(!!group_col == "External")

    N_t <- strat_trt %>% dplyr::pull(n)
    s_2_t <- strat_trt %>% dplyr::pull(S_sq)
    means_t <- strat_trt %>% dplyr::pull(Y_bar)

    N_c <- strat_ctl %>% dplyr::pull(n)
    s_2_c <- strat_ctl %>% dplyr::pull(S_sq)
    means_c <- strat_ctl %>% dplyr::pull(Y_bar)

    N_e <- strat_ext %>% dplyr::pull(n)
    s_2_e <- strat_ext %>% dplyr::pull(S_sq)
    means_e <- strat_ext %>% dplyr::pull(Y_bar)

    rs <- vector("list", length = num_strata)

    if (num_strata > 1) {
        for(L in 1:num_strata) {
            rs[[L]] <- ifelse(
                N_e[L] > 1,
                metric_ovl(
                    split_st_trial[[L]][[ps_var]],
                    split_st_trial[[L+num_strata]][[ps_var]]
                ),
                0
            )
        }
        rs <- unlist(rs)
        vs <- rs / sum(rs)
    } else {
        vs <- ifelse(N_e[L] > 1, 1, 0)
    }

    alpha <- pmin(A * vs / N_e, 1)
    alpha[is.nan(alpha)] <- 0
    alpha[is.na(alpha)] <- 0

    m <- N_c + alpha * N_e
    nu1 <- (means_c * N_c + means_e * alpha * N_e) / m
    nu2 <- N_c - 1 + alpha * (N_e - 1)
    nu3 <- (N_c - 1) * s_2_c +
        alpha * (N_e - 1) * s_2_e +
        (N_c * alpha * N_e / m) * (means_c - means_e)^2

    Delta_list <- Map(
        function(m_t, v_t, n_t, v1, v2, v3, m_c) {
            (rt(1e6, df = n_t - 1) * sqrt(v_t / n_t) + m_t) -
                (rt(1e6, df = v2) * sqrt(v3 / (v2 * m_c)) + v1)
        },
        means_t, s_2_t, N_t, nu1, nu2, nu3, m
    )

    Delta <- Reduce(`+`, Delta_list) / num_strata

    res <- tibble::tibble(
        mean = mean(Delta),
        sd = sd(Delta),
        alpha = list(alpha),
        tibble::as_tibble_row(boa::boa.hpd(Delta, 0.05)) %>%
            dplyr::rename(
                LB_boa = `Lower Bound`,
                UB_boa = `Upper Bound`
            ),
        n_e = list(N_e),
    )
    structure(
        res,
        class = c("pssPP_result", class(res))
    )
}

.find_f_tail <- function(df1, df2, target_ratio) {
    # browser()
    if(df1 <= 2) return(0)
    .mode <- ((df1-2)/df1) * (df2/(df2+2))
    df_val <- df(target_ratio, df1, df2)
    f <- function(x) {
        (df(x, df1, df2) - df_val)^2
    }
    if(target_ratio > .mode) {
        .q <- stats::optim(par = .mode/2, fn = f, method = "Brent",
                    lower = 0, upper = .mode)
        if(!(dplyr::near(.q$value, 0))) stop("not convergent!")
        .ans <- min(
            pf(.q$par, df1, df2),
            1 - pf(target_ratio, df1, df2)
        )
    } else if (target_ratio < .mode) {
        .q <- stats::optim(par = 2*.mode - target_ratio, fn = f,
                method = "Brent", lower = .mode,
                upper = qf(1-1e-6, df1, df2))
        if(!(dplyr::near(.q$value, 0))) stop("not convergent!")
        .ans <- min(
            pf(target_ratio, df1, df2),
            1 - pf(.q$par, df1, df2)
        )
    }
    return(.ans)
}

.find_alpha_Dec9 <- function(.data, details, n_sample = 1e5) {
    # browser()
    control <- .data %>% dplyr::filter(group == "Control")
    external <- .data %>% dplyr::filter(group == "External")

    nc <- control$n; nh <- external$n
    nu_mean_c <- nc - 1; nu_mean_h <- nh - 1
    if(nu_mean_c <= 2L || nu_mean_h <= 2L) return(tibble::tibble(
        alpha1 = 0, alpha2 = 0
    ))
    mu_mean_c <- control$Y_bar
    mu_mean_h <- external$Y_bar
    sigma_mean_c <- sqrt(control$S_sq / control$n)
    sigma_mean_h <- sqrt(external$S_sq / external$n)

    sample_mean_c <- rt(n_sample, df = nu_mean_c) * sigma_mean_c + mu_mean_c
    sample_mean_h <- rt(n_sample, df = nu_mean_h) * sigma_mean_h + mu_mean_h
    sample_var_c <- control$S_sq /
        rgamma(n_sample, shape = 0.5 * (nc - 1), rate = 0.5 * (nc - 1))
    sample_var_h <- external$S_sq /
        rgamma(n_sample, shape = 0.5 * (nh - 1), rate = 0.5 * (nh - 1))

    diff_mean <- sample_mean_c - sample_mean_h
    left_mean <- mean(diff_mean < 0)

    p_mean <- min(left_mean, 1-left_mean) # tail probability

    target_ratio <- external$S_sq / control$S_sq
    p_var <- .find_f_tail(df1 = nu_mean_h, df2 = nu_mean_c,
                          target_ratio = target_ratio)
    if(!details) {
        return(tibble::tibble(
            mPI_mean = dplyr::if_else(p_mean < 0.001, 0, p_mean),
            mPI_variance = dplyr::if_else(p_var < 0.001, 0, p_var)
        ))
    } else {
        return(tibble::tibble(
                mPI_mean = dplyr::if_else(p_mean < 0.01, 0, p_mean),
                mPI_variance = dplyr::if_else(p_var < 0.01, 0, p_var),
                samples = list(tibble::tibble(
                    group = forcats::as_factor(rep(c("Control", 'External'),
                                                   each = n_sample)),
                    mean = c(sample_mean_c, sample_mean_h),
                    var = c(sample_var_c, sample_var_h)
                ))
            )
        )
    }
}

#' Calculate Alpha Values from Summary Data
#'
#' This function processes a summary data frame (`summ`) to calculate alpha values
#' for both the overall data and stratified groups. It uses the helper function
#' `.find_alpha_Dec9` for the computation.
#'
#' @param summ A data frame containing the summary data. The data frame must
#' include a `stratum` column, where "overall" indicates the overall group and
#' other values indicate stratified groups.
#'
#' @return A data frame (tibble) with calculated alpha values for both the
#' overall group and each stratified group. The output includes a `stratum` column
#' indicating the corresponding group.
#'
#' @details
#' - The input data frame is split into two subsets:
#'   - `no_strat`: Rows with `stratum == "overall"`.
#'   - `strat`: Rows with `stratum != "overall"`.
#' - The `.find_alpha_Dec9` function is applied to each subset to calculate the alpha values.
#' - The results are combined into a single tibble, with `stratum` indicating
#'   the group for each set of alpha values.

#' @export
find_alpha <- function(summ, details = FALSE) {
    # browser()
    # Check for required column
    if (!"stratum" %in% colnames(summ)) {
        stop("The input data frame must contain a 'stratum' column.")
    }

    # Separate 'overall' and stratified data
    no_strat <- summ %>% dplyr::filter(stratum == "overall")
    strat <- summ %>% dplyr::filter(stratum != "overall")

    # Process 'overall' data
    no_strat_alpha <- no_strat %>%
        .find_alpha_Dec9(details = details) %>%
        dplyr::mutate(stratum = "overall", .before = tidyselect::everything())

    # Process stratified data
    if (nrow(strat) > 0) {
        strat_alpha <- strat %>%
            dplyr::group_split(stratum, .keep = TRUE) %>%
            purrr::map(~ .find_alpha_Dec9(.x, details = details)) %>%
            purrr::list_rbind() %>%
            dplyr::mutate(stratum = unique(strat$stratum),
                          .before = tidyselect::everything())
    } else {
        strat_alpha <- tibble::tibble()
    }

    # Combine results
    alpha <- dplyr::bind_rows(no_strat_alpha, strat_alpha) %>%
        dplyr::mutate(stratum = forcats::as_factor(stratum))
    return(alpha)
}

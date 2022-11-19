#' Calibration for `PML` model
#'
#' @inheritParams PML
#' @inheritParams rtop::sceua
#'
#' @param IGBPcode (optional) If `hc_raw` specified in `par`, `IGBPcode` can be ignored.
#' Otherwise, `IGBPcode` should be provided.
#'
#' @importFrom rtop sceua
#' @export
PML_calib <- function(data, IGBPcode = 1, of_gof = NSE, ..., verbose = TRUE, maxn = 1e3) {
    # c("Alpha", "Thelta", "m", "Am_25", "D0", "kQ", "kA", "S_sls", "fER0", "VPDmin", "VPDmax")
    parnames = param0_PML$name
    npar = length(parnames)
    par0 = with(param0_PML, set_names(par, name))[parnames]
    lb = with(param0_PML, set_names(lb, name))[parnames]
    ub = with(param0_PML, set_names(ub, name))[parnames]

    # cobyla:
    # bobyqa: slightly better
    par = nloptr::bobyqa(par0, PML_goal, lb, ub,
        data = data, IGBPcode = IGBPcode, of_gof = of_gof)$par
    # par = RcppDE::DEoptim(PML_goal, lb, ub,
    #     control = DEoptim.control(trace = FALSE, NP = 11*10),
    #     data = data, IGBPcode = IGBPcode, of_gof = of_gof)$optim$bestmem
    # par = RMPSH::RMPSH_opt(par0, PML_goal, lb, ub,
    #     data = data, IGBPcode = IGBPcode, of_gof = of_gof)
    # par = sceua(PML_goal, par0, lb, ub, iprint = ifelse(verbose, 0, -1), maxn = maxn,
    #                 data = data, IGBPcode = IGBPcode, of_gof = of_gof)$par
    par = set_names(par, parnames)
    par["hc"] = options_param$hc_raw[IGBPcode + 1]

    gof = PML_goal(par, data, detailed = TRUE, of_gof = NSE)
    cat(str(gof))
    # cat(bold(green(sprintf("[%s], NSE_ET=%.3f, NSE_GPP=%.3f\n",
    #                         IGBPnames_005[IGBPcode+1], gof[1], gof[2]))))
    listk(data = PML_predict(data, par), gof = as.list(gof),
        model = listk(par, data))
}

#' @rdname PML_calib
#' @export
PML_predict <- function(data, par, IGBPcode = NULL) {
    d_sim = PML(data, par, IGBPcode = IGBPcode)
    cbind(data[, .(date, site, GPPobs, ETobs)], d_sim[, .(GPP, ET)])
}

cal_yearly <- function(d) {
    vars = c("GPP", "ET") %>% {c(paste0(., "obs"), .)} %>% intersect(colnames(d))
    d[, map(.SD, ~ mean(.x, na.rm = TRUE) * 365), .(site, year(date)), .SDcols = vars]
}

caL_yearly_anorm <- function(d) {
    vars = c("GPP", "ET") %>% {c(paste0(., "obs"), .)} %>% intersect(colnames(d))
    d[, c(listk(year), map(.SD, ~ .x - mean(.x, na.rm = TRUE))), .(site), .SDcols = vars]
}

#' @rdname PML_calib
#' @export
PML_gof <- function(dat, of_gof = NSE, detailed = TRUE) {
    # gof_GPP = dat[, (of_gof(GPPobs, GPP)), by]
    # gof_ET = dat[, (of_gof(ETobs, ET)), by]
    vars = c("GPP", "ET") %>% {c(paste0(., "obs"), .)} %>% intersect(colnames(dat))
    dat_year = dat[, map(.SD, ~ mean(.x, na.rm = TRUE) * 365), .(site, year(date)), .SDcols = vars]
    dat_year_ano = dat_year[, map(.SD, ~ .x - mean(.x, na.rm = TRUE)), .(site), .SDcols = vars]

    if (detailed) {
        list(`8-day` = .gof(dat, detailed = detailed),
            yearly = .gof(dat_year, detailed = detailed),
            yearly_anom = .gof(dat_year_ano, detailed = detailed))
    } else .gof(dat) + .gof(dat_year_ano) # + .gof(dat_year)
}

.gof <- function(dat, of_gof = NSE, by, detailed = FALSE) {
    gof_GPP = NULL
    if ("GPPobs" %in% colnames(dat)) {
        gof_GPP = dat[, (of_gof(GPPobs, GPP)), by]
    }
    gof_ET = dat[, (of_gof(ETobs, ET)), by]

    if (detailed) {
        list(ET = gof_ET, GPP = gof_GPP)
    } else 1 - (gof_GPP + gof_ET) / 2
}

#' @rdname PML_calib
#' @export
#' @import crayon
PML_goal <- function(par, data, IGBPcode = NULL, ..., of_gof = NSE, detailed = FALSE) {
    dat <- PML_predict(data, par, IGBPcode = IGBPcode)
    # smaller, better
    PML_gof(dat, of_gof = of_gof, detailed = detailed)
}

# 为了最后的结果展示
cal_gof <- function(df, by = "IGBP") {
  .cal_gof = function(df) {
    ans = df[, GOF(yobs, ysim), by = by] %>%
      rbind(., cbind(IGBP = "ALL", df[, GOF(yobs, ysim)])) %>%
      dplyr::select(IGBP, R2, KGE, NSE, R, RMSE, Bias_perc, n_sim)
    ans
  }
  GPP = df[, .(yobs = GPPobs, ysim = GPP, IGBP, site)] %>% .cal_gof()
  ET  = df[, .(yobs = ETobs , ysim = ET , IGBP, site)] %>% .cal_gof()
  info = listk(ET, GPP) %>% melt_list("variable") %>%
    mutate(IGBP = factor(IGBP, c(IGBPinfo$IGBPname, "ALL"))) %>%
    .[order(IGBP, variable), ]
  info %>% as_tibble %>% dt_round(3) %>%
    subset(!is.na(IGBP))
}

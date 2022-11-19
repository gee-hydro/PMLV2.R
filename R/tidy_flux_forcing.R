
#' Tidy meteorological forcing of FLUXNET
#'
#' @param df A data.frame read from FLUXNET
#' - `Pa`     : kPa
#' - `Rs`     : W m-2
#' - `Tavg`   : degC
#' - `Albedo` : -
#' - `Emiss`  : -
#' - `LAI`    : -
#'
#' - `LE`     : W m-2, (will be converted to mm/d)
#' - `GPP_DT`,`GPP_NT`: `GPPobs = nanmean(GPP_DT, GPP_NT)`, `gC m-2 d-1`
#'
#' @return
#' - lambda
#' - Rn
#' - Eeq
#' - rou_a
#' - gama
#' - epsilon
#' - fval_soil
#'
#' @references
#' 1. Convert PAR from MJ/m2/d to umol m-2 s-1, http://www.egc.com/useful_info_lighting.php
#' 2. https://appgeodb.nancy.inra.fr/biljou/pdf/Allen_FAO1998.pdf
#' @export
forcing_tidy_flux <- function(df) #, par
{
    Pa        = df$Pa;       # kPa
    Rs        = df$Rs;       # W m-2
    # Rn      = df$Rn;       # W m-2
    Tavg      = df$Tavg;     # 0C
    # PAR       = 0.45*Rs;         # W m-2, taken as 0.45 time of solar radiation
    ## Convert PAR from MJ/m2/d to umol m-2 s-1
    # 1 W m-2 = 4.57 umol m-2 s-1
    # PAR_mol   = PAR * 4.57; # from [W m-2] to [umol m-2 s-1]

    lambda    = cal_lambda(Tavg) * 1000 # MJ kg-1 to J g-1
    # 1. obs ET and GPP
    ETobs  = df$LE / lambda * 86400 * 10^-3     # W M-2 to mm
    # GPPobs = df$GPP_NT;                        # [g C m-2 d-1]
    GPPobs = df[, .(GPP_DT, GPP_NT)] %>% as.matrix() %>% rowMeans(na.rm = TRUE)

    # 2. add VPD and Rn
    Rn <- get_Rn(df$Rs, df$Rl_in, Tavg, df$Albedo, df$Emiss)

    # 2. intermediate variables
    rou_a     = 3.846*10^3 * Pa / (Tavg+273.15);             # kg m-3
    gama      = Cp * Pa / (0.622*lambda);                    # kpa deg-1
    slop      = 4098 * 0.6108 * exp((17.27 * Tavg) / (Tavg+237.3)) / (Tavg+237.3)^2; # kpa deg-1
    epsilon   = slop / gama;

    ## Equilibrium evaporation
    Eeq       = epsilon / (epsilon+1) * Rn / lambda*86400*10^-3; # mm
    Eeq       = pmax(0.0001, Eeq);

    # Evaporation from soil surface
    if (is.null(df$f_value_soil)) {
        kA           = 0.9; #insensitive parameter for PMLv2
        Tou          = exp(-kA*df$LAI);
        Es_eq        = Eeq  *  Tou;     # Soil evaporation at equilibrium, mm/d
        fval_soil = add_fvalSoil(df$Prcp, Es_eq, 3, df$site)
    }

    ## Soil heat flux data not available
    # if (!is.null(df$G_soil)) {
    #     G_soil       = (Tavg-Tavg_1) * 1/0.408*0.38 / (lambda * 1e-3);# MJ /m2/day
    #     G_soil       = G_soil / (1 / lambda*86400*10^-3);# W/m2
    # }
    out <- data.table(ETobs, GPPobs, Rn, Eeq, lambda, rou_a, gama, epsilon, fval_soil)
    cbind(df, out)
}

# 需要每个站点单独计算
add_fvalSoil <- function(prcp, Es_eq, frame = 3, sites = NULL) {
    # sites = NULL
    if (!is.null(sites)) {
        # site should be in the order
        d <- data.table(site =sites, prcp, Es_eq)
        fval_soil = d[, movmean2(prcp, frame, 0) / movmean2(Es_eq, frame, 0), .(site)]$V1
    } else {
        if (is.matrix(prcp)) {
            # Ei只占不到10%，因此前期土壤含水量可以用Prcp/Es_eq代替Pi/Es_eq
            # 每年的前frame-1个时间段中，frame变小；8-day尺度 应该影响不大
            fval_soil = movmean_2d(prcp, frame, 0) / movmean_2d(Es_eq, frame, 0)
        } else {
            fval_soil = movmean2(prcp, frame, 0) / movmean2(Es_eq, frame, 0)
        }
    }
    # fval_soil = movmean(prcp, [frame, 0], 'omitNA')  / movmean(Es_eq, [frame, 0], 'omitNA');
    clamp(fval_soil, lims = c(0, 1))
}

add_Es <- function(l, frame = 3) {
    fval_soil <- add_fvalSoil(l$Pi, l$Es_eq, frame)
    l$Es <- fval_soil * l$Es_eq
    l
}

#' @export
add_ETsum <- function(l) {
    l$Ec <- l$Ecr + l$Eca
    l$ET <- l$Ec + l$Ei + l$Es
    l
}

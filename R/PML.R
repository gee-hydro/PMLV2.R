#' Calculation of actual evaporation and GPP using the PML_V2 for global land
#' surface for each time step (16-day).
#'
#' This algorithm fits to high resolution data data in GEE.
#' 
#' @param data A data.frame with the columns of
#' - `Prcp`   : Precipitation, mm
#' - `Tavg`   : daily average temperature, deg C
#' - `Rs`     : shortwave radiation, W m-2
#'
#' - `Pa`     : surface air pressure, kPa
#' - `U2`     : wind speed, m/s
#' - `LAI`    : m2 m-2
#'
#' (can be calculated by other variables)
#' - `Rn`     : net solar radiation, W m-2
#'      + `albedo`
#'      + `emiss`
#'      + `Rln`, W m-2 (plus Rs, Tavg): `Rn = get_Rn(Rs, Rln, Tavg, albedo, emiss)`
#' - `VPD`    : Vapor pressure deficit, kPa
#'      + `q`, kg/kg (plus Pa): `VPD = get_VPD(Pa, q)`
#'
#' (optional)
#' - `CO2`     : about 380-570, umol mol-1
#' - `IGBPname`: If not specified parameters, this column is necessary for matching
#'      parameter.
#'
#' (optional, for speeding up optimization)
#' - `lambda`  :
#' - `rou_a`   :
#' - `gama`    :
#' - `epsilon` :
#' 
#' @param par PML_V2 parameters (see Zhang 2019 for details).
#' - `hc`      : canopy height (m)
#' - `Alpha`   : initial photochemical efficiency, 0.02-0.08
#' - `m`       : the initla slope of CO2 response curve `[umol m-2 s-1]/[umol mol-1]`
#' - `Am_25`   : 10 - 150
#' - `D0`      : the value of VPD when stomtal conductance is reduced
#' - `kQ`      : 0.7,
#' - `kA`      : 0.7, light extinction coefficient
#' - `S_sls`   :
#' - `fER0`    :
#' - `VPDmin`  :
#' - `VPDmax`  :
#'
#' @param V2 Boolean, default `TRUE`. If `TRUE` `PML_V2` used, otherwise `PML_V1`.
#' @param CO2 A constant number. If provided, CO2 in the `data` will be replaced.
#' @param ratio land cover ratio
#' @param return.Es Because `fval_soil` need moving average in temporal. We can't
#' get it directly if the `data` is Spatial pixels.
#' @param IGBPname IGBPname
#' @param scale one of c("site", "regional").
#' @param ... ignored
#' 
#' @return
#' - `GPP`: gC m-2 d-1, Gross primary product
#' - `Ec` : mm d-1, Vegetation transpiration
#' - `Es` : mm d-1, Soil evaporation |
#' - `Ei` : mm d-1, Interception from vegetation canopy
#' - `Es_eq`: mm d-1, Soil evaporation at equilibrium
#' - `Pi`: mm d-1, `Prcp - Ei`
#'
#' @references
#' 1. Zhang, Y., Kong, D., Gan, R., Chiew, F. H. S., McVicar, T. R., Zhang, Q.,
#'    & Yang, Y. (2019). Coupled estimation of 500 m and 8-day resolution global
#'    evapotranspiration and gross primary production in 2002–2017. Remote
#'    Sensing of Environment, 222, 165–182. <doi:10.1016/j.rse.2018.12.031>.
#' 2. Kong, D., Zhang, Y., Gu, X., & Wang, D. (2019). A robust method for
#'    reconstructing global MODIS EVI time series on the Google Earth Engine.
#'    ISPRS Journal of Photogrammetry and Remote Sensing, 155, 13–24.
#'    <https://doi.org/10.1016/j.isprsjprs.2019.06.014>
#' 3. basic units change, <http://www.fao.org/docrep/X0490E/x0490e0i.htm>
#'
#' @export
PML <- function(
  data, par = NULL, CO2 = NULL, IGBPname = NULL, ratio = 1, ..., 
  scale = c("site", "regional"), V2 = TRUE, return.Es = TRUE) 
{
  scale <- match.arg(scale)
  # 380, 475, 570# umol mol-1,475; #data$CO2
  Ca <- if (is.null(CO2)) data$CO2 else CO2

  # PML-V1 constan par
  Q50 <- 30 # the value of absorbed PAR when gs=gsx/2, W/m2
  D0 <- 0.7
  kQ <- 0.6
  kA <- 0.7 # extinction coefficient
  LAIref <- 5 # par[10);  # 1-5

  ind <- match(IGBPname, options_param$IGBPname) # index of options_param

  if (is.null(par)) {
    Alpha  <- options_param$Alpha[ind] # initial photochemical efficiency, 0.02-0.08
    Thelta <- options_param$Thelta[ind] #
    m      <- options_param$m[ind] # Ball-Berry coefficient 2-20
    Am_25  <- options_param$Am_25[ind] # 10 - 150
    D0     <- options_param$D0[ind]
    kQ     <- options_param$kQ[ind]
    kA     <- options_param$kA[ind]
    S_sls  <- options_param$S_sls[ind]
    fER0   <- options_param$fER0[ind]
    VPDmin <- options_param$VPDmin[ind]
    VPDmax <- options_param$VPDmax[ind]
    hc     <- options_param$hc_raw[ind]
  } else {
    # parnames = c("Alpha", "Thelta", "m", "Am_25", "D0", "kQ", "kA", "S_sls", "fER0", "VPDmin", "VPDmax")
    if (is.null(names(par))) par %<>% set_names(param0_PML$name)
    # works for vec, data.frame, data.table or list
    Alpha  <- par[["Alpha"]] # initial photochemical efficiency, 0.02-0.08
    Thelta <- par[["Thelta"]] #
    m      <- par[["m"]] # Ball-Berry coefficient 2-20
    Am_25  <- par[["Am_25"]] # 10 - 150
    D0     <- par[["D0"]]
    kQ     <- par[["kQ"]]
    kA     <- par[["kA"]]
    S_sls  <- par[["S_sls"]]
    fER0   <- par[["fER0"]]
    VPDmin <- par[["VPDmin"]]
    VPDmax <- par[["VPDmax"]]

    hc <- ifelse("hc" %in% names(par), par[["hc"]], options_param$hc_raw[ind])
  }

  ## 2. ASSIGN INPUT VARIABLES
  Prcp    <- data$Prcp # mm/d
  Tavg    <- data$Tavg # degC
  Rs      <- data$Rs # W m-2
  Pa      <- data$Pa # kPa
  u2      <- data$U2 # m/s
  LAI     <- data$LAI # m2 m-2

  PAR     <- 0.45 * Rs # W m-2, taken as 0.45 time of solar radiation
  PAR_mol <- PAR * 4.57 # 1 W m-2 = 4.57 umol m-2 s-1

  Rn <- data$Rn # W m-2
  if (is.null(Rn)) Rn <- get_Rn(Rs, data$Rln, Tavg, data$Albedo, data$Emiss)
  # if (is.null(Rn)) Rn <- get_Rn(Rs, data$Rln, Tavg, data$albedo, data$emiss)

  VPD <- data$VPD # kPa
  if (is.null(VPD)) VPD <- get_VPD(Pa, data$q, data$Tmax, data$Tmin)
  # if (is.null(VPD))  VPD <- get_VPD(Pa, data$q, data$Tmax, data$Tmin)

  # outside static variables
  lambda <- data$lambda
  if (is.null(lambda)) lambda <- get_lambda(Tavg) # [2500 J g-1]

  rou_a <- data$rou_a
  gama <- data$gama
  epsilon <- data$epsilon
  Eeq <- data$Eeq

  if (is.null(rou_a)) {
    rou_a   <- 3.846 * 10^3 * Pa / (Tavg + 273.15)
    gama    <- Cp * Pa / (0.622 * lambda)
    slop    <- cal_slope(Tavg) # kPa deg-1
    epsilon <- slop / gama
    Eeq     <- epsilon / (epsilon + 1) * Rn / lambda * 86400 * 10^-3 # W m-2 to mm
    Eeq     <- clamp_min(Eeq, 0.0001)
  }

  ## 3. PML_V2 GPP and Canopy conductance (Gc)
  # 1. Leuning f_vpd
  f_VPD_gc <- 1 / (1 + VPD / D0)
  # # 2. Rong f_vpd
  # VPD_sqrt = sqrt(VPD);
  # f_VPD = VPD_sqrt  * (VPD_sqrt < D0) + 1 /VPD_sqrt  * (VPD_sqrt >= D0);
  # f_VPD = min(f_VPD, 1);

  # 2. Piecewise function
  f_VPD <- 0 * (VPD > VPDmax) + (VPDmax - VPD) / (VPDmax - VPDmin) * (VPD >= VPDmin & VPD <= VPDmax) + 1 * (VPD < VPDmin)
  # f_VPD = min(f_VPD, 1);

  # 3. exponential
  # f_VPD = exp(-D0 * VPD.^2);
  if (V2) {
    # fT2  = exp(0.088*(Tavg-25)) /(1 +exp(0.29*(Tavg-41)));
    fT2 <- exp(0.031 * (Tavg - 25)) / (1 + exp(0.115 * (Tavg - 41))) # Vm
    fT2 <- pmin(fT2, 1)
    Am <- Am_25 * fT2
    # Am   = Am_25;

    # dim <- dim(Am)
    nrow <- nrow(Am)
    if (is.null(nrow)) nrow <- length(Am)

    if (length(Ca) != 1 && nrow != length(Ca)) {
      Ca <- pracma::repmat(Ca, nrow, 1) # 2d array, [ngrid, ntime]
    }

    P1 <- Am * Alpha * Thelta * PAR_mol
    P2 <- Am * Alpha * PAR_mol
    P3 <- Am * Thelta * Ca
    P4 <- Alpha * Thelta * PAR_mol * Ca
    ## canopy conductance in (mol m-2 s-1)
    Ags <- Ca * P1 / (P2 * kQ + P4 * kQ) * (kQ * LAI + log((P2 + P3 + P4) / (P2 + P3 * exp(kQ * LAI) + P4))) # umol m-1 s-1
    Ag <- Ags  # gross assimilation rate in umol m-2 s-1, # update20180830,
    Ag <- Ag * f_VPD # * data$dhour_norm^2      # constrained by f_VPD;

    Ag[Am == 0] <- 0
    # Rd   = 0.015 * Am;               # day time respiration rate umol m-2 s-1
    # An   = Ag - Rd;                  # net assimilation rate in umol m-2 s-1
    GPP <- Ag * 60 * 60 / 10^6 * 24 * 12 # [umol m-2 s-1] to [g C m-2 d-1]
    Gc <- m / Ca * Ag * 1.6 * f_VPD_gc # 1.6 = conductance of water / conductance of CO2 (mol m-2 s-1)
    ## Convert from mol m-2 s-1 to cm s-1 to m s-1
    Gc <- Gc * 1e-2 / (0.446 * (273 / (273 + Tavg)) * (Pa / 101.3)) # unit convert to m s-1
  } else {
    # Gc  = gsx /kQ *log((PAR+Q50) /(PAR *exp(-kQ  * LAI)+Q50))  *f_VPD_gc;
    # GPP = NA(size(Gc));
  }
  Gc <- clamp_min(Gc, 10^-6)

  ## 4. Vegetation Evaportranspiration
  # making sure vegetation transpiration is negaligable, this is very important for very dry Sahara
  # available energy A partitioning using Tou
  # Tou(find(LAI<=0.001))= 1;
  Tou <- exp(-kA * LAI)

  d   <- 0.64 * hc
  zom <- 0.13 * hc
  zoh <- 0.10 * zom
  # Aerodynamic conductance (Leuning, 2008, Eq.13, doi:10.1029/2007WR006562)
  uz <- log(67.8 * Zob - 5.42) / 4.87 * u2 # convert from u2 to uz
  Ga <- uz * kmar^2 / (log((Zob - d) / zom) * log((Zob - d) / zoh)) # m s-1

  # Transpiration from plant cause by radiation water transfer
  LEcr <- epsilon * Rn * (1 - Tou) / (epsilon + 1 + Ga / Gc) # W m-2
  # Transpiration from plant cause by aerodynamic water transfer
  LEca <- (rou_a * Cp * Ga * VPD / gama) / (epsilon + 1 + Ga / Gc) # W m-2

  Ecr  <- LEcr / lambda * 86400 * 10^-3 # [W m-2] change to [mm d-1]
  Eca  <- LEca / lambda * 86400 * 10^-3 # [W m-2] change to [mm d-1]
  Ec   <- Ecr + Eca

  ## 5. Intercepted Evaporation (Ei)
  # van Dijk, A.I.J.M, 2001, Eq2.
  fveg <- 1 - exp(-LAI / LAIref)  # Canopy cover fraction, Eq.1
  Sveg <- S_sls * LAI # Specific leaf storage, Eq.2

  # factor      =   0.5; # E_bar / R_bar
  fER <- fER0 * fveg # the value of 0.50 based on optimisation at Australian catchments
  Pwet <- -log(1 - fER0) / fER0 * Sveg / fveg # -log(1 - fER /fveg),
  Pwet[is.na(Pwet)] <- 0
  Ei <- (Prcp < Pwet) * fveg * Prcp +
    (Prcp >= Pwet) * (fveg * Pwet + fER * (Prcp - Pwet))
  # Ei[LAI == 0] = 0
  # Ei[is.na(Prcp]
  # (P < Pwet) * fveg * P + (P >= Pwet) * ( fveg*Pwet + fER*(P - Pwet) )

  ## 6. Soil Evaporation (Es)
  Es_eq <- Eeq * Tou # Soil evaporation at equilibrium, mm d-1
  Pi <- Prcp - Ei

  out_FUN <- if (scale == "site") data.table else listk
  if (return.Es) {
    fval_soil <- data$fval_soil
    if (is.null(fval_soil)) {
      fval_soil <- add_fvalSoil(Pi, Es_eq, frame = 3, data$site) # 0-1 constrain
    }
    Es <- fval_soil * Es_eq # actual latent heat of soil in mm d-1
    ET <- Ec + Ei + Es
    out <- out_FUN(GPP, ET, Es, Ec, Ei, Ecr, Eca, Ga, Gc, Pi, Es_eq)
  } else {
    out <- out_FUN(GPP, Ec, Ei, Ecr, Eca, Ga, Gc, Pi, Es_eq)
  }

  if (length(ratio) > 2) out %<>% map(multiply_by, ratio)
  out
}

# from Water, Carbon, and Nutrient Cycling in the Soil-Plant Atmosphere, in
# the Handbook of Groundwater Engineering, Second Edition (Page 29-7).

# https://onlineconversion.vbulletin.net/forum/main-forums/convert-and-calculate/8797-convert-soil-respiration-rates-in-umol-co2-m-2-s-1-to-g-co2-m-2-h-1


## Units
# ------------------------------------------------------------------------------
# - `1 mm day-1`       : 2.45 `MJ m-2 day-1`
# - `1 J cm-2 day-1`   : 0.01 `MJ m-2 day-1`
# - `1 cal`            : 4.1868 J = 4.1868 1e-6 `MJ`
# - `1 cal cm-2 day-1` : 4.1868 1e-2 `MJ m-2 day-1`
# - `1 W m-2`          : 0.0864 `MJ m-2 d-1`

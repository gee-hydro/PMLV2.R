bands <- c("LAI", "emiss", "albedo", 'qc', "Tmax", "Tmin", "Tavg", "q", "Rln", "Rs", "Pa", "U2", "Prcp", 'CO2')
nband <- length(bands)

#' tidy_forcing_yearly
#' @param l_static NULL
tidy_forcing_yearly <- function(file, l_static = NULL,
    write2nc = FALSE, overwrite = FALSE)
{
    x <- readGDAL(file, silent = TRUE)
    grps  = 1:46 %>% set_names(1:46)

    lst <- lapply(grps, function(i) {
        ind <- seq((i - 1)*nband+1, i*nband)
        data = x@data[, ind] %>% set_names(bands) %>% .[, -c(4, 14)] # rm `qc` and `CO2`
        # replace static bands if provided
        if (!is.null(l_static)) {
            data = cbind(l_static[[i]][, 1:3], data[, -(1:3)])
        }
        data
    })

    if (write2nc) {
        outfile <- gsub(".tif$", ".nc", file)

        if (!file.exists(outfile) || overwrite) {
            range <- tif_range(file)
            lst   <- purrr::transpose(lst) %>% map(~do.call(cbind, .) %>% spdata_array(range))
            year  <- str_extract(basename(file), "\\d{4}") %>% as.numeric()
            dates <- get_modis_date(year)

            # browser()
            fprintf("writing %s ...\n", basename(outfile))
            ncwrite(lst, outfile, compression = 1, range = range, dates = dates)
        }
    } else {
        lst
    }
}

trans_list <- function(l) {
    purrr::transpose(l) %>% map(~do.call(cbind, .))
}

tif_range <- function(file) {
    info <- rgdal::GDALinfo(file, returnStats=FALSE)[1:7] %>% as.list()
    lon_range <- with(info, c(ll.x, ll.x + columns*res.x))
    lat_range <- with(info, c(ll.y, ll.y + rows*res.y))
    c(lon_range, lat_range)
}


PML_yearly <- function(file_forcing, LC, l_static = NULL, Year,
    is_save = TRUE, overwrite = TRUE, outdir = "OUTPUT/static",
    prefix = "PML_V2-RAW-static-8day-TP_010deg")
{
    # file = files_forcing[j]
    outfile <- sprintf("%s/%s-%d.RDS", outdir, prefix, Year)
    if (file.exists(outfile) && !overwrite) return()

    # note here ------------------------------------------------------------
    l_forcing <- tidy_forcing_yearly(file_forcing, l_static)
    CO2s  <- d_CO2[year == Year, CO2]
    dates <- d_CO2[year == Year, date]

    grps  = set_names(seq_along(dates), dates)
    r_yearly <- lapply(grps, function(i) {
        runningId(i, 5, prefix = sprintf("year = %s", Year))
        data = l_forcing[[i]]
        # d_sim = PML(data, param, CO2 = CO2, ratio = lc)
        r <- PML_yearly_LargeGrid(data, LC, CO2s[i])
        # browser()
        r
    })

    if (is_save) {
        check_dir(outdir)
        if (!file.exists(outfile) || overwrite) saveRDS(r_yearly, file = outfile)
        invisible()
    } else {
        r_yearly
    }
}

#' PML V2 dynamic and static experiments
#'
#' @import data.table
#' @export
PML_years <- function(files_forcing, lst_LC_ratio,
                      type = c("dynamic", "static")[2],
                      is_save = TRUE, overwrite = TRUE, outdir = "OUTPUT/static",
                      prefix = "PML_V2-RAW-static-8day-TP_010deg") {
    years <- names(files_forcing) %>% as.numeric()
    l_static <- NULL
    # The first year as static vegetation forcing
    l_static <- if (type == "static") tidy_forcing_yearly(files_forcing[1]) else NULL
    # Then replace: LAI, albedo and emiss
    LC <- lst_LC_ratio[[1]]

    lst_raw <- lapply(seq_along(files_forcing), function(j) {
        if (type == "dynamic") LC <- lst_LC_ratio[[j]]
        Year <- years[j]
        PML_yearly(
            files_forcing[j], LC, l_static, Year,
            is_save, overwrite, outdir, prefix
        )
    })
    lst_raw
}

nc_add_Es <- function(file, frame = 3, grid_type = "mat") {
    l <- ncread(file, c("Pi", "Es_eq"), ntime = -1, grid_type = grid_type)

    fval_soil <- add_fvalSoil(l$data$Pi, l$data$Es_eq, frame)
    Es <- fval_soil * l$data$Es_eq
    if (!is.null(grid_type)) {
        range <- nc_range(file)$range
        Es <- spdata_array(Es, range, flip = FALSE)
    }
    ncwrite(list(Es = Es), file,
        # dates = dates,
        verbose = TRUE, overwrite = FALSE)
}

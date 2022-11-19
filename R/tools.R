set_year_names <- . %>% set_names(., stringr::str_extract(basename(.), "\\d{4}"))

is_empty <- function(x) length(x) == 0

listk <- function(...) {
  # get variable names from input expressions
  cols <- as.list(substitute(list(...)))[-1]
  vars <- names(cols)
  Id_noname <- if (is.null(vars)) seq_along(cols) else which(vars == "")

  if (length(Id_noname) > 0) {
    vars[Id_noname] <- sapply(cols[Id_noname], deparse)
  }
  # ifelse(is.null(vars), Id_noname <- seq_along(cols), Id_noname <- which(vars == ""))
  x <- setNames(list(...), vars)
  return(x)
}

clamp_min <- function(x, value = 0) {
  x[x < value] <- value
  x
}

clamp <- function(x, lims = c(0, 1), fill.na = FALSE) {
  if (fill.na) {
    x[x < lims[1]] <- NA_real_
    x[x > lims[2]] <- NA_real_
  } else {
    x[x < lims[1]] <- lims[1]
    x[x > lims[2]] <- lims[2]
  }
  x
}

check_dir <- function(path) {
  for (path_i in unique(path)) {
    if (!dir.exists(path_i)) {
      dir.create(path_i, recursive = TRUE)
    }
  }
  path
}

# https://stackoverflow.com/questions/11641701/sum-a-list-of-matrices
list_sum <- function(l) {
  x <- l[[1]]
  # recursive function
  if (is.list(x) && !is.data.frame(x)) {
    return(lapply(l, list_sum))
  }
  lapply(seq_along(l)[-1], function(i) {
    x <<- x + l[[i]]
  })
  x
}


get_modis_date <- function(year, dn = 8) {
  nptperyear <- ceiling(366 / dn)
  year <- rep(year, each = nptperyear)
  days <- seq(1, 366, dn)
  sprintf("%d%03d", year, days) %>% as.Date("%Y%j")
}

fprintf <- function(fmt, ...) cat(sprintf(fmt, ...))

dir2 <- function(path = ".", pattern = NULL, ...) {
  dir(path, pattern, ..., full.names = TRUE)
}

is_leapyear <- function(year) {
  (year %% 4 == 0) & ((year %% 100 != 0) | (year %% 400 == 0))
}

summary_list <- function(l) {
  if (is.array(l)) {
    return(summary(as.numeric(l)))
  }
  lapply(l, function(.x) summary(as.numeric(.x)))
}

fix_neg <- function(x) {
  x[x < 0] <- 0
  return(x)
}

add_dn <- function(d, days = 8) {
  if (class(d$date)[1] != "Date") {
    d$date %<>% ymd()
  }

  d %<>% plyr::mutate(d, year = year(date), doy = yday(date))

  days <- floor(days)
  for (i in seq_along(days)) {
    day <- days[i]
    # d$d8  = ceiling(d$doy/8)
    eval(parse(text = sprintf("d$d%d <- ceiling(d$doy/%d)", day, day)))
  }
  return(d)
}

#' @importFrom purrr map
trans_list <- function(l) {
  purrr::transpose(l) %>% map(~ do.call(cbind, .))
}

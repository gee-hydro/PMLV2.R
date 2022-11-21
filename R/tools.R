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

#' @importFrom dplyr mutate
#' @importFrom lubridate ymd yday
add_dn <- function(d, days = 8) {
  if (class(d$date)[1] != "Date") {
    d$date %<>% ymd()
  }

  d %<>% mutate(d, year = year(date), doy = yday(date))

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

dt_round <- function(d, digits = 2) {
  mutate(d, across(where(is.double), ~ round(.x, digits)))
}

melt_list <- function(list, ..., na.rm = TRUE) {
  list <- rm_empty(list)
  if (is.null(list) || length(list) == 0) {
    return(NULL)
  }
  n <- length(list)
  params <- list(...)
  key <- names(params)[1]
  vals <- params[[1]]
  if (is.null(key)) {
    key <- vals
    vals <- names(list)
  }
  if (is.null(vals)) {
    vals <- seq_along(list)
  }
  if (length(vals) == 1) {
    vals <- rep(vals, n)
  }
  if (is.character(vals)) {
    vals %<>% as.factor()
  }
  first <- list[[1]]
  if (is.data.frame(first)) {
    for (i in seq_along(list)) {
      x <- list[[i]]
      eval(parse(text = sprintf("x$%s <- vals[i]", key)))
      list[[i]] <- x
    }
    res <- do.call(rbind, list) %>% data.table()
  }
  res %>% dplyr::relocate(key)
}

runningId <- function(i, step = 1, N, prefix = "") {
  perc <- ifelse(missing(N), "", sprintf(", %.1f%% ", i / N *
    100))
  if (mod(i, step) == 0) {
    cat(sprintf(
      "[%s] running%s %d ...\n", prefix, perc,
      i
    ))
  }
}

rm_empty <- function(x) {
  if (is.list(x)) {
    x[!sapply(x, is_empty)]
  } else {
    x[!is.na(x)]
  }
}

which.notna <- function(x) {
  which(!is.na(x))
}

# ---- request layer -------------------------------------------------------------
# Options (all optional):
#   musicbrainz.base_url      web service root, default "https://musicbrainz.org/ws/2";
#                             e.g. "http://localhost:5000/ws/2" for a local mirror
#   musicbrainz.min_interval  seconds between two request starts; default 1.6 for
#                             musicbrainz.org (its rate limit), 0 for any other host
#   musicbrainz.user_agent    User-Agent header (musicbrainz.org asks for a contact)
#   musicbrainz.timeout       seconds per request, default 60
#   musicbrainz.retry_waits   waits (s) before retrying temporary errors (timeouts, 429, 5xx),
#                             default c(2, 10, 30, 120, 600); random jitter +0-25 %
#   musicbrainz.cache         TRUE (default): identical URLs are fetched once per session
# Permanent errors (400, 404, ...) return NULL as before. Temporary errors that persist
# after all retries raise an error of class "musicbrainz_unavailable" instead of NULL, so
# that an outage is never recorded as "no match".

mb_option <- function(name, default) getOption(paste0("musicbrainz.", name), default)

mb_base_url <- function() sub("/+$", "", mb_option("base_url", "https://musicbrainz.org/ws/2"))

mb_min_interval <- function() {
  x <- getOption("musicbrainz.min_interval")
  if (!is.null(x)) return(as.numeric(x))
  host <- httr::parse_url(mb_base_url())$hostname
  if (!is.null(host) && grepl("(^|\\.)musicbrainz\\.org$", host)) 1.6 else 0
}

.mb_state <- new.env(parent = emptyenv())
.mb_state$last <- 0
.mb_state$cache <- new.env(hash = TRUE, parent = emptyenv())

#' @importFrom httr GET add_headers timeout
httr_get <- function(url) {
  wait <- .mb_state$last + mb_min_interval() - as.numeric(Sys.time())
  if (wait > 0) Sys.sleep(wait)
  .mb_state$last <- as.numeric(Sys.time())
  httr::GET(
    url,
    httr::add_headers(
      Accept = "application/json",
      "user-agent" = mb_option("user_agent", "musicbrainz R package (fork github.com/markusradke/musicbrainz)")
    ),
    httr::timeout(mb_option("timeout", 60))
  )
}

#' @importFrom httr status_code content
#' @importFrom stats runif
.GET_data <- function(url, verbose = TRUE) { # nolint
  waits <- mb_option("retry_waits", c(2, 10, 30, 120, 600))
  last_problem <- NA_character_
  for (attempt in seq_len(length(waits) + 1)) {
    resp <- tryCatch(httr_get(url), error = function(e) e)
    status <- if (inherits(resp, "error")) NA_integer_ else httr::status_code(resp)
    if (identical(status, 200L)) return(httr::content(resp, type = "application/json"))
    temporary <- is.na(status) || status %in% c(429L, 500L, 502L, 503L, 504L)
    if (!temporary) {
      if (verbose) message(paste("http error code:", status))
      return(NULL)
    }
    last_problem <- if (is.na(status)) conditionMessage(resp) else paste("http error code", status)
    if (attempt > length(waits)) break
    w <- waits[attempt] * stats::runif(1, 1, 1.25)
    if (verbose) message(sprintf("%s; retry %d/%d in %.0f s", last_problem, attempt, length(waits), w))
    Sys.sleep(w)
  }
  rlang::abort(paste0("MusicBrainz unavailable (", last_problem, ") for ", url),
               class = "musicbrainz_unavailable")
}

get_data <- function(url, verbose = TRUE) {
  if (!isTRUE(mb_option("cache", TRUE))) return(.GET_data(url, verbose))
  if (exists(url, envir = .mb_state$cache, inherits = FALSE)) return(get(url, envir = .mb_state$cache))
  res <- .GET_data(url, verbose)
  assign(url, res, envir = .mb_state$cache)
  res
}

#' @importFrom httr build_url parse_url
#' @importFrom utils URLencode
lookup_by_id <- function(resource, mbid, includes) {
  # lookup:   /<ENTITY>/<MBID>?inc=<INC>
  # API request function for lookup
  base_url <- mb_base_url()
  url <- base::paste(c(base_url, resource, mbid), collapse = "/")
  url <- utils::URLencode(url)

  if (!is.null(includes) && length(includes)) {
    parsed_url <- httr::parse_url(url)
    parsed_url$query <- base::list(inc = paste0(includes, collapse = "+"))
    url <- httr::build_url(parsed_url)
  }

  get_data(url)
}

#' @importFrom httr build_url
search_by_query <- function(type, query, limit, offset) {
  # API request function for search
  # search:   /<ENTITY>?query=<QUERY>&limit=<LIMIT>&offset=<OFFSET>
  base_url <- mb_base_url()
  url <- base::paste(c(base_url, type), collapse = "/")

  parsed_url <- httr::parse_url(url)
  parsed_url$query <- base::list(query = query, limit = limit, offset = offset)

  url <- httr::build_url(parsed_url)

  get_data(url)
}


#' @importFrom httr build_url
#' @importFrom stats setNames
#' @importFrom utils URLencode
browse_by_lnkd_id <- function(resource, lnk_resource, mbid, includes, limit, offset) {
  # API request function for search
  # browse:   /<ENTITY>?<ENTITY>=<MBID>&limit=<LIMIT>&offset=<OFFSET>&inc=<INC>
  base_url <- mb_base_url()
  url <- base::paste(c(base_url, resource), collapse = "/")
  url <- utils::URLencode(url)
  parsed_url <- httr::parse_url(url)
  parsed_url$query <- stats::setNames(base::list(mbid), nm = lnk_resource)
  parsed_url$query <- base::append(parsed_url$query, list(limit = limit, offset = offset))

  if (!is.null(includes) && length(includes)) {
    parsed_url$query <- base::append(parsed_url$query, list(inc = paste0(includes, collapse = "+")))
  }

  url <- httr::build_url(parsed_url)

  get_data(url)
}

#' Tidy eval helpers
#'
#' These functions provide tidy eval-compatible ways to capture
#' symbols (`sym()`, `syms()`, `ensym()`), expressions (`expr()`,
#' `exprs()`, `enexpr()`), and quosures (`quo()`, `quos()`, `enquo()`).
#' To learn more about tidy eval and how to use these tools, read
#' <http://rlang.tidyverse.org/articles/tidy-evaluation.html>
#'
#' @name tidyeval
#' @keywords internal
#' @aliases          quo quos enquo sym syms ensym expr exprs enexpr quo_name
#' @importFrom rlang quo quos enquo sym syms ensym expr exprs enexpr quo_name
#' @export           quo quos enquo sym syms ensym expr exprs enexpr quo_name
#' @importFrom rlang UQ UQS .data :=
NULL

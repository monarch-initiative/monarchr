# Make a request to the Monarch web API, retrying once after a short pause if
# the server returns a transient gateway error (502, 503, 504). The API can be
# briefly overloaded, so we keep this gentle: one retry, ~1-2 seconds of wait.
# Any other non-200 response is an error.
monarch_api_request <- function(verb, url, ...) {
    response <- httr::RETRY(verb, url, ...,
        times = 2,
        pause_base = 1,
        pause_cap = 2,
        pause_min = 1,
        terminate_on = setdiff(400:599, 502:504),
        quiet = TRUE
    )

    if (response$status_code != 200) {
        stop(response$status_code, " ", httr::http_status(response$status_code)$message)
    }

    response
}

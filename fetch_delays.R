# This script fetches all current traffic delays
# In detail:
# - List motorways via GET /.
# - Pull warnings per motorway and clean


# load libraries----
library(httr)
library(jsonlite)
library(dplyr)
library(purrr)
library(tibble)
library(stringr)

# fetch motorways----
base_url <- "https://verkehr.autobahn.de/o/autobahn"
roads_r   <- fromJSON(content(GET(base_url, query = list()), "text"))$roads 
# roads is a character vector like c("A1","A2",…)

# clean roads: remove spaces and then duplicates
roads <- roads_r %>% str_squish() %>% unique()

# get current traffic delays----
# helper to fetch and decode all delays on one motorway
fetch_delays <- function(roadId) {
    
    message(sprintf("fetching data for %s", roadId))
    
    # pull warnings list
    url_w    <- sprintf("%s/%s/services/warning", base_url, roadId)
    warn_dat <- fromJSON(content(GET(url_w), "text"), flatten = TRUE)$warning
    
    n_items <- if (is.data.frame(warn_dat))
    {
        nrow(warn_dat)
    } else {
        length(warn_dat)
    }
    
    if (n_items == 0) {
        return(NULL)
    }
    
    # for each delay, fetch its detail record
    details <- lapply(warn_dat$id, function(b64_id) {
        url_d <- sprintf("%s/details/warning/%s", base_url, b64_id)
        fromJSON(content(GET(url_d), "text"), flatten = TRUE)
    })
    
    # helper to provide defaults:
    null_or <- function(a, b) if (is.null(a)) b else a
    
    # clean details and parse into df
    details_tbl <-  map_dfr(details, ~ {
      tibble(
        road                = roadId,
        identifier          = null_or(.x$identifier          , NA_character_),
        extent              = null_or(.x$extent              , NA_character_),
        point               = null_or(.x$point               , NA_character_),
        display_type        = null_or(.x$display_type        , NA_character_),
        title               = null_or(.x$title               , NA_character_),
        subtitle            = null_or(.x$subtitle            , NA_character_),
        delay_minutes       = null_or(.x$delayTimeValue      , null_or(.x$delayInMinutes, NA_real_)) %>% as.numeric(),
        abnormalTrafficType = null_or(.x$abnormalTrafficType , NA_character_),
        averageSpeed        = null_or(.x$averageSpeed        , NA_character_),
        startTimestamp      = .x$startTimestamp,
        description         = null_or(paste(.x$description, collapse=" ", recycle0=T), NA_character_),
        latitude            = null_or(.x$coordinate$lat      , NA_real_) %>% as.numeric(),
        longitude           = null_or(.x$coordinate$long     , NA_real_) %>% as.numeric(),
        #geometry_raw        = null_or(list(.x$geometry), list(NULL))
      ) 
    })
    
    return(details_tbl)
}

# iterate over all roads (or subset if you only care about A-roads)
all_delays <- roads %>%
    map(fetch_delays) %>%
    discard(is.null) %>%
    bind_rows()

# clean data frame----
all_delays <- all_delays %>% 
    mutate(startTimestamp = as.Date(startTimestamp))

# write timestamped CSV to a fixed path----
out_dir <- "data"
if (!dir.exists(out_dir)) dir.create(out_dir)
out_file <- file.path(out_dir, paste0("delays_", format(Sys.time(), tz="Europe/Berlin", "%Y%m%d-%H%M"), ".csv"))
write.csv(all_delays, out_file, row.names = FALSE)
message("Wrote: ", out_file)

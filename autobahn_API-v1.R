# This script does the following:
# - List motorways via GET /.
# - Pull warnings per motorway
# - Base64-encode each jam’s identifier to unlock its detail record (including delayInMinutes and exact coordinate).
# - Assemble all jams into an sf data frame.
# - Plot over Germany’s outline using a Viridis green-to-red ramp (clamped at 240 min).
# - Run the full script once, and you’ll get a static map of all current jams. Next, when you’re happy with this, we can automate pulling & saving every 15 minutes. Let me know!


# load libraries----
library(httr)
library(jsonlite)
library(base64enc)
library(sf)
library(ggplot2)
library(rnaturalearth)
library(viridis)
library(osmdata)

# fetch motorways----
base_url <- "https://verkehr.autobahn.de/o/autobahn"
roads_r   <- fromJSON(content(GET(base_url, query = list()), "text"))$roads 
# roads is a character vector like c("A1","A2",…)

# clean roads: remove spaces and then duplicates
roads <- roads_r %>% str_squish() %>% unique()

# get current traffic delays----
# helper to fetch and decode all delays on one motorway
fetch_jams <- function(roadId) {
    
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
    
    # for each jam, fetch its detail record
    details <- lapply(warn_dat$id, function(b64_id) {
        url_d <- sprintf("%s/details/warning/%s", base_url, b64_id)
        fromJSON(content(GET(url_d), "text"), flatten = TRUE)
    })
    
    # helper to provide defaults:
    `%||%` <- function(a, b) if (is.null(a)) b else a
    
    # clean details and parse into df
    details_tbl <-  map_dfr(details, ~ {
        tibble(
            road          = roadId,
            identifier    = .x$identifier %||% NA_character_,
            extent        = .x$extent %||% NA_character_,
            point         = .x$point %||% NA_character_,
            display_type  = .x$display_type %||% NA_character_,
            title         = .x$title %||% NA_character_,
            subtitle      = .x$subtitle %||% NA_character_,
            delay_minutes = as.numeric(.x$delayTimeValue %||% .x$delayInMinutes %||% NA_real_),
            abnormalTrafficType = .x$abnormalTrafficType %||% NA_character_,
            averageSpeed  = .x$averageSpeed %||% NA_character_,
            startTimestamp = .x$startTimestamp,
            description   = paste(.x$description, collapse=" ", recycle0=T) %||% NA_character_,
            latitude      = as.numeric(.x$coordinate$lat   %||% NA_real_),
            longitude     = as.numeric(.x$coordinate$long  %||% NA_real_),
            #geometry_raw  = list(.x$geometry %||% list(NULL))
        )
    })
    
    return(details_tbl)
}

# iterate over all roads (or subset if you only care about A-roads)
all_jams <- roads %>%
    map(fetch_jams) %>%
    discard(is.null) %>%
    bind_rows()

# Prepare spatial data frame----
all_jams <- all_jams %>% 
    mutate(startTimestamp = as.Date(startTimestamp))

# convert coordinates to sf points
all_jams_sf <- st_as_sf(all_jams, coords = c("longitude","latitude"), crs = 4326)

# Plotting----

FETCH_MOTOR_WAYS_FROM_SCRATCH <- F # takes ca 2min
SAVE_MOTORWAYS_TO_RDS <- F # if fetching motorways, do you want to save them?

if(FETCH_MOTOR_WAYS_FROM_SCRATCH){
    # Fetch OSM motorways + trunks (for background of highways)
    germany_bb <- getbb("Germany")
    osm_res <- opq(bbox    = germany_bb, timeout = 180) %>%        # allow 3 minutes
        add_osm_feature(key   = "highway", value = c("motorway", "trunk")) %>%
        osmdata_sf()
    
    motorway_sf <- osm_res$osm_lines
    
    if(SAVE_MOTORWAYS_TO_RDS){
        motorway_sf %>% saveRDS('./exercises_data/autobahn_exercise-motorway_sf.RDS')
    }
    
} else {
    motorway_sf <- readRDS("./exercises_data/autobahn_exercise-motorway_sf.RDS")
}

# ensure both are in the same CRS
motorway_sf_transformed <- st_transform(motorway_sf, st_crs(germany))

# clip to Germany’s polygon
motorway_de <- st_intersection(motorway_sf_transformed, germany)

# get outline of Germany
germany <- ne_countries(country = "Germany", scale = "medium", returnclass = "sf")

ggplot() +
    geom_sf(data = germany, fill = "grey95", color = NA) +
    geom_sf(data = motorway_de, color = "#00f", size = 0.3, alpha=.1) +
    #geom_sf(data = motorway_sf, color = "#00f", size = 0.3, alpha=.1) + # use this line for motorways to not end at the borders
    geom_sf(data = all_jams_sf, aes(color = pmin(delay_minutes, 240)), size = .25) +
    scale_color_viridis_c(name = "Delay (min)", limits = c(0,240), direction = -1) +
    labs(
        title    = "Autobahn Network & Current Traffic Jams",
        subtitle = paste0("pulled at ", format(Sys.time(), "%Y-%m-%d %H:%M")),
        caption  = "Sources: Highways: OSM | Jams: Autobahn API"
    ) +
    theme_minimal()

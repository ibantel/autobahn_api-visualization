library(rnaturalearth)
library(sf)
library(tidyverse)

# load data ----
delay_data_files <- list.files("./data", pattern="*.csv", full.names=TRUE)

delay_data_r <- delay_data_files %>% 
  lapply(., read_csv) %>% 
  bind_rows()

delay_data <- delay_data_r %>%
    distinct(identifier, .keep_all = TRUE) %>%
    arrange(identifier)

delay_data %>% 
  write_csv("./delays_20250519-1128_delays_20250531-1627_distinct")

# prepare data ----
delay_data_filtered_by_length <- delay_data %>% 
  filter(delay_minutes > 5) %>% 
  mutate(day_of_week = weekdays(startTimestamp, abbreviate=T),
         weekend_day = ifelse(day_of_week %in% c("Sat", "Sun"), "Sat/Sun", "Weekday"),
         weekend_day = fct_relevel(weekend_day, "Weekday", "Sat/Sun"))

# prepare plotting ----

# convert coordinates to sf points
delays_sf <- st_as_sf(delay_data_filtered_by_length, coords = c("longitude","latitude"), crs = 4326)

FETCH_MOTOR_WAYS_FROM_SCRATCH <- F # takes ca 2min / if fetching motorways, save them

if(FETCH_MOTOR_WAYS_FROM_SCRATCH){
    # Fetch OSM motorways + trunks (for background of highways)
    germany_bb <- getbb("Germany")
    osm_res <- opq(bbox    = germany_bb, timeout = 180) %>%        # allow 3 minutes
        add_osm_feature(key   = "highway", value = c("motorway", "trunk")) %>%
        osmdata_sf()
    
    motorway_sf <- osm_res$osm_lines
    
    motorway_sf %>% saveRDS('./motorways_raw-data/motorways_sf.RDS')
} else {
    motorway_sf <- readRDS("./motorways_raw-data/motorways_sf.RDS")
}

# get outline of Germany
germany <- ne_countries(country = "Germany", scale = "medium", returnclass = "sf")

# ensure both are in the same CRS
motorway_sf_transformed <- st_transform(motorway_sf, st_crs(germany))

# clip to Germany’s polygon
motorway_de <- st_intersection(motorway_sf_transformed, germany)

# create plot ----
(
    plot_autobahn_delays <- ggplot(data=delays_sf) +
        geom_sf(data = germany, fill = "grey95", color = NA) +
        geom_sf(data = motorway_de, color = "#00f", size = 0.3, alpha=.05) +
        #geom_sf(data = motorway_sf, color = "#00f", size = 0.3, alpha=.1) + # use this line for motorways to not end at the borders
        geom_sf(data = delays_sf, aes(color = pmin(delay_minutes, 240)), size = .6) +
        scale_color_gradient(name = "Delay (min)", low = "green", high = "red", limits = c(0, 30)) +
        labs(title    = "Autobahn Network & Current Traffic Delays",
             subtitle = paste0("Between ", delays_sf$startTimestamp %>% min() %>% format(., "%d %B %Y"),
                               " and ", delays_sf$startTimestamp %>% max() %>% format(., "%d %B %Y")),
             caption  = paste0("Based on ", formatC(nrow(delays_sf), big.mark = ",", decimal.mark = "."),
                               " delays on German motorways ",
                               "exceeding ", delays_sf$delay_minutes %>% min()-1, " min delay.\n\n",
                               "Sources: Highways: OSM | Delays: Autobahn API")
             ) +
      facet_wrap(~weekend_day) + 
      theme_minimal() +
      theme(axis.ticks = element_blank(),
            axis.title = element_blank(),
            axis.text = element_blank())
)



ggsave(plot=plot_autobahn_delays, filename=paste0("./plots_out/", 
                                                  format(Sys.time(), "%Y%m%d-%H%M"),
                                                  "_autobahnplot.pdf"))


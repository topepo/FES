library(tidyverse)

if(Sys.getenv("LOGNAME") == "max"){  
  db_path <- "~/Dropbox/FeatureArt/"
  gh_path <- "~/github/"
  c_path <- "~/Dropbox/FeatureArt/caches/eda/"
} else if(Sys.getenv("LOGNAME") == "macuser"){
  db_path <- "~/Dropbox/FeatureArt/"
  gh_path <- "~/Documents/GitHub/"
  c_path <- "~/Dropbox/FeatureArt/caches/eda/"
} else {
  db_path <- "V:/FeatureArt/"
  gh_path <- "C:/Users/Kjell/Documents/GitHub/"
  c_path <- "V:/FeatureArt/caches/intro/windows/"
}

stations_new <-  
  read_csv(file.path(gh_path, "FES", "Data_Sets", "Chicago_trains", "cta_L_stops_rls_2018.csv")) %>%
  # Remove duplicate rows
  group_by(MAP_ID) %>%  
  slice(1) %>%  
  ungroup() %>%  
  mutate(
    Location  = gsub("[\\)\\(]", "", Location),
    LAT = str_split(Location, pattern = ",") %>% map_chr(pluck, 1) %>% as.numeric(),
    LON = str_split(Location, pattern = ",") %>% map_chr(pluck, 2) %>% as.numeric()
  ) %>%  
  select(PARENT_STOP_ID = MAP_ID, STATION_NAME, STATION_DESCRIPTIVE_NAME, LON, LAT)

stops <- unique(stations_new$PARENT_STOP_ID)

stations_old <-  
  read_csv(file.path(gh_path, "FES", "Data_Sets", "Chicago_trains", "cta_L_stops_rls.csv")) %>%  
  # Remove duplicate rows
  group_by(PARENT_STOP_ID) %>%  
  slice(1) %>%  
  select(PARENT_STOP_ID, STATION_NAME, STATION_DESCRIPTIVE_NAME, LON, LAT) %>%  
  filter(!(PARENT_STOP_ID %in% stops))

stations <-  
  bind_rows(stations_new, stations_old) %>%  
  distinct(PARENT_STOP_ID, STATION_NAME, STATION_DESCRIPTIVE_NAME, LON, LAT) %>%  
  mutate(PARENT_STOP_ID = paste0("s_", PARENT_STOP_ID)) %>%  
  select(station_id = PARENT_STOP_ID, name = STATION_NAME,  
         description = STATION_DESCRIPTIVE_NAME, lon = LON, lat = LAT)  

save(stations, file=file.path(gh_path, "FES", "Data_Sets", "Chicago_trains", "stations.RData"))

#stops_2018 <- read_csv(
#  file.path(gh_path, "data", "Chicago", "cta_L_stops_rls_2018.csv")) %>%
#  rename(PARENT_STOP_ID = MAP_ID) %>%
#  mutate(ADA = as.numeric(ADA),
#         Red = as.numeric(RED),
#         Blue = as.numeric(BLUE),
#         G = as.numeric(G),
#         Brn = as.numeric(BRN),
#         P = as.numeric(P),
#         Pexp = as.numeric(Pexp),
#         Y = as.numeric(Y),
#         Pink = as.numeric(Pnk),
#         Org = as.numeric(O)
#         ) %>%
#  separate(Location, c("LAT","LON"), sep = ',') %>%
#  mutate(LAT = as.numeric(gsub("\\(", "", LAT)),
#         LON = as.numeric(gsub("\\)", "", LON))) %>%
#  dplyr::select(STOP_ID, DIRECTION_ID, STOP_NAME, LON, LAT, STATION_NAME, STATION_DESCRIPTIVE_NAME,
#         PARENT_STOP_ID, ADA, Red, Blue, Brn, G, P, Pexp, Y, Pink, Org)
#write_csv(stops_2018, file.path(gh_path, "data", "Chicago", "cta_L_stops_rls_new.csv"))
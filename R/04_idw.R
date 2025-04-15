# packages ---------------------------

lop <- c("data.table", "ggplot2", "readxl",
         "sf", "RCzechia", "gstat",
         "leaflet")

to_instal <- lop[which(x = !(lop %in% installed.packages()[,"Package"]))]

if(length(to_instal) != 0) {
  
  install.packages(to_instal)
}

temp <- lapply(X = lop, 
               FUN = library, 
               character.only = T)

rm(lop, to_instal, temp)

# import dat ---------------------------

unzip(zipfile = "./data/proj_4/data_GIS.zip",
      exdir = "./data/proj_4/")

dta_stations <- st_read(dsn = "./data/proj_4/stations.shp")

ggplot() +
  geom_sf(data = dta_stations,
          colour = "red4") +
  theme_light()

ggplot() +
  geom_sf(data = kraje(resolution = "low"),
          fill = NA) +
  geom_sf(data = dta_stations,
          colour = "red4") +
  theme_light()

dta_pr <- read_excel(path = "./data/proj_4/Data.xlsx", 
                     skip = 2)

dta_pr <- as.data.table(x = dta_pr)
names(x = dta_pr)[1] <- "date"

dta_pr[, date := as.IDate(x = date)]

# preproces & idw ---------------------------

station_dist <- as.data.table(x = st_distance(x = dta_stations))

nms <- c("Borová Lada", "Churáňov", "Filipova Huť", "Kubova Huť", "Kvilda", 
         "Prášily", "Srní", "Strážný", "Zwieslerwaldhaus", 
         "Sankt Oswald - Riedlhutte", "Spiegelau - Althutte", 
         "Mauth", "Zwiesel - Rabenstein", "Frauenau")

ids <- c("C1BLAD01", "C1CHUR01", "C1FILH01", "C1KHUT01", "C1KVIL01", 
         "C1PRAS01", "C1SRNI01", "C1STRZ01", "ZWIES", "STOSW", 
         "SPIEG", "MAUTH", "RABEN", "FRAU")

nms_ids <- setNames(object = ids, 
                    nm = nms)

names(x = station_dist) <- nms_ids[dta_stations$stanice]

station_dist[, station := as.factor(x = nms_ids[dta_stations$stanice])]

station_dist





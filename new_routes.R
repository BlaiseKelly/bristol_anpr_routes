library(stringr)
library(osmdata)
library(lwgeom)
library(NISTunits)
#library(mapview)
library(tidyverse)
library(osrm)
library(sf)
library(leaflet)
library(leaflet.extras)
library(leaflet.opacity)
library(htmlwidgets)

select <- dplyr::select

##set the code for ukgrid and lat lon conversions
ukgrid <- "+init=epsg:27700"
latlong <- "+init=epsg:4326"

#dat <- st_read("https://opendata.bristol.gov.uk/explore/dataset/fact-journey-hourly/download/?format=geojson&disjunctive.journeylinkdescription=true&refine.journeylinkdescription=Bond+St+EB+%3E+M32+OB&q=rollupdatetime:%5B2020-03-31T22:00:00Z+TO+2021-05-17T21:59:59Z%5D&timezone=Europe/London&lang=en")
#datj <- st_read("https://opendata.bristol.gov.uk/explore/dataset/fact-journey-hourly/download/?format=json&disjunctive.journeylinkdescription=true&refine.journeylinkdescription=Bond+St+EB+%3E+M32+OB&q=rollupdatetime:%5B2020-03-31T22:00:00Z+TO+2021-05-17T21:59:59Z%5D&timezone=Europe/London&lang=en")
#dat_csv <- read.csv("https://opendata.bristol.gov.uk/explore/dataset/fact-journey-hourly/download/?format=csv&disjunctive.journeylinkdescription=true&refine.journeylinkdescription=Bond+St+EB+%3E+M32+OB&q=rollupdatetime:%5B2020-03-31T22:00:00Z+TO+2021-05-17T21:59:59Z%5D&timezone=Europe/London&lang=en&use_labels_for_header=true&csv_separator=%3B", sep = ";")

all_routes <- st_read("data/dim-journey-links.geojson")

#all_countz <- read.csv("data/fact-journey-hourly.csv", sep = ";")

start_points <- distinct(all_routes, journeystart, .keep_all = TRUE)
spz <- unique(all_routes$journeystart)

start_points <- list()
for (r in spz){

df <- filter(all_routes, journeystart == r)
all_rdz <- unique(df$sk_dim_journeylinkid)
point_angles <- list()
for (a in all_rdz){
  dt <- filter(df, sk_dim_journeylinkid == a)
  dt <- data.frame(st_coordinates(dt))
  dt <- dt[2:3,]
  dt <- st_as_sf(dt, coords = c("X", "Y"), crs = latlong)
  dt1 <- data.frame(a = dt$geometry[1], b = dt$geometry[2])

    dt1$anglez <- as.numeric(NISTradianTOdeg(st_geod_azimuth(c(dt1$geometry, dt1$geometry.1))))
  nam1 <- as.character(a)
  point_angles[[nam1]] <- dt1
}

all_route_angles <- do.call(rbind, point_angles)
avg_angle <- mean(all_route_angles$anglez)

df_coords <- data.frame(st_coordinates(df))
df <- st_transform(df, ukgrid)
##find second point from start of route by looping through each route
journiez <- unique(df$sk_dim_journeylinkid)
sfs <- list()
for (d in journiez){
  df2 <- filter(df, sk_dim_journeylinkid == d)
  second_from_start <- data.frame(st_coordinates(df2))[2,]
  sfs[[d]] <- second_from_start
}

#scs <-  st_line_sample(df, sample = 0)
scz <- do.call(rbind, sfs)

max_min_area <- data.frame(X = c(min(scz$X), max(scz$X), max(scz$X), min(scz$X)),
                            Y = c(max(scz$Y), max(scz$Y), min(scz$Y), min(scz$Y)))

modelled_area <- max_min_area %>% 
  st_as_sf(coords = c("X", "Y"), crs = ukgrid) %>%
  dplyr::summarise(data = st_combine(geometry)) %>% 
  st_cast("POLYGON") %>%
  st_buffer(3) %>% 
  st_transform(latlong)

x <- opq(bbox = modelled_area) %>% 
  add_osm_feature((key = c('highway'))) %>% 
  osmdata_sf()

rdz <- x$osm_lines
if(!is.null(rdz)){
  rdz <- rdz %>% 
    filter(highway %in% c("primary", "primary_link", "secondary", "trunk", "trunk_link", "tertiary", "motorway", "unclassified")) 
}

if(NROW(rdz)>0){
  rdz <- rdz %>% select(id = osm_id, geometry)
}

polyz <- x$osm_polygons 

if(NROW(polyz)>0 & !is_null(rdz)){
  
roundabouts <- filter(polyz, highway == "primary")

if(NROW(roundabouts)>0 ){
  
  roundabouts <- st_cast(roundabouts, "LINESTRING")
  rbz <- st_cast(roundabouts, "LINESTRING")
  rds <- st_coordinates(rbz)
  p1 <- data.frame(x1 = rds[1:NROW(rds)-1,1], y1 = rds[1:NROW(rds)-1,2])
  p1 <- st_as_sf(p1, coords = c("x1", "y1"), crs = latlong)                
  p2 <-  data.frame(x2 = rds[2:NROW(rds),1], y2 = rds[2:NROW(rds),2])
  p2 <- st_as_sf(p2, coords = c("x2", "y2"), crs = latlong)
  pf <- st_sfc(mapply(function(a,b){st_cast(st_union(a,b),"LINESTRING")}, p1$geometry, p2$geometry, SIMPLIFY=FALSE))
  roundabouts <- st_as_sf(pf, crs = latlong)
  roundabouts$id <- seq(1:NROW(roundabouts))
  if(NROW(rdz)>0){
  rdz <- rbind(rdz, roundabouts)
  } else {rdz <- roundabouts}
} 
}

if(NROW(polyz)>0 & is.null(rdz)){
  
  roundabouts <- filter(polyz, highway == "primary")
  
  if(NROW(roundabouts)>0 ){
    
    rdz <- st_cast(roundabouts, "LINESTRING")
    rds <- st_coordinates(rdz)
    p1 <- data.frame(x1 = rds[1:NROW(rds)-1,1], y1 = rds[1:NROW(rds)-1,2])
    p1 <- st_as_sf(p1, coords = c("x1", "y1"), crs = latlong)                
    p2 <-  data.frame(x2 = rds[2:NROW(rds),1], y2 = rds[2:NROW(rds),2])
    p2 <- st_as_sf(p2, coords = c("x2", "y2"), crs = latlong)
    pf <- st_sfc(mapply(function(a,b){st_cast(st_union(a,b),"LINESTRING")}, p1$geometry, p2$geometry, SIMPLIFY=FALSE))
    rdz <- st_as_sf(pf, crs = latlong)
    rdz$id <- seq(1:NROW(rdz))
    
  } else {
    rdz <- NULL
    roundabouts <- NULL
    }
} 



if(is.null(rdz)| !NROW(rdz)>0){
  modelled_area <- max_min_area %>% 
    st_as_sf(coords = c("X", "Y"), crs = ukgrid) %>%
    dplyr::summarise(data = st_combine(geometry)) %>% 
    st_cast("POLYGON") %>% 
    st_buffer(10) %>% 
    st_transform(latlong)
  
  x <- opq(bbox = modelled_area) %>% 
    add_osm_feature(key = c('highway')) %>% 
    osmdata_sf()

  rdz <- x$osm_lines
  
  if(!is.null(rdz)){
    rdz <- rdz %>% 
      filter(highway %in% c("primary", "trunk", "trunk_link", "tertiary", "motorway", "unclassified")) %>% 
      select(id = osm_id, geometry)
  }
  
  polyz <- x$osm_polygons 
  
  if(NROW(polyz)>0 & !is_null(rdz)){
    
    roundabouts <- filter(polyz, highway == "primary")
    
    if(NROW(roundabouts)>0 ){
      
      
      roundabouts <- st_cast(roundabouts, "LINESTRING")
      rbz <- st_cast(roundabouts, "LINESTRING")
      rds <- st_coordinates(rbz)
      p1 <- data.frame(x1 = rds[1:NROW(rds)-1,1], y1 = rds[1:NROW(rds)-1,2])
      p1 <- st_as_sf(p1, coords = c("x1", "y1"), crs = latlong)                
      p2 <-  data.frame(x2 = rds[2:NROW(rds),1], y2 = rds[2:NROW(rds),2])
      p2 <- st_as_sf(p2, coords = c("x2", "y2"), crs = latlong)
      pf <- st_sfc(mapply(function(a,b){st_cast(st_union(a,b),"LINESTRING")}, p1$geometry, p2$geometry, SIMPLIFY=FALSE))
      roundabouts <- st_as_sf(pf, crs = latlong)
      roundabouts$id <- seq(1:NROW(roundabouts))
      
      
      if(NROW(roundabouts)>0){
        if("geometry" %in% colnames(roundabouts))
        {
          rdz <- rbind(rdz, roundabouts)
        } else {
          r_geo <- data.frame(roundabouts$x)
          r_geo$id <- roundabouts$id
          st_geometry(r_geo) <- r_geo$geometry
          
          rdz <- rbind(rdz, r_geo)
        }
        
      } else {rdz <- roundabouts}
    } 
  }
  
  if(NROW(polyz)>0 & is.null(rdz)){
    
    roundabouts <- filter(polyz, highway == "primary")
    
    if(NROW(roundabouts)>0 ){
      
      rdz <- st_cast(roundabouts, "LINESTRING")
      rds <- st_coordinates(rdz)
      p1 <- data.frame(x1 = rds[1:NROW(rds)-1,1], y1 = rds[1:NROW(rds)-1,2])
      p1 <- st_as_sf(p1, coords = c("x1", "y1"), crs = latlong)                
      p2 <-  data.frame(x2 = rds[2:NROW(rds),1], y2 = rds[2:NROW(rds),2])
      p2 <- st_as_sf(p2, coords = c("x2", "y2"), crs = latlong)
      pf <- st_sfc(mapply(function(a,b){st_cast(st_union(a,b),"LINESTRING")}, p1$geometry, p2$geometry, SIMPLIFY=FALSE))
      rdz <- st_as_sf(pf, crs = latlong)
      rdz$id <- seq(1:NROW(rdz))
      
    } else {
      rdz <- NULL
      roundabouts <- NULL
    }
  }
  
}

rdz <- st_transform(rdz, ukgrid)

all_rdz <- unique(rdz$id)

point_angles <- list()
for (rd in all_rdz){
  dt <- filter(rdz, id == rd)
  
  start <- st_line_sample(dt, sample = 0)
  start <- st_cast(start, "POINT")
  start <- st_transform(start, latlong)
  #start <- data.frame(start)
  end <- st_line_sample(dt, sample = 1)
  end <- st_cast(end, "POINT")
  end <- st_transform(end, latlong)
  # end <- data.frame(end)
  p <- c(start, end)
  a <- st_geod_azimuth(p)
  a <- as.numeric(NISTradianTOdeg(a))
  dt$angle <- a
  nam <- as.character(rd)
  point_angles[[nam]] <- dt
}

all_osm_angles <- do.call(rbind, point_angles)

if(NROW(all_osm_angles)>1){
  all_osm_angles <- filter(all_osm_angles, angle < avg_angle+45 & angle > avg_angle-45)
}

if(NROW(all_osm_angles)==1){
  most_common <- st_line_sample(all_osm_angles, sample = 1)
  most_common <- st_as_sf(most_common)
  most_common <- st_transform(most_common, latlong)
}

if(NROW(all_osm_angles)>1){
  all_osm_angles <- st_line_sample(all_osm_angles, sample = 1)
  all_osm_angles <- st_transform(all_osm_angles, latlong)
  all_osm_angles <- data.frame(st_coordinates(all_osm_angles))
  all_osm_angles <- st_as_sf(all_osm_angles, coords = c("X", "Y"), crs = latlong)
  startz <- scz %>% 
    st_as_sf(coords = c("X", "Y"), crs = ukgrid) %>% 
    st_transform(latlong)
  
  startz$near <- st_nearest_feature(startz, all_osm_angles)
  most_common <- all_osm_angles[median(startz$near),]
} 

if(NROW(all_osm_angles)<1){
  startz <- scz %>% 
    st_as_sf(coords = c("X", "Y"), crs = ukgrid)
  all_osm_angles <- do.call(rbind, point_angles)
  startz$near <- st_nearest_feature(startz, all_osm_angles)
  most_common_line <- all_osm_angles[median(startz$near),]
  mcl_coords_sf <- st_line_sample(most_common_line, n = 5)
  mcl_coords <- data.frame(st_coordinates(mcl_coords_sf))
  mcl_points_sf <- st_as_sf(mcl_coords, coords = c("X", "Y"), crs = ukgrid)
  
  startz$near <- st_nearest_feature(startz, mcl_points_sf)
  most_common <- mcl_points_sf[median(startz$near),]
  most_common <- most_common %>% 
    st_transform(latlong)
  
  
}
#startz <- st_transform(scs, latlong)
#startz <- data.frame(st_coordinates(startz))
if("geometry" %in% colnames(most_common)){
  most_common$L1 <- r
} else {
  mc_geo <- data.frame(most_common$x)
  mc_geo$L1 <- r
  st_geometry(mc_geo) <- mc_geo$geometry
  most_common <- mc_geo
}

start_points[[r]] <- most_common

#show progress
print(r)
# update GUI console
flush.console()

}

new_starts <- do.call(rbind, start_points)
saveRDS(new_starts, "data/start_points_new.RDS")

##END POINTS
  epz <- unique(all_routes$journeyend)
  r <- epz[3]
  end_points <- list()
  for (r in epz){
    
    df <- filter(all_routes, journeyend == r)
    all_rdz <- unique(df$sk_dim_journeylinkid)
    point_angles <- list()
    for (a in all_rdz){
      dt <- filter(df, sk_dim_journeylinkid == a)
      dt <- data.frame(st_coordinates(dt))
      s <- NROW(dt)-2
      f <- NROW(dt)-1
      dt <- dt[s:f,]
      dt <- st_as_sf(dt, coords = c("X", "Y"), crs = latlong)
      dt1 <- data.frame(a = dt$geometry[1], b = dt$geometry[2])
      
      dt1$anglez <- as.numeric(NISTradianTOdeg(st_geod_azimuth(c(dt1$geometry, dt1$geometry.1))))
      nam1 <- as.character(a)
      point_angles[[nam1]] <- dt1
    }
    
    all_route_angles <- do.call(rbind, point_angles)
    avg_angle <- mean(all_route_angles$anglez)
    
    df_coords <- data.frame(st_coordinates(df))
    df <- st_transform(df, ukgrid)
    ##find second point from start of route by looping through each route
    journiez <- unique(df$sk_dim_journeylinkid)
    sfs <- list()
    for (d in journiez){
      df2 <- filter(df, sk_dim_journeylinkid == d)
      sfe <- data.frame(st_coordinates(df2))
      second_from_end <- sfe[NROW(sfe)-1,]
      sfs[[d]] <- second_from_end
    }
    
    #scs <-  st_line_sample(df, sample = 0)
    scz <- do.call(rbind, sfs)
   
    max_min_area <- data.frame(X = c(min(scz$X), max(scz$X), max(scz$X), min(scz$X)),
                               Y = c(max(scz$Y), max(scz$Y), min(scz$Y), min(scz$Y)))
    
    modelled_area <- max_min_area %>% 
      st_as_sf(coords = c("X", "Y"), crs = ukgrid) %>%
      dplyr::summarise(data = st_combine(geometry)) %>% 
      st_cast("POLYGON") %>%
      st_buffer(3) %>% 
      st_transform(latlong)
    
    x <- opq(bbox = modelled_area) %>% 
      add_osm_feature((key = c('highway'))) %>% 
      osmdata_sf()
   
    rdz <- x$osm_lines
    ##filter out roads that are not wanted such as cycle paths
    if(!is.null(rdz)){
      rdz <- rdz %>% 
        filter(highway %in% c("primary", "primary_link", "secondary", "trunk", "trunk_link", "tertiary", "motorway", "unclassified")) 
    }
    
    if(NROW(rdz)>0){
      if("osm_id" %in% colnames(rdz))
      {
        rdz <- rdz %>% select(id = osm_id, geometry)
      } else {
        rdz <- rdz %>% 
          mutate(osm_id = row.names(rdz)) %>% 
          select(id = osm_id, geometry)
      }
      
      
    }
    
    polyz <- x$osm_polygons 
    
    if(NROW(polyz)>0 & !is_null(rdz)){
      
      roundabouts <- filter(polyz, highway == "primary")
      
      if(NROW(roundabouts)>0 ){
        
        roundabouts <- st_cast(roundabouts, "LINESTRING")
        rbz <- st_cast(roundabouts, "LINESTRING")
        rds <- st_coordinates(rbz)
        p1 <- data.frame(x1 = rds[1:NROW(rds)-1,1], y1 = rds[1:NROW(rds)-1,2])
        p1 <- st_as_sf(p1, coords = c("x1", "y1"), crs = latlong)                
        p2 <-  data.frame(x2 = rds[2:NROW(rds),1], y2 = rds[2:NROW(rds),2])
        p2 <- st_as_sf(p2, coords = c("x2", "y2"), crs = latlong)
        pf <- st_sfc(mapply(function(a,b){st_cast(st_union(a,b),"LINESTRING")}, p1$geometry, p2$geometry, SIMPLIFY=FALSE))
        roundabouts <- st_as_sf(pf, crs = latlong)
        roundabouts$id <- seq(1:NROW(roundabouts))
        if(NROW(rdz)>0){
          rdz <- rbind(rdz, roundabouts)
        } else {rdz <- roundabouts}
      } 
    }
    
    if(NROW(polyz)>0 & is.null(rdz)){
      
      roundabouts <- filter(polyz, highway == "primary")
      
      if(NROW(roundabouts)>0 ){
        
        rdz <- st_cast(roundabouts, "LINESTRING")
        rds <- st_coordinates(rdz)
        p1 <- data.frame(x1 = rds[1:NROW(rds)-1,1], y1 = rds[1:NROW(rds)-1,2])
        p1 <- st_as_sf(p1, coords = c("x1", "y1"), crs = latlong)                
        p2 <-  data.frame(x2 = rds[2:NROW(rds),1], y2 = rds[2:NROW(rds),2])
        p2 <- st_as_sf(p2, coords = c("x2", "y2"), crs = latlong)
        pf <- st_sfc(mapply(function(a,b){st_cast(st_union(a,b),"LINESTRING")}, p1$geometry, p2$geometry, SIMPLIFY=FALSE))
        rdz <- st_as_sf(pf, crs = latlong)
        rdz$id <- seq(1:NROW(rdz))
        
      } else {
        rdz <- NULL
        roundabouts <- NULL
      }
    } 
    
    
    
    if(is.null(rdz)| !NROW(rdz)>0){
      modelled_area <- max_min_area %>% 
        st_as_sf(coords = c("X", "Y"), crs = ukgrid) %>%
        dplyr::summarise(data = st_combine(geometry)) %>% 
        st_cast("POLYGON") %>% 
        st_buffer(10) %>% 
        st_transform(latlong)
      
      x <- opq(bbox = modelled_area) %>% 
        add_osm_feature(key = c('highway')) %>% 
        osmdata_sf()
      
      rdz <- x$osm_lines
      
      if(!is.null(rdz)){
        rdz <- rdz %>% 
          filter(highway %in% c("primary", "trunk", "trunk_link", "tertiary", "motorway", "unclassified")) %>% 
          select(id = osm_id, geometry)
      }
      
      polyz <- x$osm_polygons 
      
      if(NROW(polyz)>0 & !is_null(rdz)){
        
        roundabouts <- filter(polyz, highway == "primary")
        
        if(NROW(roundabouts)>0 ){
          
          
          roundabouts <- st_cast(roundabouts, "LINESTRING")
          rbz <- st_cast(roundabouts, "LINESTRING")
          rds <- st_coordinates(rbz)
          p1 <- data.frame(x1 = rds[1:NROW(rds)-1,1], y1 = rds[1:NROW(rds)-1,2])
          p1 <- st_as_sf(p1, coords = c("x1", "y1"), crs = latlong)                
          p2 <-  data.frame(x2 = rds[2:NROW(rds),1], y2 = rds[2:NROW(rds),2])
          p2 <- st_as_sf(p2, coords = c("x2", "y2"), crs = latlong)
          pf <- st_sfc(mapply(function(a,b){st_cast(st_union(a,b),"LINESTRING")}, p1$geometry, p2$geometry, SIMPLIFY=FALSE))
          roundabouts <- st_as_sf(pf, crs = latlong)
          roundabouts$id <- seq(1:NROW(roundabouts))
         
          
          if(NROW(roundabouts)>0){
            if("geometry" %in% colnames(roundabouts))
            {
              rdz <- rbind(rdz, roundabouts)
            } else {
              r_geo <- data.frame(roundabouts$x)
              r_geo$id <- roundabouts$id
              st_geometry(r_geo) <- r_geo$geometry
              
              rdz <- rbind(rdz, r_geo)
            }
            
          } else {rdz <- roundabouts}
        } 
      }
      
      if(NROW(polyz)>0 & is.null(rdz)){
        
        roundabouts <- filter(polyz, highway == "primary")
        
        if(NROW(roundabouts)>0 ){
          
          rdz <- st_cast(roundabouts, "LINESTRING")
          rds <- st_coordinates(rdz)
          p1 <- data.frame(x1 = rds[1:NROW(rds)-1,1], y1 = rds[1:NROW(rds)-1,2])
          p1 <- st_as_sf(p1, coords = c("x1", "y1"), crs = latlong)                
          p2 <-  data.frame(x2 = rds[2:NROW(rds),1], y2 = rds[2:NROW(rds),2])
          p2 <- st_as_sf(p2, coords = c("x2", "y2"), crs = latlong)
          pf <- st_sfc(mapply(function(a,b){st_cast(st_union(a,b),"LINESTRING")}, p1$geometry, p2$geometry, SIMPLIFY=FALSE))
          rdz <- st_as_sf(pf, crs = latlong)
          rdz$id <- seq(1:NROW(rdz))
          
        } else {
          rdz <- NULL
          roundabouts <- NULL
        }
      }
      
    }
    
    rdz <- st_transform(rdz, ukgrid)
    
    all_rdz <- unique(rdz$id)
    
    point_angles <- list()
    for (rd in all_rdz){
      dt <- filter(rdz, id == rd)
      
      start <- st_line_sample(dt, sample = 0)
      start <- st_cast(start, "POINT")
      start <- st_transform(start, latlong)
      #start <- data.frame(start)
      end <- st_line_sample(dt, sample = 1)
      end <- st_cast(end, "POINT")
      end <- st_transform(end, latlong)
      # end <- data.frame(end)
      p <- c(start, end)
      a <- st_geod_azimuth(p)
      a <- as.numeric(NISTradianTOdeg(a))
      dt$angle <- a
      nam <- as.character(rd)
      point_angles[[nam]] <- dt
    }
    
    all_osm_angles <- do.call(rbind, point_angles)
    
    if(NROW(all_osm_angles)>1){
      all_osm_angles <- filter(all_osm_angles, angle < avg_angle+45 & angle > avg_angle-45)
    }
    
    if(NROW(all_osm_angles)==1){
      most_common <- st_line_sample(all_osm_angles, sample = 1)
      most_common <- st_as_sf(most_common)
      most_common <- st_transform(most_common, latlong)
    }
    
    if(NROW(all_osm_angles)>1){
      all_osm_angles <- st_line_sample(all_osm_angles, sample = 1)
      all_osm_angles <- st_transform(all_osm_angles, latlong)
      all_osm_angles <- data.frame(st_coordinates(all_osm_angles))
      all_osm_angles <- st_as_sf(all_osm_angles, coords = c("X", "Y"), crs = latlong)
      startz <- scz %>% 
        st_as_sf(coords = c("X", "Y"), crs = ukgrid) %>% 
        st_transform(latlong)
      
      startz$near <- st_nearest_feature(startz, all_osm_angles)
      most_common <- all_osm_angles[median(startz$near),]
    } 
    
    if(NROW(all_osm_angles)<1){
      startz <- scz %>% 
        st_as_sf(coords = c("X", "Y"), crs = ukgrid)
      all_osm_angles <- do.call(rbind, point_angles)
      startz$near <- st_nearest_feature(startz, all_osm_angles)
      most_common_line <- all_osm_angles[median(startz$near),]
      mcl_coords_sf <- st_line_sample(most_common_line, n = 5)
      mcl_coords <- data.frame(st_coordinates(mcl_coords_sf))
      mcl_points_sf <- st_as_sf(mcl_coords, coords = c("X", "Y"), crs = ukgrid)
      
      startz$near <- st_nearest_feature(startz, mcl_points_sf)
      most_common <- mcl_points_sf[median(startz$near),]
      most_common <- most_common %>% 
        st_transform(latlong)
      
      
    }
    #startz <- st_transform(scs, latlong)
    #startz <- data.frame(st_coordinates(startz))
    if("geometry" %in% colnames(most_common)){
      most_common$L1 <- r
    } else {
      mc_geo <- data.frame(most_common$x)
      mc_geo$L1 <- r
      st_geometry(mc_geo) <- mc_geo$geometry
      most_common <- mc_geo
    }
    
    end_points[[r]] <- most_common
    
    #show progress
    print(r)
    # update GUI console
    flush.console()
    
  }
  

new_ends <- do.call(rbind, end_points)
new_ends <- st_cast(new_ends, "POINT")
saveRDS(new_ends, "data/end_points_new.RDS")
st_geometry(new_ends) <- new_ends$geometry
new_ends <- st_as_sf(new_ends, crs = latlong)
##GENERATE ROUTE BETWEEN POINTS


# route_carshort = route(
#   l = df,
#   route_fun = osrmRoute,
#   overview = "full",
#   returnclass = "sf" # argument passed to route_fun
# )

rowts <- unique(all_routes$sk_dim_journeylinkid)
new_routes <- list()
rowt <- rowts[5]
for(rowt in rowts){
  
  r_df <- filter(all_routes, sk_dim_journeylinkid == rowt)

  sp <- filter(new_ends, L1 == r_df$journeystart)
  if(!NROW(sp)>0){
    sp <- filter(new_starts, L1 == r_df$journeystart)
  }
  ep <- filter(new_ends, L1 == r_df$journeyend)
  
  success <- FALSE
  while(!success){
    tryCatch({
      route <- osrmRoute(src = sp, dst = ep,
                         overview = "full", returnclass = "sf")
      
      success <- NROW(route) > 0
      
      
      #show progress
      print(rowt)
      # update GUI console
      flush.console()
    },error=function(e){
      Sys.sleep(5)
    },finally={})
    
  }
  
  nam <- as.character(rowt)
  route$link_id <- nam
  
  new_routes[[nam]] <- route
  
}

all_new_routes <- do.call(rbind, new_routes)
saveRDS(all_new_routes, "data/new_routes.RDS")
# ID <- rowts[11]
# n_route <- filter(all_new_routes, link_id == ID)
# o_route <- filter(all_routes, sk_dim_journeylinkid == ID)

#all_routes <- st_read("data/dim-journey-links.geojson")
#all_new_routes <- readRDS("data/new_routes.RDS")

all_routes <- st_transform(all_routes, latlong)
all_new_routes <- st_transform(all_new_routes, latlong)



#pal_NO2 = colorNumeric("viridis", domain = roadside_concs$RNO22018)

lat <- mean(st_coordinates(all_routes)[,2])
lon <- mean(st_coordinates(all_routes)[,1])

m <- leaflet() %>% 
  setView(lon, lat, zoom = 15) %>% 
  addProviderTiles("CartoDB.Positron", group = "CartoDB") %>%
  addScaleBar(position = "bottomleft", options = scaleBarOptions(maxWidth = 200, metric = TRUE, imperial = TRUE, updateWhenIdle = TRUE)) %>% 
  leafem::addMouseCoordinates(proj4string = CRS(ukgrid), native.crs = F)

route_ids <- unique(all_new_routes$link_id)

for (r in route_ids){
  
  old <- filter(all_routes, sk_dim_journeylinkid == r)
  new <- filter(all_new_routes, link_id == r)
  
  m <- m %>% 
    addPolylines(data = old, color = "red", weight = 6,
                 opacity = 0.7, fillOpacity = 0.2,
                 fillColor = "blue", popup = paste("Link ID:", old$sk_dim_journeylinkid, "<br>",
                                                      "Distance:", old$length_m, "<br>",
                                                      "Start Node:", old$journeystart, "<br>",
                                                      "End Node:", old$journeyend, "<br>"),
                 highlightOptions = highlightOptions(color = "white", weight = 2,
                                                     bringToFront = FALSE), group = r)
  
  m <- m %>% 
    addPolylines(data = new, color = "blue", weight = 6,
                 opacity = 0.7, fillOpacity = 0.2,
                 fillColor = "blue", popup = paste("Link ID:", new$link_id, "<br>",
                                                      "Distance:", new$distance, "<br>",
                                                      "Start Node:", new$src, "<br>",
                                                      "End Node:", new$dst, "<br>"),
                 highlightOptions = highlightOptions(color = "white", weight = 2,
                                                     bringToFront = FALSE), group = r)
  
}

m = m %>% addLegend("bottomleft", colors = c("blue", "red"),labels = c("new route", "old route"),
                        title = "routes",
                        opacity = 1)

m <- m %>% addLayersControl(baseGroups = route_ids,
                            options = layersControlOptions(collapsed = FALSE), position = "topright") %>% 
  addMeasurePathToolbar() %>% 
  addMeasure(primaryLengthUnit = "meters")

withr::with_dir('./', saveWidget(m, file="old_vs_new.html"))



# 
# rdz$id <- seq(1:NROW(rdz))
# rdz$azi <- st_geod_azimuth(rdz)
# ?st_geod_azimuth
# 
# route_1 <- filter(all_json_routes, sk_dim_journeylinkid == 3)
# 
# all_sites <- unique(all_countz$Journey.Link.ID)
# 
# for (s in all_sites){
#   df <- filter(all_countz, Journey.Link.ID == s)
#   df$Date.Time <- gsub("T", " ", df$Date.Time)
#   df$Date.Time <- str_sub(df$Date.Time, 0, -7)
#   df$date <- ymd_hms(df$Date.Time)
#   df$Date.Time <- gsub("+", " ", df$Date.Time)
#   df$Total.Matches <- as.numeric(df$Total.Matches)
#   df <- select(df, -Date.Time, -Geo.Shape, -geo_point_2d)
#   saveRDS(df, paste0("hourly/", s, ".RDS"))
# }
# 
# n <- 2
# num_2 <- readRDS(paste0("01_ANPR_hourly/", n, ".RDS"))
# 
# timeVariation(num_2)

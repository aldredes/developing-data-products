library(sp)
library(rworldmap)
library(rworldxtra)

# The single argument to this function, points, is a data.frame in which:
#   - column 1 contains the longitude in degrees
#   - column 2 contains the latitude in degrees
coords2MapObject <- function(points, returnVal = 'name')
{  
    #countriesSP <- getMap(resolution='low')
    countriesSP <- getMap(resolution='high') #you could use high res map from rworldxtra if you were concerned about detail
    # some of the points are near coast and they’re not picked up properly from low resolution
    
    # converting points to a SpatialPoints object
    # setting CRS directly to that from rworldmap
    pointsSP = SpatialPoints(points, proj4string = CRS(proj4string(countriesSP)))  
    
    
    # use 'over' to get indices of the Polygons object containing each point 
    indices = over(pointsSP, countriesSP)
    #print(names(indices))
    
    if (returnVal == "continent6") return (indices$continent)   # returns the continent (6 continent model)
    if (returnVal == "continent7") return (indices$REGION)   # returns the continent (7 continent model)
    if (returnVal == "shortcode") return (indices$ISO3) # returns the ISO3 code 
    if (returnVal == "shortcode2") return (indices$ISO_A2) # returns the ISO3 code 
    if (returnVal == "shortcode3") return (indices$ISO_A3) # returns the ISO3 code 
    
    
    return (indices$ADMIN)  #returns country name
}

meteors <- fread("Meteorite_Landings.csv", na.strings = c("NA", "#DIV/0!", ""), header = TRUE)
meteors <- meteors[!is.na(meteors$GeoLocation), ]
meteors <- meteors[!is.na(meteors$year), ]
meteors$year <- as.numeric(format(as.POSIXct(strptime(meteors$year, "%m/%d/%Y %H:%M:%S %p"), tz="GMT"), "%Y")) 
meteors <- meteors[meteors$year <= 2017, ]
meteors$countryName <- coords2MapObject(meteors[, c(9,8)])
meteors$iso2 <- coords2MapObject(meteors[, c(9,8)], returnVal = 'shortcode2')
meteors$wmk2006Class <- ""
meteors[substr(meteors$recclass, start=1, stop=3) %in% c("ure", "bra", "aca", "lod", "win", "iab"), ]$wmk2006Class <- "Primitive Achondrites"
meteors[substr(meteors$recclass, start=1, stop=5) %in% c("iiicd"), ]$wmk2006Class <- "Primitive Achondrites"
meteors[substr(meteors$recclass, start=1, stop=2) %in% c("mg", "es", "pp", "ic"), ]$wmk2006Class <- "Achondrites"
meteors[substr(meteors$recclass, start=1, stop=3) %in% c("opx", "cha", "nak", "she", "ivb", "iva", "iid", "iie", "iic", "how", "dio", "euc", "aub", "ang", "mes"), ]$wmk2006Class <- "Achondrites"
meteors[substr(meteors$recclass, start=1, stop=4) %in% c("iiab", "iiie", "iiif"), ]$wmk2006Class <- "Achondrites"
meteors[substr(meteors$recclass, start=1, stop=5) %in% c("iiiab"), ]$wmk2006Class <- "Achondrites"
meteors[substr(meteors$recclass, start=1, stop=1) %in% c("r", "k"), ]$wmk2006Class <- "Chondrites"
meteors[substr(meteors$recclass, start=1, stop=2) %in% c("ll"), ]$wmk2006Class <- "Chondrites (Ordinary)"
meteors[substr(meteors$recclass, start=1, stop=1) %in% c("l","h"), ]$wmk2006Class <- "Chondrites (Ordinary)"
meteors[substr(meteors$recclass, start=1, stop=2) %in% c("ci","cm","co", "cv", "ck", "cr", "ch", "cb"), ]$wmk2006Class <- "Chondrites (Carbonaceous)"
meteors[meteors$recclass %in% c("iron, iid-an", "iron, ivb", "iron, iiab", "iron, iie", "iron, iiiab-an", "iron, iva", "iron, iid", "iron, iif", "iron, iiie", "iron, iva-an", "iron, ic-an", "iron, iiie-an", "iron, iig", "iron, iiab-an",  "iron, iie-an", "iron, iic", "iron, iiiab?", "iron, iiiab" ,  "iron, ic" ,  "iron, iiif"), ]$wmk2006Class <- "Achondrites"
meteors[substr(meteors$recclass, start=1, stop=11) %in% c("iron, iiicd"), ]$wmk2006Class <- "Primitive Achondrites"
meteors[substr(meteors$recclass, start=1, stop=9) %in% c("iron, iab"), ]$wmk2006Class <- "Primitive Achondrites"
meteors[substr(meteors$recclass, start=1, stop=3) %in% c("mar", "pal"), ]$wmk2006Class <- "Achondrites"
meteors[substr(meteors$recclass, start=1, stop=3) %in% c("ach"), ]$wmk2006Class <- "Achondrites"
meteors[substr(meteors$recclass, start=1, stop=1) == "c", ]$wmk2006Class <- "Chondrites (Carbonaceous)"
meteors[substr(meteors$recclass, start=1, stop=1) == "e", ]$wmk2006Class <- "Chondrites (Enstatite)"
meteors[substr(meteors$recclass, start=1, stop=2) %in% c("eh", "el"), ]$wmk2006Class <- "Chondrites (Enstatite)"
meteors[substr(meteors$recclass, start=1, stop=2) == "es", ]$wmk2006Class <- "Achondrites"
meteors[meteors$recclass %in% c("fusion crust","iron","iron, ungrouped","oc","stone-uncl","stone-ung","unknown" ), ]$wmk2006Class <- "Unclassified"
meteors <- meteors[meteors$GeoLocation != "(0.000000, 0.000000)"]
        
fwrite(meteors, "nasa_meteors_scrubbed.csv")
        
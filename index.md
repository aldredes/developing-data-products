---
title       : Meteorite Landings
subtitle    : A Reproducible Pitch Presentation for Coursera Developing Data Products Course
author      : Aldrin R. Desoloc 
date        : Apr. 30, 2017
framework   : revealjs       
mode        : selfcontained 
revealjs    :
    theme       : night     # {"default", "simple", "sky", "beige", "serif", "solarized", "blood", "moon", "night", "black", "league" or "white"}
    highlight   : tango   # {"default", "tango", "pygments", "kate", "monochrome", "espresso", "zenburn", and "haddock".}
    center      : "true"
    transition  : concave          # {"default", "fade", "slide", "convex", "concave", "zoom" or "none"}
    incremental : "true"
---

<style>
body {
    background-image:url(assets/img/20161008_GINOP_KHK_2.jpg); 
    background-position: left top;
    background-repeat: no-repeat;
    background-size: cover;
}

.left-aligned {
    text-align: left;
}

.slides .footer{
    position: absolute;
    bottom: -30%;
    left: 3%;
    white-space: nowrap; 
}
</style>

## Meteorite Landings 
#### (A Reproducible Pitch Presentation for 
#### Coursera Developing Data Products Course)
<br />
#### Aldrin R. Desoloc 
#### May 04, 2017

--- .left-aligned 

## Overview

* The **Meteorite Landings** application can be accessed via https://aldredes.shinyapps.io/meteors/.  
* This shiny application basically shows the map of known meteorite landings around the world since year 301 until 2013 A.D. based on the comprehensive dataset from The Meteoritical Society.  
* The map is rendered using **leaflet**, and the graph is built with **plotly**.
* The dataset can be downloaded from https://data.nasa.gov/Space-Science/Meteorite-Landings/gh4g-9sfh.
<div class="footer">
           <small> ** The background image used: http://www.csillagaszat.hu/wp-content/uploads/2016/10/20161008_GINOP_KHK_2.jpg </small>
</div>


--- .left-aligned  &vertical 

## The Dataset

The data was made tidy using an R script by:

* removing all rows with no year and geolocation data;
* removing all rows with year beyond the current year;
* removing all rows with geolocation `(0, 0)`; 
* creating a new columns `countryName` and `iso2` to get the country and its corresponding official ISO 2 alpha country code from the map boundaries based on `getMap()` function of `rworldmap` library; and 
* creating a new column `wmk2006Class` derived from `recclass` column based on the _Weissberg-McCoy-Krot (2006) scheme_ * 
<br>
<small> * https://en.wikipedia.org/wiki/File:Meteorite_Classification_after_Weissberg_McCoy_Krot_2006_Stony_Iron.svg </small>

_(Click on down navigation pointer to view the slide of summary of dataset.)_

*** .left-aligned 

## The Dataset

Below is the summary of data *:


```r
suppressPackageStartupMessages(library(data.table))
meteors <- fread("data/nasa_meteors_scrubbed.csv", na.strings = c("NA", "#DIV/0!", ""), header = TRUE)
meteors$recclass <- as.factor(meteors$recclass)
meteors$wmk2006Class <- as.factor(meteors$wmk2006Class)
meteors$countryName <- as.factor(meteors$countryName)
meteors$iso2 <- as.factor(meteors$iso2)
summary(meteors)
##      name                 id          nametype            recclass   
##  Length:32039       Min.   :    1   Length:32039       L6     :6583  
##  Class :character   1st Qu.: 9258   Class :character   H5     :5611  
##  Mode  :character   Median :18627   Mode  :character   H4     :3335  
##                     Mean   :20802                      H6     :3232  
##                     3rd Qu.:27266                      L5     :2746  
##                     Max.   :57455                      LL5    :1897  
##                                                        (Other):8635  
##     mass (g)            fall                year          reclat      
##  Min.   :       0   Length:32039       Min.   : 301   Min.   :-87.37  
##  1st Qu.:       6   Class :character   1st Qu.:1982   1st Qu.:-79.68  
##  Median :      30   Mode  :character   Median :1991   Median :-72.00  
##  Mean   :   18553                      Mean   :1987   Mean   :-46.98  
##  3rd Qu.:     202                      3rd Qu.:2000   3rd Qu.: 18.40  
##  Max.   :60000000                      Max.   :2013   Max.   : 81.17  
##  NA's   :127                                                          
##     reclong        GeoLocation                          countryName   
##  Min.   :-165.43   Length:32039       Antarctica              :22090  
##  1st Qu.:  26.00   Class :character   Oman                    : 2992  
##  Median :  56.70   Mode  :character   United States of America: 1651  
##  Mean   :  73.01                      Libya                   : 1466  
##  3rd Qu.: 159.39                      Australia               :  638  
##  Max.   : 354.47                      (Other)                 : 3153  
##                                       NA's                    :   49  
##       iso2                      wmk2006Class  
##  AQ     :22090   Achondrites          :  986  
##  OM     : 2992   Chondrites           :30304  
##  US     : 1651   Primitive Achondrites:  441  
##  LY     : 1466   Unclassified         :  308  
##  AU     :  638                                
##  (Other): 3135                                
##  NA's   :   67
```

<small> * `NA`'s in the `countryName` and `iso2` means no country was retrieved from the function and probably points to a body of water on earth. </small>

--- .left-aligned 

## User Interface / Features

* To make the application simpler, we limit the filtering elements and user inputs.  The user can filter the data by **year**, and **classifications** according to _Weissberg-McCoy-Krot (2006) scheme_. 
* Additional features include a **plot** of counts by year, a selection to change the **map tile provider**, and an option to change the **map information display**.
* The choropleth map was built using the spatial data from a _key markup language (KML)_ file * to set the boundaries among countries.
<small> * The said KML file can be retrieved from https://community.qlik.com/docs/DOC-7295 and it's last updated in 2014. </small>
* Progress bar is also shown when loading information on the map.  _(The loading of map elements depend on the response from the shiny server, and sometimes it takes ~5secs to load the updated elements.)_


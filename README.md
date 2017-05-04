# Meteorite Landings
### _(Shiny Application and Reproducible Pitch Presentation Project for Developing Data Products course of Coursera Data Science Specialization)_


## Overview of Shiny Application

The Meteorite Landings web application basically shows the map of known meteorite landings around the world since year 301 until 2013 A.D., and is based on the comprehensive dataset: NASA's Open Data Portal: Meteorite Landings (https://data.nasa.gov/Space-Science/Meteorite-Landings/gh4g-9sfh). The application can be launched via https://aldredes.shinyapps.io/meteors/. The map is rendered using **leaflet**, and the graph is built with **plotly**.  

The documentation about how using the application is saved as PDF file and is incorporated in the application.  It can be accessed thru "How to use this application?" link, or can be read directly with this link: https://aldredes.shinyapps.io/meteors/docs/using_meteorite_landings_app.pdf.

The codes for the shiny application include the following (_showing only what was used_): 

```
 server.R
 ui.R
 data  
   +-- nasa_meteors_scrubbed.csv
   +-- spatial
         +-- world_ISO2.kml
 www
   +-- docs
   |     +-- using_meteorite_landings_app.pdf
   +-- images
         +-- 220px-Leonid_Meteor.jpg
```


## Pitch Presentation 

The 5-slide pitch presentation was built using **slidify** and can be viewed from this link: https://aldredes.github.io/developing-data-products/

The files for the slidify presentation include the following (_showing only what was added/used, others are automatically created_): 

```
 index.Rmd
 index.html
 index.md
 assets	
   +-- img
         +-- 20161008_GINOP_KHK_2.jpg
 libraries
   +-- frameworks
         +-- revealjs
```

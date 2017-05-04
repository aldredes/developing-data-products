#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(plotly)
library(leaflet)
library(leaflet.extras)
library(sp)
library(rgdal)
library(data.table)
library(viridisLite)
library(compiler)

bootstrapPage(
    tags$style(
        HTML("
            .absolutePanel {
                background-color: rgba(88, 88, 88, 0.7);
                padding-top: 10px;
                padding-right: 30px;
                padding-bottom: 10px;
                padding-left: 30px;
                border-radius: 10px;
                color: rgb(255, 255, 255);
            }

            .graphPanel {
                background-color: rgba(248, 248, 255, 0.7);
                padding-top: 10px;
                padding-right: 10px;
                padding-bottom: 10px;
                padding-left: 10px;
                border-radius: 10px;
            }

            .irs-min, .irs-max {
                color: rgb(255, 255, 255);
            }

            .marker-cluster-small, .marker-cluster-medium , .marker-cluster-large {
                background-color: rgba(153, 0, 0, 0.3);
            }

            .marker-cluster-small div, .marker-cluster-medium div, .marker-cluster-large div  {
                background-color: rgba(153, 0, 0, 0.5);
                color: rgb(255, 255, 255);
            }
            
            .leaflet-popup-content-wrapper {
                background-color: rgba(153, 0, 0, 0.7);
                color: rgb(255, 255, 255);
            }

            #howToLink {
                color: rgb(255, 255, 255);
            }

            #meteorClassLink {
                color: rgb(255, 255, 255);
                font-style: italic;
            }

            html, body {
                width: 100%;
                height: 100%;
            }")
        ),
    leafletOutput("map", width = "100%", height = "100%"),
    uiOutput("controlPanel"),
    uiOutput("graphPane")
)
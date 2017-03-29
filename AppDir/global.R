library(shiny)
library(shinySignals)
library(dplyr)
library(shinydashboard)
library(bubbles)
library(data.table)
library(ggplot2)
load("../data/jobgroups.RData")
load("../data/bubbleData.RData")
load("../data/groupjobs.RData")
load("../data/skillmiss.RData")
load("../data/skilllev1.RDatat")
load("../data/skillnet.RDatat")
source("plotFunctions.R")
library(shiny)
library(leaflet)
library(sp)
library(rgeos)
library(rgdal)
library(RColorBrewer)
library(raster)
library(GGally)
#
data(wrld_simpl,package="maptools")
countr <- subset(wrld_simpl, ISO2%in%c("IT","IE","DE","CZ","UK"))
#load("/data/shape/Countr.RData")
# germ <- readOGR("/data/shape", layer = "DEU_adm0",
#                      stringsAsFactors = F, encoding = "UTF-8")
# germ <- spTransform(germ, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
# 
# cze  <- readOGR("/data/shape", layer = "CZE_adm0",
#                 stringsAsFactors = F, encoding = "UTF-8")
# cze <- spTransform(cze, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
# 
# ita  <- readOGR("/data/shape", layer = "ITA_adm0",
#                 stringsAsFactors = F, encoding = "UTF-8")
# ita <- spTransform(ita, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
# 
# irl  <- readOGR("/data/shape", layer = "IRL_adm0",
#                 stringsAsFactors = F, encoding = "UTF-8")
# irl <- spTransform(irl, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
# 
# gbr  <- readOGR("/data/shape", layer = "GBR_adm0",
#                 stringsAsFactors = F, encoding = "UTF-8")
# gbr <- spTransform(gbr, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
# 
# countr <- rbind(germ,cze,ita,irl,gbr)

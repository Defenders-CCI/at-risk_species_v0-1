# Global functions to be called at app initiation.
# Copyright (c) 2016 Defenders of Wildlife, jmalcom@defenders.org

# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program; if not, see <http://www.gnu.org/licenses/>.

#############################################################################
# Load packages and source files
#############################################################################
library(DBI)
library(DT)
library(dplyr)
library(httr)
library(lattice)
library(lubridate)
library(RCurl)
library(jsonlite)
library(reldist)
library(rgbif)
library(shiny)
library(shinydashboard)
library(shinyBS)
library(RSQLite)
library(xtable)

library(leaflet)
library(maptools)
library(sp)

library(googleVis)
library(plyr)

source("data_mgmt/make_dataframes.R")
source("data_mgmt/subset_fx.R")
source("data_mgmt/summary_fx.R")
source("plot/graphs.R")
source("txt/help.R")
source("txt/metadata.R")
source("txt/notes.R")
source("txt/text_styles.R")


#############################################################################
# Load the data and basic data prep
#############################################################################
dbfil <- "data/dbs/SE_Candidates_v036.db"
con <- dbConnect(SQLite(), dbfil)
tables <- dbListTables(con)

cleanq <- function(x) { gsub('"', '', x, fixed=TRUE) }
for(i in tables) {
    res <- dbSendQuery(con, paste("SELECT * FROM", i))
    temp_df <- dbFetch(res)
    temp_df <- data.frame(apply(temp_df, MARGIN=2, FUN=cleanq))
    temp_df <- temp_df[c(2:length(temp_df[[1]])), ]
    assign(i, temp_df)
    dbClearResult(res)
}
dbDisconnect(con)

# Get the connection to the GBIF db opened, ready to pass around
# GBIF_db <- "data/dbs/GBIF_data_24Jan2016.db"
# GBIF_con <- dbConnect(SQLite(), GBIF_db)

Species$comb_name <- paste0(Species$CommonName, " (",
                            Species$ScientificName, ")")
avail_spp <- c("All", levels(as.factor(Species$comb_name)))

# #############################################################################
# # Now, load and prep each of the shapefiles...
huc_shp <- readShapePoly("data/shp/HUC8_WGS84_simple0-01/HUC8_WGS84_simple0-01.shp",
                         proj4string=CRS("+proj=merc +lon_0=90w"))
huc_shp@data$HUC_CODE <- as.character(huc_shp@data$HUC_CODE)

# #############################################################################
# # Now, get the extents and midpoint of each of the shapefiles for zooming in
# getExtents <- function(x) {
#     extent <- as.vector(bbox(x))
#     xmin <- extent[1]
#     ymin <- extent[2]
#     xmax <- extent[3]
#     ymax <- extent[4]
#     xmid <- (xmin + xmax) / 2
#     ymid <- (ymin + ymax) / 2
#     return(list(xmin=xmin, ymin=ymin, xmax=xmax, ymax=ymax, xmid=xmid, ymid=ymid))
# }
# 
#############################################################################
# update colors for CSS
validColors_2 <- c("red", "yellow", "aqua", "blue", "light-blue", "green",
                   "navy", "teal", "olive", "lime", "orange", "orange_d", "fuchsia",
                   "purple", "maroon", "black")

validateColor_2 <- function(color) {
    if (color %in% validColors_2) {
        return(TRUE)
    }
  
    stop("Invalid color: ", color, ". Valid colors are: ",
         paste(validColors_2, collapse = ", "), ".")
}



# Bargraphs for the section 7 Shiny app.
# Copyright (C) 2015 Defenders of Wildlife, jmalcom@defenders.org

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

axis_lab <- function(x) {
    switch(x,
        "fy" = "Fiscal Year",
        "STATE" = "State",
        "cnt_st" = "County (state)",
        "practice_name" = "Work type (practice)",
        "Contract_Obligation" = "Contract amt. ($)",
        "Practice_Obligations" = "Practice amt.",
        "total_treated_acres" = "Total treated ac.",
        "number_contracts" = "N contracts")
}

#############################################################################
# Species summary barchart
make_bargraph <- function(dat, x, y, height="475px", chartHeight="65%") {
    cur_dat <- make_top_25_df(dat, x, y)
    chart2 <- gvisColumnChart(cur_dat,
                  xvar=axis_lab(x),
                  yvar=axis_lab(y),
                  options = list(height=height,
                                 colors="['#0A4783']",
                                 legend="{position: 'none'}",
                                 vAxis=paste("{title: '", axis_lab(y), "'}")
                  )
             )
    chart2
}

make_scatterp <- function(dat, x, y, height="475px", chartHeight="65%") {
    cur_dat <- make_scatterp_df(dat, x, y)
    chart2 <- gvisScatterChart(cur_dat,
                  options = list(height=height,
                                 colors="['#0A4783']",
                                 legend="{position: 'none'}",
                                 hAxis=paste0("{title: '", axis_lab(x), "', 
                                             logScale: 'true'}"),
                                 vAxis=paste0("{title: '", axis_lab(y), "', 
                                             logScale: 'true'}"),
                                 dataOpacity=0.3
                  )
             )
    chart2
}

# make_hist <- function(dat, y, height="475px", chartHeight="65%") {
#     cur_dat <- make_hist_df(dat, y)
#     cur_dat <- data.frame(cur_dat[,2])
#     chart <- gvisHistogram(cur_dat,
#                  options=list(height=height,
#                               colors="['#0A4783']",
#                               legend="{position: 'none'}",
#                               dataOpacity=0.5)
#     )
#     chart
# }



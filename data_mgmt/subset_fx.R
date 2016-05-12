# Functions to subset an input dataset, x.
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

##############################################################################
# Return a subset of the WLFW db (x) based on a suite of variables.
##############################################################################
sub_sp_df <- function(x, sel_species) {
    if(sel_species == "All") {
        x <- get_random_sp(x)
    } else {
        x <- x[Species$comb_name == sel_species, ]
    }
    return(x)
}

get_random_sp <- function(x) {
    x <- x[sample(nrow(x), 1), ]
    return(x)
}

get_GBIF <- function(sub) {
    table <- gsub(" ", "_", sub$ScientificName, fixed=TRUE)
    table <- gsub(".", "", table, fixed=TRUE)
    table <- gsub("(", "", table, fixed=TRUE)
    table <- gsub(")", "", table, fixed=TRUE)
    if(table %in% dbListTables(GBIF_con)) {
        query <- paste("SELECT * FROM", table)
        res <- dbSendQuery(GBIF_con, query)
        GBIF_data <- dbFetch(res)
        return(GBIF_data)
    }
    return(NULL)
}

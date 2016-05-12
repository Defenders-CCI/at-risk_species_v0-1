# Functions to create dataframes, typically for plot generation.
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

############################################################################
# Create the df for mapping the selected values
make_map_df <- function(sub, shp) {
    cur_huc_list <- sub$HUC8
    # observe({ print(head(cur_huc_list)) })
    nvar <- length(shp@data)
    shp@data$pres_abs <- rep(0, length(shp@data[[nvar]]))
    shp@data$pres_abs <- ifelse(shp@data$HUC_CODE %in% sub$HUC8,
                                   1,
                                   0)
    # observe({ print(head(shp@data)) })
    return(shp)
}

############################################################################
# Create a small dataframe for top 25 species bar plot
tooltips <- function(sp, dol, src) {
    paste0("<div style='padding:5px 5px 5px 5px;'><b>", 
           sp, '</b><br>', src, ":<br>$", prettyNum(dol, big.mark=","), "</div>")
}

make_top_25_df <- function(sub, x, y) {
    res <- tapply(sub[[y]], INDEX=sub[[x]], FUN=sum, na.rm=TRUE)
    if (x == "fy") {
        res_df <- data.frame(num=rep(1:length(res)))
        res_df[[axis_lab(x)]] <- names(res)
        res_df[[axis_lab(y)]] <- as.vector(res)
    } else {
        end <- ifelse(length(res) > 24, 25, length(res))
        res <- sort(res, decreasing=TRUE)[1:end]
        res_df <- data.frame(num=rep(1:length(res)))
        res_df[[axis_lab(x)]] <- names(res)
        res_df[[axis_lab(y)]] <- as.vector(res)
    }
    return(res_df)
}

make_scatterp_df <- function(sub, x, y) {
    if (y != "number_contracts") {
        res_df <- data.frame(num=c(1:length(sub[[x]])))
        res_df[[axis_lab(x)]] <- sub[[x]]
        res_df[[axis_lab(y)]] <- sub[[y]]
        res_df <- res_df[, -1]
    } else {
        n_items <- table(sub$contract_id)
        dol_contract <- tapply(sub$Contract_Obligation,
                               INDEX=sub$contract_id,
                               FUN=mean, na.rm=TRUE)
        res_df <- data.frame(num=c(1:length(sub[[x]])))
        res_df[[axis_lab(x)]] <- as.vector(dol_contract)
        res_df[[axis_lab(y)]] <- as.vector(n_items)
        res_df <- res_df[, -1]
    }
    return(res_df)
}

# make_hist_df <- function(sub, y) {
#     if (y != "number_contracts") {
#         res_df2 <- data.frame(num=c(1:length(sub[[y]])))
#         res_df2[[axis_lab(y)]] <- sub[[y]]
#     } else {
#         n_contr <- table(sub$contract_id)
#         res_df2 <- data.frame(num=c(1:length(n_contr)))
#         res_df2[[axis_lab(y)]] <- as.vector(n_contr)
#     }
#     return(res_df2)
# }


# Functions to fetch GBIF records for SE_Candidates.
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
# 
get_gbif <- function(x) {
    if(x != "") {
        cur_search <- occ_search(scientificName=x, 
                                 hasCoordinate=TRUE,
                                 limit=1000)
        if(!is.null(dim(cur_search$data)[0])) {
            cur_dat <- cur_search$data
            img_dat <- get_image_data(cur_search)
            new_dat <- cur_dat %>% full_join(img_dat, key="key")
            return(new_dat)
        } else {
            return(NULL)
        }
    } else {
        return(NULL)
    }
}

get_image_data <- function(x) {
    n_ent <- length(x$media)
    res <- data.frame(key=rep(NA, n_ent),
                      img=rep(NA, n_ent),
                      lic=rep(NA, n_ent),
                      holder=rep(NA, n_ent))
    for(i in 1:n_ent) {
        if(length(x$media[[i]]) > 0) {
            cur_dat <- c(NA, NA, NA, NA)
            key <- ls(x$media[[i]])
            if(!is.null(key)) { cur_dat[1] <- key }
            img <- x$media[[i]][[1]][[1]]$identifier
            if(!is.null(img)) { cur_dat[2] <- img }
            lic <- x$media[[i]][[1]][[1]]$license
            if(!is.null(lic)) { cur_dat[3] <- lic }
            holder <- x$media[[i]][[1]][[1]]$rightsHolder
            if(!is.null(holder)) { cur_dat[4] <- holder }
            res[i, ] <- cur_dat
        } else {
            res[i, ] <- c(NA, NA, NA, NA)
        }
    }
    res$key <- as.numeric(res$key)
    return(res[complete.cases(res), ])
}


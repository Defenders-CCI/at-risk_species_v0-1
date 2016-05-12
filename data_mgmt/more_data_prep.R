# Helper functions for shiny app.
# Copyright (c) 2015 Defenders of Wildlife, jmalcom@defenders.org

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

library(readxl)

load("data/WLFW_data_semifinal.RData")

# dang it, I never brought in that new county and date data...
new_dat <- read_excel("data/LPCandGSG2014withCountiesContractDates.xlsx",
                      sheet=2,
                      col_names=TRUE)
head(new_dat)

# returns string w/o leading or trailing whitespace
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
new_dat$cnty_nm <- trim(new_dat$cnty_nm)

new_sub <- data.frame(contract_id = new_dat$contract_id,
                      cnty_nm = new_dat$cnty_nm,
                      start_date = new_dat$start_date,
                      expiration_date = new_dat$expiration_date)
dim(new_sub)

contracts <- as.character(levels(as.factor(new_sub$contract_id)))
full_old <- full[!(as.character(full$contract_id) %in% contracts), ]
full_old$cnty_nm <- as.character(full_old$cnty_nm)
full_old$start_date <- as.character(full_old$start_date)
full_old$expiration_date <- as.character(full_old$expiration_date)

full_new <- full[as.character(full$contract_id) %in% contracts, ]
sum(as.character(full_new$contract_id) != as.character(new_sub$contract_id))

full_new$cnty_nm <- as.character(new_sub$cnty_nm)
full_new$start_date <- new_sub$start_date
full_new$expiration_date <- new_sub$expiration_date

tmp <- rbind(full_old, full_new)
dim(tmp)
full <- tmp
save(full, file="data/WLFW_data_near-final_31Dec2015.RData")

# First fix up the county names and paste with state
simpleCap <- function(x) {
    if (!is.na(x)) {
        s <- strsplit(x, " ")[[1]]
        paste(substring(s, 1,1), tolower(substring(s, 2)),
              sep="", collapse=" ")
    } else {
        NA
    }
}

full$county_name <- sapply(full$cnty_nm, FUN=simpleCap)
full$county_name <- gsub("St ", "St. ", full$county_name, fixed=TRUE)
full$county_name <- gsub("Mccone", "McCone", full$county_name, fixed=TRUE)
full$county_name <- gsub("Miami-dade", "Miami-Dade", full$county_name, fixed=TRUE)
full$county_name <- gsub("Dona Ana", "Doña Ana", full$county_name, fixed=TRUE)
full$state_name <- as.character(full$st_nm)
full$county_state <- paste(full$county_name, full$state_name)
head(full$county_state)

fixFIPS <- function(x) {
    if (nchar(x) == 4) {
        return(paste0("0", x))
    } else {
        return(x)
    }
}

# Now get the TIGER data to pull in GEOIDs
TIGER <- read.csv("data/US_states_counties_TIGER_v1-1.tab",
                  header=TRUE,
                  sep="\t")
TIGER$GEOID <- as.character(TIGER$GEOID)
TIGER$GEOID <- sapply(TIGER$GEOID, FUN=fixFIPS)
head(TIGER$GEOID)
TIGER$county_state <- paste(TIGER$NAME, TIGER$STATE)

# Now let's try some merging:
dim(full)
tmp <- merge(full, TIGER, by="county_state")
dim(tmp)

full_co_st <- as.character(levels(as.factor(full$county_state)))
TIGR_co_st <- as.character(levels(as.factor(TIGER$county_state)))
full_co_st[!(full_co_st %in% TIGR_co_st)]

full$dups <- duplicated(full$contract_item_id)
sum(full$dups)

tmp$dups <- duplicated(tmp$contract_item_id)
sum(tmp$dups)

# The extra 10 rows come from both Roanoke city and Roanoke county in the FIPS
# table; will just keep the County entries for mapping
t2 <- tmp[tmp$NAMELSAD != "Roanoke city", ]
dim(t2)

full <- t2
save(full, file="data/WLFW_data_final_31Dec2015.RData")



# Central landing page for SE_Candidates app
# Copyright (C) 2016 Defenders of Wildlife, jmalcom@defenders.org

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

source("pages/species_page.R")
# source("pages/area_page.R")
source("pages/data_page.R")

###############################################################################
# Central landing page for sec7 basic app
results_page <- {
    navbarPage("Southeast Candidate Species",
        species_page,
        # chart_page,
        data_page,
        inverse=FALSE,
        position="fixed-top"
    )
}

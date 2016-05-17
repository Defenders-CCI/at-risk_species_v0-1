# Top-level UI organization for SE_Candidates app.
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
# source("pages/data_page.R")

#############################################################################
# Define the header and sidebar (disabled)
header <- dashboardHeader(disable=TRUE)
sidebar <- dashboardSidebar(disable=TRUE)

#############################################################################
# Define the page(s) with dashboardBody
body <- dashboardBody(
    bsModal(id="about",
            title="What is this?",
            trigger="get_started",
            includeMarkdown("txt/getting_started.md"),
            size="large"
    ),
    bsModal(id="datatable_help",
            title="Using the data table",
            trigger="table_help",
            HTML("<ul><li>Hover over the table and scroll right to see additional columns.</li>
                      <li>Search each column using the boxes at the top of the columns.</li>
                      <li>Sort the table by column using the arrows above each column.</li>
                      <li>Show/hide additional columns using the button at right.</li>
                 </ul>"),
            size="small"
    ),
    bsModal("mod_big_chart",
            title="",
            trigger="big_chart",
            size="large",
            htmlOutput("large_chart"),
            fluidRow(
                column(2),
                column(10,
                    helpText("Placeholder.")
                )
            )
    ),
    navbarPage("Southeast Candidate Species",
        species_page,
        tabPanel("About",
            column(4),
            column(4,
                br(), br(),
                includeMarkdown("txt/getting_started.md")
            ),
            column(4)
        ),
        # chart_page,
        # data_page,
        inverse=TRUE,
        position="fixed-top"
    )
)

dashboardPage(header, sidebar, body, skin="blue")
# shinyUI(body)

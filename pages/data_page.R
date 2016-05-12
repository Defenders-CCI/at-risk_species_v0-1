# Page for users to get the selected data.
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

###############################################################################
# Page for users to get the selected data.
data_page <- {
    tabPanel(title="Data",
        fluidRow(
            div(style="overflow-x: scroll; background-color: #FFFFFF;
                       padding-left: 15px", 
                fluidRow(
                    column(4,
                        h3("Selected data"),
                        div(class="btn-group",
                            bsButton("table_help",
                                     label="Table Help",
                                     style="info"
                            ),
                            tipify(
                                downloadButton("download_data", "Download (tab-sep.)"),
                                title="Download the data subset that is selected on the map page."
                            # ),
                            # tipify(
                            #     downloadButton("download_metadata", "Download metadata"),
                            #     title="Download metadata for the section 7 data (in JSON format)"
                            )
                        )
                    )
                ),
                br(), hr(),
                DT::dataTableOutput("selected_data")
            )
        ),
        br(),
        fluidRow(
            column(3),
            column(6,
                div(HTML(defenders_cc()), style=center_text)
            ),
            column(3)
        )
    )
}

# Species overview page for SE_Candidates app.
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

# openSans <- "<link href='https://fonts.googleapis.com/css?family=Open+Sans:300,400' 
#             rel='stylesheet' type='text/css'>"
# fontAwesome <- "<link rel='stylesheet' 
#                href='http://maxcdn.bootstrapcdn.com/font-awesome/4.3.0/css/font-awesome.min.css'/>"

species_page <- {
    tabPanel(
        title="Species",
        br(), br(), br(),
        column(1),
        column(10,

        # Species name, selection
        fluidRow(
            box(width=12,
                column(9, h2(textOutput("cur_species"))),
                column(3,
                tipify(
                    el=selectInput(
                        inputId="sel_species",
                        label="Select a species",
                        choices=avail_spp,
                        width="98%"),
                    title="Scroll through list or delete and start typing a name",
                    placement="left"
                    )
                )
            )
        ),

        # Photo & status, map
        fluidRow(
            column(4,
                fluidRow(
                    box(width=12,
                        # imageOutput("aline", height=5),
                        div(htmlOutput("curPhoto"),
                            style="margin:auto;"),
                        helpText(textOutput("cur_photo_credit"),
                                 style="background-color:white;"),
                        imageOutput("aline", height=5),
                        htmlOutput("cur_record_info") #,
                        # imageOutput("bline", height=1)
                    )
                ),
                fluidRow(
                    box(title="Status summary",
                        status="danger",
                        solidHeader=FALSE,
                        height=NULL,
                        width=12,
                        collapsible=FALSE,
                        collapsed=FALSE,
                        htmlOutput("stat_summary")
                    )
                )
            ),
            column(8,
                box(width=NULL,
                    div(style="position:static",
                        leafletOutput("sp_map", height=425),
                        absolutePanel(id = "rezoom", class = "panel panel-default", 
                            draggable = FALSE, top = "5%", left = "auto", 
                            right = 20, bottom = "auto", width = "auto", 
                            height = "auto",
                            bsButton(inputId="map_rezoom",
                                label=icon("home", lib="font-awesome"),
                                size="small")
                        )
                    ),

                    column(4,
                        checkboxInput("show_points",
                            label="Show point occurrences?",
                            value=TRUE
                        )
                    ),
                    column(4,
                        checkboxInput("cluster_mark",
                            label="Cluster markers?",
                            value=TRUE
                        )
                    ),
                    column(4,
                        checkboxInput("show_huc",
                            label="Show watershed occ.? (HUC8)",
                            value=FALSE
                        )
                    ),
                    helpText("If no points or blue-highlighted watersheds show up (and are checked!), then locality information is not available.")
                )
            )
        ),

        # More info
        fluidRow(
            column(6,
                box(title="Species Information",
                    status="success",
                    solidHeader=FALSE,
                    height=NULL,
                    width=NULL,
                    collapsible=FALSE,
                    collapsed=FALSE,
                    helpText("Relevant biological information would be added
                             here, for example:"),
                    dataTableOutput("habitats")
                )
            ),
            column(6,
                box(title="Other resources",
                    status="info",
                    solidHeader=FALSE,
                    height=NULL,
                    width=NULL,
                    collapsible=TRUE,
                    collapsed=FALSE,
                    htmlOutput("ecos_link"),
                    br(),
                    htmlOutput("eol_link"),
                    br(),
                    htmlOutput("ns_link")
                )
            )
        ),
        
        # footer area
        fluidRow(
            column(2),
            column(8,
                HTML("<p style='text-align:center'>This web app is based on data compiled and made available by Roy Hewitt of the US Fish and Wildlife Service's Southeast Region (Region 4). Please visit <a href='http://www.fws.gov/southeast/candidateconservation/finder.html'>their web page</a> for more information.")
            ),
            column(2)
        ),
        fluidRow(
            column(3),
            column(6,
                div(HTML(defenders_cc()), style=center_text)
            ),
            column(3)
        ),
        column(1))
    )
}

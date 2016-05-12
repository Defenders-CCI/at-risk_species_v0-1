# Generic server-side code for the SE_Candidates app.
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

source("server_pages/server_species_page.R")
# source("server_pages/server_chart_page.R")
# source("server_pages/server_alt_map_page.R")

#############################################################################
# Define the server with calls for data subsetting and making figures
#############################################################################
shinyServer(function(input, output, session) {

    # The basic reactive subsetting functions...separate functions for each
    # of the pages.
    sel_sp_dat <- reactive({
        sub_sp_df(Species, input$sel_species)
    })

    # GBIF_data <- reactive({
    #     get_GBIF(sel_sp_dat())
    # })

    output$defenders <- renderImage({
        width <- session$clientData$output_defenders_width
        if (width > 100) {
            width <- 100
        }
        list(src = "www/01_DOW_LOGO_COLOR_300.png",
             contentType = "image/png",
             alt = "Overview of section 7 consultation",
             a(href = "http://www.defenders.org"),
             width=width)
    }, deleteFile=FALSE)

    # Call the files with server functions broken out by page
    server_species_page(input, output, sel_sp_dat, GBIF_data, session)

    # ###########################################################################
    # # The following function calls are used for getting the data selections
    # # for download.

    # sel_cols <- c(1:19, 21:23, 29)
    # 
    # output$selected_data <- DT::renderDataTable(
    #     selected()[, sel_cols],
    #     rownames=FALSE,
    #     filter="top", 
    #     extensions="ColVis", 
    #     options = list(dom = 'C<"clear">lfrtip')
    # )
    # 
    # output$download_data <- downloadHandler(
    #     filename=function() {
    #         "selected_data.tab"
    #     },
    #     content=function(file) {
    #         data_to_get <- selected()[, sel_cols]
    #         for_write <- data_to_get
    #         write.table(for_write, 
    #                     file=file, 
    #                     sep="\t",
    #                     row.names=FALSE,
    #                     quote=FALSE)
    #     }
    # )
    # 
    # # output$download_metadata <- downloadHandler(
    # #     filename=function() {
    # #         "section_7_metadata.json"
    # #     },
    # #     content=function(file) {
    # #         sink(file)
    # #         cat(the_metadata)
    # #         sink()
    # #     }
    # # )

})

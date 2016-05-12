# Server-side code for the SE_Candidates species page.
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

source("data_mgmt/get_gbif.R")

###########################################################################
# Server-side code for the section 7 app basic single-view page
###########################################################################
server_species_page <- function(input, output, selected, GBIF_data, session) {

    cur_lnk <- reactive({
        return(list(gbif=selected()$GBIF,
                    eol=selected()$EOL,
                    ecos=selected()$ECOScode))
    })

    clean_sci <- function(x) {
        gsub(" pop. 1", "", x)
    }

    current_gbif <- reactive({
        withProgress(message="Getting GBIF records",
                     detail="Please be patient...",
                     value=0.25, {
            res <- get_gbif(clean_sci(selected()$ScientificName))
            setProgress(1)
        })
        return(res)
    })

    # Return the combined common (sci) of the current species
    current_sel_species <- reactive({
        if(input$sel_species == "All") {
            return(selected()$comb_name)
        } else {
            return(input$sel_species)
        }
    })

    output$cur_species <- renderText({
        current_sel_species()
    })

    output$aline <- renderImage({
        width <- session$clientData$output_aline_width
        list(src = "www/line-01.png",
             contentType = "image/png",
             alt = "",
             width=width)
    }, deleteFile=FALSE)

    output$bline <- renderImage({
        width <- session$clientData$output_bline_width
        list(src = "www/line-01.png",
             contentType = "image/png",
             alt = "",
             width=width)
    }, deleteFile=FALSE)

    # pull an image from GBIF records, if available
    get_img_data <- function() {
        GBdat <- current_gbif()
        if(!is.null(GBdat)) {
            nona <- GBdat[!is.na(GBdat$lic), ]
            case <- nona[sample(nrow(nona), 1), ]
            cur_img <- case$img
            cur_lic <- case$lic
            cur_own <- case$holder
            cur_sci <- case$scientificName
            cur_rights <- case$rights
            cur_ref <- case$references
            res <- list(img=cur_img, lic=cur_lic, own=cur_own,
                        sci=cur_sci, right=cur_rights, ref=cur_ref)
            return(res)
        }
        # add code to search other resources for images, or to pull in from
        # our upcoming End. Sp. Images database
        return(list(img=NULL, lic=NULL, own=NULL, sci=NULL, right=NULL,
                    ref=NULL))
    }

    output$curPhoto <- renderText({
        width <- session$clientData$output_aline_width
        img_dat <- get_img_data()
        
        # get the photo credit output
        output$cur_photo_credit <- renderText({
            if(length(img_dat$right) > 0) {
                if(!is.na(img_dat$right)) {
                    img_dat$right
                } else {
                    "No copyright information available."
                }
            } else { 
                ""
            }
        })

        # get the GBIF/iNat record output
        output$cur_record_info <- renderText({
            if(length(img_dat$ref) > 0) {
                if(!is.na(img_dat$ref)) {
                    paste0("<a href='", img_dat$ref, "' target='_blank'>Record for photo</a>")
                } else {
                    "No record information available."
                }
            } else { 
                ""
            }
        })

        # Now get the image, if possible
        image <- img_dat$img
        if(length(image) > 0 & !is.null(image)) {
            if(grep("http:", image, fixed=TRUE)) {
                res <- paste0("<img src='", image, 
                              "' alt='' ",
                              "width='", width, "'>")
                return(res)
            } else {
                return(paste0("<p>No photos of ", selected()$comb_name, " found.</p>",
                              "Try a <a href='https://www.google.com/search?site=&tbm=isch&q=",
                              selected()$comb_name, "' target='_blank'>Google image search</a>"))
            }
        } else {
            return(paste0("<p>No photos of ", selected()$comb_name, " found.</p>",
                          "Try a <a href='https://www.google.com/search?site=&tbm=isch&q=",
                          selected()$comb_name, "' target='_blank'>Google image search</a>"))
        }
    })

    #######################################################################
    # Some reactive values needed in several places for making the map 
    cur_zoom <- reactive({
        if (!is.null(input$map_zoom)) {
            input$map_zoom
        } else {
            4
        }
    })

    observe({
        input$map_rezoom
        leafletProxy("sp_map") %>%
            setView(lng=-95, 
                    lat=38, 
                    zoom = 4)
    })
        

    cur_poly <- reactive({
        switch(input$show_poly,
            "None" = allsp_shp,
            "State" = BOTU_shp,
            "HUC8" = GOTO_shp,
            "LCC" = GRSG_shp)
    })

    #######################################################################
    # Now to make the map
    output$sp_map <- renderLeaflet({ 
		cur_map <- leaflet() %>%
                   setView(lng=-95, 
                           lat=38, 
                           zoom = 4) %>%
                   mapOptions(zoomToLimits = "first")
        print( cur_map )
        return(cur_map)
    })

    # proxy to add/change the basemap
    observe({ 
        leafletProxy("sp_map") %>% 
            clearTiles() %>% 
            addProviderTiles("Stamen.TonerLite") 
    })

    point_vis <- reactive({
        if(input$show_points == TRUE) { TRUE } else { FALSE }
    })

    # proxy to add occurrences, if available
    observe({
        if(point_vis()) {
            GBdat <- current_gbif()
            if(!is.null(GBdat)) {
                ref <- rep("", length(GBdat$references))
                if(!is.null(GBdat$references)) {
                    ref <- ifelse(!is.na(GBdat$references),
                                  paste0("<a href='", 
                                         GBdat$references, 
                                         "' target='_blank'>Specimen record</a>"),
                                  paste0("<a href=http://www.gbif.org/occurrence/",
                                         GBdat$key,
                                         " target='_blank'>GBIF record</a>"))
                } else {
                    ref <- paste0("<a href=http://www.gbif.org/occurrence/",
                                  GBdat$key,
                                  " target='_blank'>GBIF record</a>", ref)
                }
                coord_dat <- data.frame(long=GBdat$decimalLongitude,
                                        lat=GBdat$decimalLatitude,
                                        popup=ref)

                if(input$cluster_mark) {
                    leafletProxy("sp_map", data=coord_dat) %>%
                        clearMarkerClusters() %>%
                        addMarkers(lng=~long, lat=~lat, popup=~popup,
                                   clusterOptions=markerClusterOptions())
                } else {
                    leafletProxy("sp_map", data=coord_dat) %>%
                        clearMarkers() %>%
                        addMarkers(lng=~long, lat=~lat, popup=~popup)
                }
            } else {
                if(input$cluster_mark) {
                    leafletProxy("sp_map") %>%
                        clearMarkerClusters()
                } else {
                    leafletProxy("sp_map") %>%
                        clearMarkers()
                }
            }
        } else {
            if(input$cluster_mark) {
                leafletProxy("sp_map") %>%
                    clearMarkerClusters()
            } else {
                leafletProxy("sp_map") %>%
                    clearMarkers()
            }
        }
    })

    observe({
        if(input$cluster_mark) {
            leafletProxy("sp_map") %>%
                clearMarkers()
        } else {
            leafletProxy("sp_map") %>%
                clearMarkerClusters()
        }
    })

    huc_vis <- reactive({
        if(input$show_huc == TRUE) { TRUE } else { FALSE }
    })

    # proxy to add/change polygons
    observe({ 
        if(huc_vis()) {
            withProgress(message="Calculating watershed layer...",
                         value=0.25, {
                popupFormat <- paste0("<strong>", huc_shp@data[[6]], "</strong>", 
                                      "<br>HUC8: ", huc_shp@data[[5]], "<br>")

                huc_dat <- selected() %>% left_join(HUC8, by="Species_ID")
                if(!is.na(huc_dat$HUC8[1])) {
                    cur_shp_dat <- make_map_df(huc_dat, huc_shp)
                    leafletProxy("sp_map", data=cur_shp_dat) %>%
                        clearShapes() %>%
                        addPolygons(
                            fill = TRUE,
                            fillColor = ~colorNumeric(
                                            palette=c(rgb(0.3, 0.3, 0.3, 0.05), 
                                                      rgb(0, 0, 1, 1)), 
                                            domain=range(cur_shp_dat@data$pres_abs, 
                                                         na.rm=T)
                                        )(cur_shp_dat@data$pres_abs),
                            stroke = TRUE, 
                            weight = 1, 
                            color = "#ffffff",
                            popup = popupFormat
                        )
                } else {
                    leafletProxy("sp_map", data=huc_shp) %>%
                        clearShapes() %>%
                        addPolygons(
                            fill=TRUE,
                            fillColor=rgb(0,0,0,0.05),
                            stroke = TRUE, 
                            weight = 1, 
                            color = "#ffffff",
                            popup = popupFormat
                        )
                }
             })
        } else {
            leafletProxy("sp_map") %>%
                clearShapes()
        }
    })

    # # proxy to add/change the legend, conditioned on viz selection
    # observe({
    #     make_legend_title <- function(sp, res) {
    #         lede <- "<p style='text-align:center;'>"
    #         paste(lede, res, "for<br>", sp, "</p>")
    #     }

    #     cur_title <- switch(input$cur_species,
    #         "All" = make_legend_title("all species", input$cur_response),
    #         "botu" = make_legend_title("bog turtle", input$cur_response),
    #         "goto" = make_legend_title("gopher tortoise", input$cur_response),
    #         "grsg" = make_legend_title("Greater Sage-Grouse", input$cur_response),
    #         "gwwa" = make_legend_title("Golden-winged Warbler", input$cur_response),
    #         "lpch" = make_legend_title("Lesser Prairie-Chicken", input$cur_response),
    #         "neco" = make_legend_title("New England cottontail", input$cur_response),
    #         "swfl" = make_legend_title("SW Willow Flycatcher", input$cur_response)
    #     )

    #     cur_prefix <- switch(input$cur_response,
    #         "Total spending" = "  $",
    #         "Acres treated" = "  ",
    #         "Dollars / acre" = "  $",
    #         "# contracts" = "  "
    #     )

    #     cur_suffix <- switch(input$cur_response,
    #         "Total spending" = "",
    #         "Acres treated" = " ac.",
    #         "Dollars / acre" = " / ac.",
    #         "# contracts" = " contracts"
    #     )

    #     range_vals <- range(cur_data()@data[[cur_output_num()]], na.rm=T)
    #     values <- cur_data()@data[[cur_output_num()]]
    #     leafletProxy("map", data=cur_data()) %>%
    #         clearControls() %>%
    #         addLegend("bottomleft",
    #                   pal=colorNumeric(palette="RdYlBu", 
    #                                    domain=range_vals),
    #                   values=~values,
    #                   title=cur_title,
    #                   labFormat=labelFormat(prefix=cur_prefix,
    #                                         suffix=cur_suffix),
    #                   opacity=0.8)
    # })

    # observe({
    #     leafletProxy("map", data=cur_data()) %>%
    #         fitBounds(cur_bnds()$xmin, cur_bnds()$ymin,
    #                   cur_bnds()$xmax, cur_bnds()$ymax)
    # })

    output$stat_summary <- renderText({
        with_cont <- selected() %>% left_join(Contacts,
                                              by=c("LeadOffice" = "Office"))
        emails <- ifelse(length(with_cont$Email) > 1,
                         paste(as.character(with_cont$Email), collapse=";"),
                         as.character(with_cont$Email))
        res <- paste0("<p><strong>Review status: </strong>", selected()$Status,
               "</p>",
               "<p><strong>Lead office: </strong>", selected()$LeadOffice,
               "</p>")
        if(!is.na(emails)) {
            email_cat <- paste0("<a href='mailto:", emails,
                                    "'>Email</a>")
            res <- paste0(res, "<p><strong>Contact: </strong>", 
                          email_cat, "</p>")
        }
        res
    })

    # Make the DataTables table of habitats/regions used
    output$habitats <- renderDataTable({
        with_habs <- selected() %>% left_join(BroadlyDefinedHabitats,
                                              by="Species_ID")
        if(!is.na(with_habs$BroadHabitatName[1])) {
            if(length(with_habs$BroadHabitatName) == 1) {
                cur_hab <- as.character(with_habs$BroadHabitatName)
            } else {
                cur_hab <- paste(as.character(with_habs$BroadHabitatName), 
                                 collapse="; ")
            }
        } else {
            cur_hab<- "No information"
        }

        # LCC
        with_lccs <- selected() %>% left_join(LCC,
                                              by="Species_ID")
        if(length(with_lccs$LCCName) > 1) {
            cur_lcc <- paste(as.character(with_lccs$LCCName), collapse="; ")
        } else {
            cur_lcc <- as.character(with_lccs$LCCName)
        }

        # Pysiographic province
        with_phys <- selected() %>% left_join(Physiographic,
                                              by="Species_ID")
        if(length(with_phys$PhysiographicName) > 1) {
            cur_phy <- paste(as.character(with_phys$PhysiographicName), 
                             collapse="; ")
        } else {
            cur_phy <- as.character(with_phys$PhysiographicName)
        }

        vars <- c("Habitats used", 
                  HTML("<a href='http://lccnetwork.org/' target='_blank'>Landscape Cons Coop</a>"),
                  "Physiographic provinces")
        cats <- c(cur_hab, cur_lcc, cur_phy)
        res <- data.frame(vars, cats)
        names(res) <- c("", "")
        res
    },
        options = list(paging=FALSE, searching=FALSE, info=FALSE, ordering=FALSE),
        rownames = FALSE,
        escape = FALSE
    )
    
    output$ecos_link <- renderText({
        paste0("<a href='http://ecos.fws.gov/tess_public/profile/speciesProfile.action?spcode=",
               selected()$ECOScode, "' class='btn btn-primary' role='button' target='_blank'>ECOS</a>",
               "<span style='padding-left: 2em'>",
               "Information from FWS's ECOS website</span>")
    })
               
    output$eol_link <- renderText({
        paste0("<a href='http://eol.org/pages/",
               selected()$EOL, "' class='btn btn-primary' role='button' target='_blank'>EOL</a>",
               "<span style='padding-left: 2em'>",
               "Information from the Encyclopedia of Life</span>")
    })
    
    output$ns_link <- renderText({
        search <- gsub(" ", "+", clean_sci(selected()$ScientificName))
        paste0("<a href='http://explorer.natureserve.org/servlet/NatureServe?searchName=",
               search, "' class='btn btn-primary' role='button' target='_blank'>NatureServe</a>",
               "<span style='padding-left: 2em'>",
               "Information from NatureServe</span>")
    })

}

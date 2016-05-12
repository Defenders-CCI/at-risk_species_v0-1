# Server-side code for the section 7 app basic comparative-view page
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

###########################################################################
# Server-side code for the section 7 app basic single-view page
###########################################################################
server_chart_page <- function(input, output, selected_2, session) {
    state_txt <- reactive({
        res <- ifelse(input$state_2 == "All",
                      "all states",
                      input$state_2)
        res
    })

    years_txt <- reactive({
        res <- ifelse(input$years_2 == "All",
                            paste0("all data from ", minYear, " - ", maxYear),
                            paste0("data from ", input$years_2))
        res
    })

    group_txt <- reactive({
        res <- ifelse(input$groups_2 == "All",
                      "all taxonomic groups",
                      input$groups_2)
        res
    })

    species_txt <- reactive({
        res <- ifelse(input$species_2 == "All",
                      "all species",
                      input$species_2)
        res
    })

    sources_txt <- reactive({
        res <- ifelse(input$sources_2 == "All",
                      "all funding sources",
                      input$sources_2)
        res
    })

    ### Get the text and chart for expenditures among species
    output$spp_chart_text <- renderText({
        if (input$species_2 == "All") {
            cur_exp <- tapply(selected_2()$grand_per_cnty,
                              INDEX=selected_2()$sp,
                              FUN=sum, na.rm=TRUE)
            which_dat <- "the current data selection"
        } else {
            cur_exp <- tapply(full$grand_per_cnty,
                              INDEX=full$sp,
                              FUN=sum, na.rm=TRUE)
            which_dat <- "all expenditure data"
            exp_rnk <- rank(-cur_exp)
            sp_graphd <- ifelse(exp_rnk[input$species_2] < 26,
                                "shown",
                                "ranked too low to show")
        }

        cur_gini <- format(gini(cur_exp), digits=3)
        if (cur_gini >= 0.75) {
            gini_rel <- "much more skewed than"
        } else if (cur_gini > 0.45 & cur_gini < 0.75) {
            gini_rel <- "a bit more skewed than"
        } else if (cur_gini <= 0.45 & cur_gini > 0.25) {
            gini_rel <- "a bit less skewed than"
        } else if (cur_gini <= 0.25 & cur_gini > 0) {
            gini_rel <- "much less skewed than"
        } else {
            gini_rel <- "not really comparable to"
        }

        cur_med <- median(cur_exp, na.rm=TRUE)
        hilow <- ifelse(cur_exp[input$species_2] >= cur_med,
                        "higher",
                        "lower")

        heading <- ifelse(input$species_2 == "All",
                          "<h3>Spending is uneven across species</h3>",
                          paste0("<h4>Spending is uneven and spending on ", 
                                 input$species_2, " is ", hilow, 
                                 " than the median expenditure on listed species
                                 </h4>"))

        sel_sp <- ifelse(input$species_2 == "All",
                         "",
                         paste0("The currently selected species (", input$species_2,
                                ") is ", sp_graphd, " in the chart."))

        x <- paste0(heading, 
                    "<p>The <a href=https://en.wikipedia.org/wiki/Gini_coefficient>
                    Gini coefficient</a> of ESA expenditures given ", which_dat, 
                    " is ", cur_gini, ".  For comparison, the estimated 
                    Gini coefficient for income distribution in the U.S. is ~0.45; 
                    the distribution of spending among listed species is therefore ", 
                    gini_rel, " than the skew of U.S. incomes.</p>
                    <p>The chart to the right illustrates the sources of funding
                    for the set of top species (up to 25) given the current 
                    selection. ", sel_sp, "</p>")
        return(x)
    })
             
    output$big_spp_chart <- renderGvis({        
        if (input$species_2 == "All") {
            make_species_plot(selected_2(), height="450px")
        } else {
            make_species_plot(full, height="450px")
        }
    })

    output$group_chart_text <- renderText({
        cur_df <- make_tax_group_df(selected_2())
        all_df <- make_tax_group_df(full)
        top_grp <- all_df$group[1]
        bot_grp <- all_df$group[length(all_df$group)]

        cur_notna <- cur_df[!is.na(cur_df$state), ]
        
        if (dim(cur_notna)[1] > 1) {
            focal_txt <- ""
        } else {
            all_tap <- tapply(full$grand_per_cnty, 
                              INDEX=full$Group, 
                              FUN=sum, na.rm=T)
            all_rnk <- rank(-all_tap)
            cur_rnk <- all_rnk[cur_notna$group]
            rel_grp <- ifelse(cur_rnk < (length(all_rnk)/2), "higher", "lower")
            focal_txt <- paste0("Expenditures for the group in the current data 
                                selection, ", cur_notna$group, ", are ", rel_grp,
                                " than most other taxonomic groups.")
        }
            
        x <- paste0("<h3>Some taxonomic groups get much more money than others</h3>
                    
                    <p>The bias towards some groups (e.g., ", top_grp, 
                    ") and away from others (e.g., ", bot_grp, ") is probably
                    very apparent given the current data selection.</p>
                    <p>The chart to the right illustrates the sources of funding
                    for all of the taxonomic groups present in the current 
                    selection. ", focal_txt, "</p>")
        return(x)
    })

    output$big_group_chart <- renderGvis({        
        if (input$groups_2 == "All" & input$species_2 == "All") {
            make_tax_group_plot(selected_2(), height="450px")
        } else {
            make_tax_group_plot(full, height="450px")
        }
    })

    output$group_bubble_text <- renderText({
        cur_df <- make_group_bubble_df(selected_2())
        all_df <- make_group_bubble_df(full)

        if (input$groups_2 == "All" & input$species_2 == "All") {
            top_grp <- cur_df$group[1]
            resort <- cur_df[order(cur_df$`N counties occupied`, decreasing=T), ]
            top_cty <- resort$group[1]
        } else {
            top_grp <- all_df$group[1]
            resort <- all_df[order(all_df$`N counties occupied`, decreasing=T), ]
            top_cty <- resort$group[1]
        }
        cond_1 <- ifelse(top_grp == top_cty,
                         "and",
                         "but")
        conditional <- ifelse(top_grp == top_cty,
                              "sometimes occupies",
                              "often does not occupy")

        cort <- cor.test(log(cur_df$Expenditures), log(cur_df$`N counties occupied`))
        cor1 <- format(cort$estimate, digits=3)
        corp <- format(cort$p.value, digits=3)

        x <- paste0("<h3>Expenditures <span style=font-style:italic;>tend</span> 
                    to correlate with area occupied...</h3>
                    
                    <p>Here the correlation* is ", cor1, 
                    " (<span style=font-style:italic>p</span> = ", corp, "). ", 
                    cond_1, " the group with the highest expenditures (", 
                    top_grp, ") ", conditional, " the greatest area (", top_cty, 
                    ").</p><p>The bubble chart to the right illustrates the 
                    relationship between area occupied (x-axis) and expenditures 
                    (y-axis) for the taxonomic groups in the selected data. The 
                    size of the bubbles is proportional to the number of species 
                    in each group.</p>
                    <p><span style=font-weight:bold;>NOTE</span> that both the 
                    x- and the y-axis are log-scaled!</p><br><p>*On log-transformed
                    values</p>")
        return(x)
    })

    output$group_bubble_chart <- renderGvis({        
        if (input$groups_2 == "All" & input$species_2 == "All") {
            make_group_bubble_plot(selected_2(), height="450px")
        } else {
            make_group_bubble_plot(full, height="450px")
        }
    })

    output$group_bubble_text_2 <- renderText({
        if (input$groups_2 == "All" & input$species_2 == "All") {
            cur_df <- make_group_bubble_df(selected_2())
        } else {
            cur_df <- make_group_bubble_df(full)
        }
        
        mod1 <- lm(log(cur_df$Expenditures) ~ log(cur_df$`N species`))
        smod <- summary(mod1)
        pval <- format(smod$coefficients[8], digits=3, scientific=T)
        fsta <- format(smod$adj.r.squared, digits=3)
        resd <- resid(mod1)
        names(resd) <- cur_df$group
        lows <- names(sort(resd))[1:3]
        low_grp <- paste(lows[1:2], collapse="; ")
        low_grp <- paste0(low_grp, "; and ", lows[3])

        x <- paste0("<h3>And expenditures <span style=font-style:italic;>tend</span> 
                    to correlate with number of species per group...</h3>
                    
                    <p>...but some groups, such as ", low_grp, ", have
                    expenditures much lower than expected given their diversity*. 
                    <p>The bubble chart to the left is based on the same data as
                    the one above, but illustrates the relationship between 
                    the number of species (x-axis) and expenditures 
                    (y-axis) for the taxonomic groups in the selected data. Here,
                    the size of the bubbles is proportional to the number of 
                    counties occupied by each group.</p>
                    <p><span style=font-weight:bold;>NOTE</span> that, as above,
                    both the x- and the y-axis are log-scaled!</p><p>*Based on 
                    the residuals from the regression expenditures ~ number of 
                    species (F = ", fsta, "; <span style=font-style:italic>
                    p</span> = ", pval, ").</p>")
        return(x)
    })

    output$group_bubble_chart_2 <- renderGvis({        
        if (input$groups_2 == "All" & input$species_2 == "All") {
            make_group_bubble_plot_2(selected_2(), height="450px")
        } else {
            make_group_bubble_plot_2(full, height="450px")
        }
    })

    output$state_exp_text <- renderText({
        cur_df <- make_top_25_states_df(selected_2())
        all_exp <- tapply(full$grand_per_cnty,
                          INDEX=full$STATE,
                          FUN=sum, na.rm=TRUE)
        med_exp <- median(all_exp, na.rm=TRUE)
        cur_exp <- ifelse(input$state_2 == "All",
                          0,
                          all_exp[input$state_2])

        # Make different headings depending on whether a state is selected
        if (cur_exp < med_exp) {
            rel_exp <- paste0("Less money is spent on listed species in ", 
                              input$state_2, " than in the average state")
        } else if (cur_exp > med_exp) {
            rel_exp <- paste0("More money is spent on listed species in ", 
                              input$state_2, " than in the average state")
        } else {
            rel_exp <- paste0(input$state_2, " represents the average state
                              expenditure")
        }
        heading <- ifelse(input$state_2 == "All",
                          "Expenditures vary widely between states",
                          rel_exp)

        lede_sent <- ifelse(input$state_2 == "All",
                            "The barchart at right illustrates the sources of 
                            expenditures on listed species for each of the top 
                            25 states.",
                            paste0("The barchart to the right shows 
                                   the sources of expenditures in ", 
                                   input$state_2, " (first bar) relative to 
                                   expenditures in the top 24 states."))

        x <- paste0("<h3>", heading, "</h3>
                    <p>", lede_sent, " This is in absolute dollars, and doesn't
                    account for the number of listed species per state.</p>")
        return(x)
    })

    output$state_exp_chart <- renderGvis({        
        if (input$state_2 == "All") {
            make_spend_state_plot(selected_2, height="450px")
        } else {
            make_select_state_spend_plot(full, 
                                         input$state_2, 
                                         height="450px")
        }
    })

    output$state_exp_text_2 <- renderText({
        per_spp_st <- tapply(selected_2()$grand_per_cnty,
                             INDEX=selected_2()$STATE,
                             FUN=sum, na.rm=TRUE) / 
                      tapply(selected_2()$sp,
                             INDEX=selected_2()$STATE,
                             FUN=get_n_levels)
        per_rank <- rank(-per_spp_st)

        raw_st <- tapply(selected_2()$grand_per_cnty,
                         INDEX=selected_2()$STATE,
                         FUN=sum, na.rm=TRUE)
        raw_rank <- rank(-raw_st)
        rank_delta <- sort(raw_rank - per_rank, decreasing=TRUE)
        len_rd <- length(rank_delta)

        x <- paste0("<h3>Spending in terms of dollars per listed species gives 
                    a different picture</h3>
                    
                    <p>After accounting for how many species are in each state,
                    states like ", paste(names(rank_delta)[1:2], collapse=' and '),
                    " move way up in the rankings relative to the 'raw' values 
                    of expenditures. In contrast, states like ",
                    paste(names(rank_delta)[(len_rd-1):len_rd], collapse=' and '), " move
                    way down in the rankings. And per-species expenditures in a few states,
                    like ", paste(names(rank_delta[rank_delta == 0])[1:2], collapse=' and '),
                    ", result in rankings that aren't much different thant the
                    rankings when using 'raw' expenditures.</p>")
        return(x)
    })

    output$state_exp_chart_2 <- renderGvis({        
        #TODO: Add an if statement for when a state is selected, different plot
        make_per_spp_state_plot(selected_2, height="450px")
    })

    output$rank_change_plot <- renderGvis({
        per_spp_st <- tapply(selected_2()$grand_per_cnty,
                             INDEX=selected_2()$STATE,
                             FUN=sum, na.rm=TRUE) / 
                      tapply(selected_2()$sp,
                             INDEX=selected_2()$STATE,
                             FUN=get_n_levels)
        per_rank <- rank(-per_spp_st)

        raw_st <- tapply(selected_2()$grand_per_cnty,
                         INDEX=selected_2()$STATE,
                         FUN=sum, na.rm=TRUE)
        raw_rank <- rank(-raw_st)
        rank_delta <- sort(raw_rank - per_rank, decreasing=TRUE)
        pos_change <- ifelse(rank_delta > 0,
                             rank_delta,
                             0)
        neg_change <- ifelse(rank_delta < 0,
                             rank_delta,
                             0)
        rd_df <- data.frame(state=names(rank_delta),
                            rank_delta=as.vector(rank_delta),
                            pos_change=pos_change,
                            neg_change=neg_change)
        make_rank_delta_plot(rd_df, height="525px")
    })

}


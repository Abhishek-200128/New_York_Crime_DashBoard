#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


# Load necessary libraries for the Shiny web application
library(shiny)
library(ggplot2)
library(heatmaply)
library(plotly)
library(leaflet)
library(gganimate)
library(shinyjs)


# Define the server logic for the Shiny web application
function(input, output, session) {
  # Read necessary data and store them in variables
  arrests <- read_csv("Arrests_Data_Wrangled.csv")
  arrests$ARREST_DATE <- as.Date(arrests$ARREST_DATE, format = "%m/%d/%Y")
  ages <- c("18-24", "25-44", "45-64", "65+", "<18", "UNKNOWN")
  ages <- data.frame(ages)
  
  schools <- read.csv("Schools.csv")
  school_high <- schools[schools$Location_Category_Description == "High school", ]
  school_sec <- schools[schools$Location_Category_Description == "Secondary School", ]
  school_colab <- schools[schools$Location_Category_Description == "Collaborative or Multi-graded", ]
  school_jun <- schools[schools$Location_Category_Description == "Junior High-Intermediate-Middle", ]
  
  output$year_choose <- renderText({
    paste("Year Chosen: ", input$year)
  })
  
  # Render Heatmap
  output$heat_map <- renderPlotly({
    # Group data by race and gender
    victims_race <- complaints %>%
      select(RPT_DT,VIC_RACE, VIC_SEX) %>%
      group_by(year(RPT_DT),VIC_RACE, VIC_SEX) %>%
      summarise(no_victims = n()) %>%
      filter(VIC_RACE != "(null)" & VIC_SEX %in% c("F","M")) %>%
      pivot_wider(
        names_from = VIC_SEX,
        values_from = no_victims
      )
    
    # filter data by year
    victims_race <- victims_race %>%
      filter(`year(RPT_DT)` == input$year)
    
    victims_race <- data.frame(victims_race)
    
    rownames(victims_race) <- victims_race$VIC_RACE
    victims_race <- victims_race %>%
      select(`F`,`M`)
    
    # initialise heatmap
    h_map <-  heatmaply(victims_race, 
                        dendrogram = "none",
                        margins = c(60,100,40,20),
                        fontsize_row = 8, fontsize_col = 8, fontsize_main = 5,
                        col = colorRampPalette(c("#FFA07A", "#FF0000", "#8B0000")),
                        grid_color = "white",
                        label_names = c("Victim Race", "Victim Sex", "Count"),
                        labCol = colnames(victims_race),
                        labRow = rownames(victims_race),
                        heatmap_layers = theme(axis.line=element_blank()),
                        colorbar = list(title = "Victims")
    )
    # remove unused data
    rm(victims_race)
    gc()
    h_map
  })
  
  
  # render spider chart
  output$spider <- renderPlotly({
    # group data by year and age group and filter the year
    victims_age <- complaints %>%
      select(RPT_DT, VIC_AGE_GROUP) %>%
      group_by(year(RPT_DT), VIC_AGE_GROUP) %>%
      summarise(no_victims = n()) %>%
      filter(VIC_AGE_GROUP != "(null)" & `year(RPT_DT)` == input$year) %>%
      select(VIC_AGE_GROUP, no_victims)
    
    # initialise spider chart
    age_spider <- plot_ly(victims_age,
      type = 'scatterpolar',
      r = victims_age$no_victims,
      theta = victims_age$VIC_AGE_GROUP,
      fill = 'toself',
      hovertemplate = "Age Group: %{theta} <br> Count: %{r}",
      source = "spider"
    ) 
    age_spider <- age_spider %>%
      layout(
        polar = list(
          radialaxis = list(
            visible = T
          )
        ),
        showlegend = F
      )
    # remove unused data
    rm(victims_age)
    gc()
    age_spider
  })
  
  # prepare offense data for top 10 complaints and arrests
  offense_grouped <- complaints %>%
    group_by(year(RPT_DT),OFNS_DESC) %>%
    summarise(complaints = n())
  
  offense_grouped_complaints <- complaints %>%
    group_by(OFNS_DESC) %>%
    summarise(no_complaints = n()) %>%
    arrange(desc(no_complaints)) %>%
    head(10)
  
  offense_grouped_arr <- arrests %>%
    group_by(OFNS_DESC) %>%
    summarise(no_arrests = n()) %>%
    arrange(desc(no_arrests)) %>%
    head(10)
  
  offense_grouped_arrests <- arrests %>%
    group_by(year(ARREST_DATE),OFNS_DESC) %>%
    summarise(arrests = n())
  
  top_10_off <- offense_grouped %>%
    filter(OFNS_DESC %in% offense_grouped_complaints$OFNS_DESC) %>%
    ungroup()
  
  top_10_off_arr <- offense_grouped_arrests %>%
    filter(OFNS_DESC %in% offense_grouped_arr$OFNS_DESC) %>%
    ungroup()
  
  # render line graph based on selection of data
  output$line_graph <- renderPlotly({
    if(input$type == "Complaints"){
      top_10_complaints <- plot_ly(top_10_off, x = ~`year(RPT_DT)`, 
                                   y = ~complaints, color = ~OFNS_DESC, 
                                   type = "scatter", mode = "lines",
                                   hovertemplate = "Year: %{x} <br> Complaints: %{y}", customdata = ~OFNS_DESC, source = "line") %>%
                           layout(title = 'Top 10 Complaints', xaxis = list(title = 'Year of Report'), 
                                yaxis = list(title = 'Number of Complaints'), legend = list(title=list(text='<b> Offense Description </b>')))
    }
    else{
      top_10_arrests <- plot_ly(top_10_off_arr, x = ~`year(ARREST_DATE)`, 
                                   y = ~arrests, color = ~OFNS_DESC, 
                                   type = "scatter", mode = "lines",
                                hovertemplate = "Year: %{x} <br> Arrests: %{y}", customdata = ~OFNS_DESC, source = "line") %>%
        layout(title = 'Top 10 Arrests', xaxis = list(title = 'Year of Arrest'), 
               yaxis = list(title = 'Number of Arrests'), legend = list(title=list(text='<b> Offense Description </b>')))
    }
  })
  
  # find top 10 offenses in each year
  offense_formatted <- offense_grouped_arrests %>%
    group_by(`year(ARREST_DATE)`) %>%
    # The * 1 makes it possible to have non-integer ranks while sliding
    mutate(rank = rank(-arrests)) %>%
    group_by(OFNS_DESC) %>% 
    filter(rank <=10) %>%
    ungroup()

  
  # render animated bar chart
  output$bars <- renderImage({
    # A temp file to save the output.
    outfile <- tempfile(fileext='.gif')
    
    # create static bar chart
    static_bar = ggplot(offense_formatted, aes(rank, group = OFNS_DESC)) +
      geom_tile(aes(y = arrests/2,
                    height = arrests,
                    width = 0.9, fill = "red"), alpha = 0.8) +
      geom_text(aes(y = 0, label = paste(OFNS_DESC, " ")), vjust = 0.2, hjust = 1, size = 7) +
      geom_text(aes(y=arrests,label = paste(" ",arrests)), hjust=0, size = 7) +
      coord_flip(clip = "off", expand = FALSE) +
      scale_y_continuous(labels = scales::comma) +
      scale_x_reverse() +
      theme(axis.line=element_blank(),
            axis.text.x=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks=element_blank(),
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            legend.position="none",
            panel.background=element_blank(),
            plot.title=element_text(size=25, hjust=0.5, face="bold", colour="grey", vjust=-1),
            plot.subtitle=element_text(size=25),
            plot.margin = margin(2,4, 2, 18, "cm"))
    
    # craete animation using static charts
    anim_bar = static_bar + transition_states(`year(ARREST_DATE)`, transition_length = 4, state_length = 1) +
      view_follow(fixed_x = TRUE)  +
      labs(title = 'Year : {closest_state}',  
           subtitle  =  "Top 10 Offenses",
      )
    
    # save animation
    anim_save("outfile.gif", animate(anim_bar, 200, fps = 20,  width = 1200, height = 1000, 
                                     renderer = gifski_renderer())) # New
    
    # Return a list containing the filename
    list(src = "outfile.gif",
         contentType = 'image/gif',
         width = 600,
         height = 350,
         alt = "This is alternate text"
    )}, deleteFile = TRUE)
  
  # render leaflet map
  output$map <- renderLeaflet({
    
    # Create the Leaflet map
    m <- leaflet() %>% 
      addProviderTiles(provider = "CartoDB.Positron")
    
    # add school markers if checkbox is selected
    if(input$schools == TRUE){
      m <- m %>%
        addCircleMarkers(
          lat = school_high$LATITUDE,
          lng = school_high$LONGITUDE,
          fillOpacity = 1,
          color = "blue",
          radius = 2,
          label = school_high$Location_Category_Description
        ) %>%
        addCircleMarkers(
          lat = school_jun$LATITUDE,
          lng = school_jun$LONGITUDE,
          fillOpacity = 1,
          color = "green",
          radius = 2,
          label = school_jun$Location_Category_Description
        ) %>%
        addCircleMarkers(
          lat = school_sec$LATITUDE,
          lng = school_sec$LONGITUDE,
          fillOpacity = 1,
          color = "yellow",
          radius = 2,
          label = school_sec$Location_Category_Description
        ) %>%
        addCircleMarkers(
          lat = school_colab$LATITUDE,
          lng = school_colab$LONGITUDE,
          fillOpacity = 1,
          color = "brown",
          radius = 2,
          label = school_colab$Location_Category_Description
        ) %>%
        addLegend(
          position = "bottomright",
          colors = c("red","blue","green","yellow","brown"),
          labels = c("Complaint/Arrest","High School","Junior High School","Secondary School","Collaborative School"),
          opacity = 1,
          title = "Legend"
        )
    }
    
    # add complaint markers if selected else arrest markers
    if(input$type == "Complaints"){
      complaint <- complaints[year(complaints$RPT_DT) == input$year, ]
      m <- m %>%
        addCircleMarkers(
            lat = complaint$Latitude,
            lng = complaint$Longitude,
            fillOpacity = 1,
            color = "red",
            clusterOptions = markerClusterOptions(),
            label = paste("Offense: ",complaint$OFNS_DESC),
            radius = 2
          )
      rm(complaint)
      gc()
    }
    else{
      arrest <- arrests[year(arrests$ARREST_DATE) == input$year, c(3,7,8)]
      m <- m %>%
        addCircleMarkers(
        lat = arrest$Latitude,
        lng = arrest$Longitude,
        fillOpacity = 1,
        color = "red",
        clusterOptions = markerClusterOptions(),
        label = paste("Offense: ",arrest$OFNS_DESC),
        radius = 2
      )
      rm(arrest)
      gc()
    }
      m
  })

  
  clearAge <- 0
  clearOfns <- 0
  
  observeEvent(c(event_data("plotly_click", source = "spider"), event_data("plotly_click", source = "line"), input$clearOfns, input$clearAge), {
    rm(list = ls())
    gc()
    
    # Store event data into variables
    click_info <- event_data("plotly_click", source = "line")
    dbl_click_info <- event_data("plotly_click", source = "spider")
    
    # change event data to null if clear filter is clicked
    if(input$clearAge > clearAge) {
      dbl_click_info[2] <- NA
      js$resetClickSpider()
      clearAge <<- as.numeric(input$clearAge)
    }
    
    if(input$clearOfns > clearOfns) {
      click_info <- NULL
      js$resetClickLine()
      clearOfns <<- as.numeric(input$clearOfns)
    }
    
    
    if (!is.null(click_info) | !is.null(dbl_click_info)) {
      # render text for selected filter
      output$clickedGroup <- renderText({
        if(is.null(click_info$customdata)){
          paste("No Offense filter")
        }
        else{
          paste("Filter applied for: ",click_info$customdata)
        }
        
      })
      
      output$clickedAgeGroup <- renderText({
        if(!is.null(dbl_click_info)){
        if(is.na(dbl_click_info[2])){
          paste("No Age filter")
        }
        else{
          paste("Filter applied for: ",  ages[as.numeric(dbl_click_info[2]) + 1,1])
        }
        }
        
      })
      
      # render spider chart with new data
      output$spider <- renderPlotly({
        # filter data based on input values
        victims_age <- complaints %>%
          select(RPT_DT, VIC_AGE_GROUP, OFNS_DESC)
        
        if(!is.null(click_info$customdata)){
          victims_age <- victims_age %>%
            filter(OFNS_DESC == click_info$customdata)
        }
        
        if(!is.null(dbl_click_info)){
        if(!is.na(dbl_click_info[2])){
          victims_age <- victims_age %>%
            filter(VIC_AGE_GROUP == ages[as.numeric(dbl_click_info[2]) + 1,1])
        }
        }
        
        victims_age <- victims_age %>%
          group_by(year(RPT_DT), VIC_AGE_GROUP) %>%
          summarise(no_victims = n()) %>%
          filter(VIC_AGE_GROUP != "(null)" & `year(RPT_DT)` == input$year) %>%
          select(VIC_AGE_GROUP, no_victims)
        
        age_spider <- plot_ly(
          type = 'scatterpolar',
          r = victims_age$no_victims,
          theta = victims_age$VIC_AGE_GROUP,
          fill = 'toself',
          hovertemplate = "Age Group: %{theta} <br> Count: %{r}",
          source = "spider"
        ) 
        age_spider <- age_spider %>%
          layout(
            polar = list(
              radialaxis = list(
                visible = T
              )
            ),
            showlegend = F
          )
        rm(victims_age)
        gc()
        age_spider
      })
      
      # render heatmap with new data
      output$heat_map <- renderPlotly({
        # filter data based on input values
        victims_race <- complaints %>%
          select(OFNS_DESC, RPT_DT, VIC_RACE, VIC_SEX, VIC_AGE_GROUP) 
        
        if(!is.null(click_info$customdata)){
          victims_race <- victims_race %>%
            filter(OFNS_DESC == click_info$customdata)
        }
        
        if(!is.null(dbl_click_info)){
        if(!is.na(dbl_click_info[2])){
          victims_race <- victims_race %>%
            filter(VIC_AGE_GROUP == ages[as.numeric(dbl_click_info[2]) + 1,1])
        }
        }
        
        victims_race <- victims_race %>%
          filter(year(RPT_DT) == input$year) %>%
          group_by(VIC_RACE, VIC_SEX) %>%
          summarise(no_victims = n()) %>%
          filter(VIC_RACE != "(null)" & VIC_SEX %in% c("F","M")) %>%
          pivot_wider(
            names_from = VIC_SEX,
            values_from = no_victims
          )
        
        victims_race <- data.frame(victims_race)
        
        rownames(victims_race) <- victims_race$VIC_RACE
        victims_race <- victims_race %>%
          select(`F`,`M`)
        
        h_map <-  heatmaply(victims_race, 
                            dendrogram = "none",
                            margins = c(60,100,40,20),
                            fontsize_row = 8, fontsize_col = 8, fontsize_main = 5,
                            col = colorRampPalette(c("#FFA07A", "#FF0000", "#8B0000")),
                            grid_color = "white",
                            label_names = c("Victim Race", "Victim Sex", "Count"),
                            labCol = colnames(victims_race),
                            labRow = rownames(victims_race),
                            heatmap_layers = theme(axis.line=element_blank())
        )
        rm(victims_race)
        gc()
        h_map
      })
      
      # render leaflet map with new data
      output$map <- renderLeaflet({
        
        # Create the Leaflet map
        m <- leaflet() %>% 
          addProviderTiles(provider = "CartoDB.Positron")
        
        if(input$schools == TRUE){
          m <- m %>%
            addCircleMarkers(
              lat = school_high$LATITUDE,
              lng = school_high$LONGITUDE,
              fillOpacity = 1,
              color = "blue",
              radius = 2,
              label = school_high$Location_Category_Description
            ) %>%
            addCircleMarkers(
              lat = school_jun$LATITUDE,
              lng = school_jun$LONGITUDE,
              fillOpacity = 1,
              color = "green",
              radius = 2,
              label = school_jun$Location_Category_Description
            ) %>%
            addCircleMarkers(
              lat = school_sec$LATITUDE,
              lng = school_sec$LONGITUDE,
              fillOpacity = 1,
              color = "yellow",
              radius = 2,
              label = school_sec$Location_Category_Description
            ) %>%
            addCircleMarkers(
              lat = school_colab$LATITUDE,
              lng = school_colab$LONGITUDE,
              fillOpacity = 1,
              color = "brown",
              radius = 2,
              label = school_colab$Location_Category_Description
            ) %>%
            addLegend(
              position = "bottomright",
              colors = c("red","blue","green","yellow","brown"),
              labels = c("Complaint/Arrest","High School","Junior High School","Secondary School","Collaborative School"),
              opacity = 1,
              title = "Legend"
            )
        }
        
        # filter data based on input values
        if(input$type == "Complaints"){
          complaint <- complaints %>%
            filter(year(RPT_DT) == input$year)
          
          
          if(!is.null(click_info$customdata)){
            complaint <- complaint %>%
              filter(OFNS_DESC == click_info$customdata)
          }
          
          if(!is.null(dbl_click_info)){
          if(!is.na(dbl_click_info[2])){
            complaint <- complaint %>%
              filter(VIC_AGE_GROUP == ages[as.numeric(dbl_click_info[2]) + 1,1])
          }
          }
          
          complaint <- complaint %>%
            select(OFNS_DESC,Latitude,Longitude)
          
          # add markers for complaint
          m <- m %>%
            addCircleMarkers(
              lat = complaint$Latitude,
              lng = complaint$Longitude,
              fillOpacity = 1,
              color = "red",
              clusterOptions = markerClusterOptions(),
              label = paste("Offense: ",complaint$OFNS_DESC),
              radius = 2
            )
          
          rm(complaint)
          gc()
        }
        else{
          # filter data based on input values
          arrest <- arrests %>%
            filter(year(ARREST_DATE) == input$year)
          
          
          if(!is.null(click_info$customdata)){
            arrest <- arrest %>%
              filter(OFNS_DESC == click_info$customdata)
          }
          
          if(!is.null(dbl_click_info)){
          if(!is.na(dbl_click_info[2])){
            arrest <- arrest %>%
              filter(AGE_GROUP == ages[as.numeric(dbl_click_info[2]) + 1,1])
          }
          }
          
          arrest <- arrest %>%
            select(OFNS_DESC,Latitude,Longitude)
          
          # add markers for arrest
          m <- m %>%
            addCircleMarkers(
              lat = arrest$Latitude,
              lng = arrest$Longitude,
              fillOpacity = 1,
              color = "red",
              clusterOptions = markerClusterOptions(),
              label = paste("Offense: ",arrest$OFNS_DESC),
              radius = 2
            )
          rm(arrest)
          gc()
        }
        m
      })
    }
  })
}



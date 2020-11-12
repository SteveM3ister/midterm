#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(leaflet)
library(magrittr)

library(shiny)
library(magrittr)
library(shinycssloaders)
library(shinythemes)
library(lubridate)
library(sf)
library(rgdal)
library(dplyr)
library(maps)
library(ggplot2)
library(DT)

public<-read.csv2("public.csv",header=TRUE,sep=",")
save<-read.csv2("save.csv",header=TRUE,sep=",")
public$lat%<>%as.numeric()
public$long%<>%as.numeric()
public_sf<-st_as_sf(map('county',plot=F,fill=T))
colnames(county.fips)[2]<-colnames(public_sf)[1]
public_sf<-left_join(public_sf,county.fips,by="ID")

button_color_css <- "
#DivCompClear, #FinderClear, #EnterTimes{
/* Change the background color of the update button
to blue. */
background: DodgerBlue;

/* Change the text size to 15 pixels. */
font-size: 15px;
}"



# Define UI
ui <- fluidPage(
    titlePanel("MA615 Mapping Project"),
    hr(),
    tags$h5("Created by Yinfeng Zhou, Ziyi Bai and Congyao Duan from Boston University"),
    hr(),
    navbarPage("Quick Stats for Hurricane",theme=shinytheme("lumen"),
               tabPanel("Interacitve Map",fluid=TRUE,icon=icon("globe-americas"),
                        leafletOutput("map", width="100%", height="1000"),
                        absolutePanel(id = "Select Panel", class = "panel panel-default", fixed = TRUE,
                                      draggable = TRUE, top = 200, left = "auto", right = 20, bottom = "auto",
                                      width = 330, height = "auto",
                                      
                                      h3("Hurricane Explorer"),
                                      selectInput(inputId="TypeFinder", label="Incident Type",choices=unique(public$incidentType)),
                                      selectInput(inputId="YearFinder", label="Year",choices=sort(unique(public$yy))),
                                      selectInput(inputId="MonthFinder", label="Month",choices=sort(unique(public$mm))),
                                      selectInput(inputId="DayFinder", label="Day",choices=sort(unique(public$dd))),
                                      
                                      ),
            
         ),
         tabPanel("Analysis",icon=icon("table"),fluid=TRUE,
                  sidebarLayout(
                      sidebarPanel(
                          titlePanel("Hurricane Explorer"),
                          fluidRow(
                              selectInput(inputId="StateFinder",
                                                 label="Select State(s):",
                                                 choices=unique(save$state))
                          )
                      ),
                      mainPanel(
                          fluidRow(
                              withSpinner(dataTableOutput(outputId="Table"))
                          ),
                          fluidRow(
                              column(6,tags$h4("Histogram"),
                                     plotOutput("Hist")
                          ),
                      )
                  ))
                  
                  ),
               
               
        

               
               ##More Info
               tabPanel("More",icon=icon("info-circle"),
                        fluidRow(
                            hr(),
                            h3("About this Project"),
                            h4("This project began as practice of ShinyApp for MA615(Data Science in R) of MSSP program in Boston University."),
                            h4("The project is intended to mapping and making some quick stats for hurricanes in United States."),
                            h4(p("These data were collected from ",a("FEMA",href="https://www.fema.gov/disasters/disaster-declarations"),".")),
                            h4(p("The dataset we use here is from",a("this webpage,",href="https://www.fema.gov/api/open/v1/PublicAssistanceFundedProjectsDetails.csv"),"downloaded as CSV file.")),
                            h4(p("Source code can be accessed from ",a("github",href="https://github.com/MSSP-615-Team-15/midterm.git"),".")),
                            hr(),
                            h5("Built with",
                               img(src = "https://www.rstudio.com/wp-content/uploads/2014/04/shiny.png", height = "30px"),
                               "by",
                               img(src = "https://www.rstudio.com/wp-content/uploads/2014/07/RStudio-Logo-Blue-Gray.png", height = "30px"),
                               ".")))
               )
)





server <- function(input, output,session) {
    

    output$map <- renderLeaflet({
        leaflet() %>%
            addProviderTiles(
                 providers$CartoDB.Positron
            ) %>%
            setView(lng = -71.057083, lat = 42.361145, zoom = 11)
    }) 
    

    observe({
        x<-Finder_yy()%>%select("yy")%>%unique()
        updateSelectInput(session,"YearFinder",label="Year",choices=sort(unique(x$yy)))
        observe({
            x<-Finder_mm()%>%select("mm")%>%unique()
            updateSelectInput(session,"MonthFinder",label="Month",choices=sort(unique(x$mm)))
            observe({
                x<-Finder_dd()%>%select("dd")%>%unique()
                updateSelectInput(session,"DayFinder",label="Day",choices=sort(unique(x$dd)))
            })
        })
    })  
    
    Finder_yy<-reactive({
       filter(public,incidentType %in% input$TypeFinder)
    })
    Finder_mm<-reactive({
        filter(public,incidentType %in% input$TypeFinder)%>%
            filter(yy %in% input$YearFinder)
    })
    Finder_dd<-reactive({
        filter(public,incidentType %in% input$TypeFinder)%>%
            filter(yy %in% input$YearFinder)%>%
            filter(mm %in% input$MonthFinder)
    })
    
    # Finder_cen<-reactive({
    # 
    #     center
    # })

    
    observe({

        
        

        Finder<-filter(public,incidentType %in% input$TypeFinder)%>%
            filter(year(declarationDate) %in% input$YearFinder)%>%
            filter(month(declarationDate) %in% input$MonthFinder) %>%
            filter(day(declarationDate) %in% input$DayFinder)
        
        Save<-filter(save,incidentType %in% input$TypeFinder)%>%
            filter(year(declarationDate) %in% input$YearFinder)%>%
            filter(month(declarationDate) %in% input$MonthFinder) %>%
            filter(day(declarationDate) %in% input$DayFinder)
        # temp1<-public%>%group_by(disasterNumber)
        # temp1$long%<>%as.numeric()
        # temp1$lat%<>%as.numeric()
        # temp1<-temp1%>%summarise(mlong=mean(long),mlat=mean(lat))
        # center<-left_join(Finder,temp1,by="disasterNumber")

            
        Finder<-right_join(public_sf,Finder,by="fips")
        
        # labels <- sprintf(
        #     "%s ",
        #     Finder$disasterNumber
        # ) 
        
            #public_sf <- left_join(public_sf, Finder, by="fips")
            leafletProxy("map", data = Finder) %>%
                addTiles() %>%
                clearShapes() %>%
                addPolygons(opacity=0.5, fillColor="gray88",fillOpacity = 0.01, 
                            label=Finder$Region,color = "white", weight = 1,dashArray = "3",
                            highlightOptions = highlightOptions(color = "red", weight = 3,
                                                                bringToFront = TRUE,dashArray="")
                            )

        
            
        })
    
#Analysis
    HurrFinder<-reactive({
        req(input$StateFinder)
        filter(save,state %in% input$StateFinder)
    })
    

    output$Table<-renderDataTable({
        datatable(HurrFinder())
    })
    output$Hist<-renderPlot({
        ggplot(HurrFinder(),aes(x=incidentType))+geom_histogram(position="identity",stat="count",fill="lightblue", color="black")+
            theme(axis.text.x = element_text(angle = 60, hjust = 1),
                  axis.text = element_text(size = 12),
                  axis.title = element_text(size = 13, face = "bold"))+
            geom_text(stat="count",aes(label = scales::percent(..count../sum(..count..))), size=5, position=position_stack(vjust = 0.5),col="black")}
    )

     

}
        
    


    


# Run the application 
shinyApp(ui = ui, server = server)

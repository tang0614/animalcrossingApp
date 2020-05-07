#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(dplyr)
library(tidyr)
library(tibble)
library(gtools)
library(shiny)
library(shinyWidgets)
library(shinythemes)
library(shinycssloaders)
library(scales)
library(treemap)
library(gtools)
library(highcharter)
library(stringr)
library(lexicon)
library(visNetwork)
library(rsconnect)
library(networkD3)

if (FALSE) {
    library(RSQLite)
    library(dbplyr)
}
library(shiny)

df <- read.csv("summary.csv", header = TRUE,stringsAsFactors = FALSE)
default_gene = c('Nate','Stitches','Megan','Marshal','Judy')


# Define UI for dataset viewer app ----
ui <- fluidPage(
    
    # App title ----
    titlePanel("Animal Crossing Villagers' Compatibility"),
    
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
        
        # Sidebar panel for inputs ----
        sidebarPanel(
            
            #Input Panel Title
            titlePanel("Input Panel"),
            hr(),
            helpText("Select the type of relationship:"),
            
            fluidRow(
                column(8, 
                       radioButtons("corr", "Relationship:",
                                    c("Exellent relationship" = "super",
                                      "Good relationship" = "positive",
                                      "Bad relationship" = "negative",
                                      "All" = "all"),
                                    selected = "all"),
                       
                )
            ),
            
            
            
            br(),
            fluidRow(
                column(11, 
                       helpText("Select animal:"),
                       selectInput("gene", label = "Animal name", 
                                   choices=mixedsort(as.vector(unique(df$aniA))), selected=default_gene,multiple=TRUE)
                )
                
            ),
            
            
            
            
            br(),
            fluidRow(
                column(11,
                       helpText("Select the top n best/worst relationship:"),
                       uiOutput("render_ui_genes")
                      
                       
                       
                )
            ),
            
            
            
            
            
            br(),
            hr(),
            br(),
            
            fluidRow(
                column(11, 
                       
                       tags$p(HTML("<b>Dataset Description</b> :This database has 391 villagers with 8 new ones.<a href=\"https://nookipedia.com/wiki/Animal_Crossing:_New_Horizons/Characters\">References</a>"))
                       
                ))
            
            
            
        ),
        
        mainPanel(
            
            fluidRow(
                
                column(11,offset = 1,
                       h3('Network Visualization for Animal Relationships'),
                       
                       helpText("Click and drag nodes below to see in detail."),
                       helpText("If you select bad relationship, then the network connects animals as nodes that are unfriendly with each other."),
                       helpText("If you select good/awesome relationship, then the network connects animals as nodes that are friendly with each other.")
                )
            ),
            
            fluidRow(
                
                column(9,align="center",
                       tabPanel("Force Network", forceNetworkOutput("force"))
                )
            ),
            
         
            
            br(),
            br(),
            
            br(),
            br(),
            
            br(),
            br(),
            
            
            
            
            fluidRow(
                column(9,offset = 1,
                       h3('Summary Table'),
                       helpText("The greatest score is 9 and the lowest score is 3.")
                )
            ),
            br(),
            
            
            
            fluidRow(
                column(9, offset = 1,
                       tableOutput('table1'),
                       downloadButton("downloadData", "Download Table")
                )
                
 
            )
            
            
            
            
        )
        
        
    )
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output,session) {
    

    
    output$value <- renderText({ missed_genes() })
    
    #select button
    
    
    dataInput1 <- reactive({
        m<-df
        if(input$corr == 'positive'){
            m<-filter(m, (total_score >= 4) & (total_score < 8))
            return(m)
            
        }
        else if(input$corr == 'negative'){
            m<-filter(m, total_score < 4)
            return(m)
            
        }
        else if(input$corr == 'super'){
            m<-filter(m, (total_score >= 8) & (total_score < 9))
            return(m)
            
        }
        else{return(m)}
        
    })
    

    
    dataInput2 <- reactive({
        m<-dataInput1()
        list_1<-m$aniA
        sub_list1<- list_1 %in% input$gene
        
        if (is.null(input$gene)){
            req(input$gene)
        } else {
            m <- subset(m, sub_list1)
        } 
        m
    })
    
    output$render_ui_genes<- renderUI({
        m<-dataInput2()
        l<-length(unique(m$aniB))
                  
        sliderInput("rank", "Rank:",
                    min = 1, max =l,
                    value = floor(l/50))
        
    })
    
    dataSort <- reactive({
        m<-dataInput2()
        r <- input$rank
        
        if (is.null(input$rank)){
            req(input$rank)
        } else {
            m<-m %>%group_by(aniA)%>%
                arrange(desc(total_score, .group_by=TRUE))%>%
                slice(1:r)
            
        } 
        
        
        
        m<-subset(m, select = -c(X,specious_x,personality_x,star_x,specious_y,personality_y,star_y))
        m
        
    })
    
    
    
    MisNodes<- reactive({
        src <-input$gene
        
        m<-dataSort()

        m<-data.frame(unique_gene=union(m$aniA, m$aniB))
        m$ID <- 0:(nrow(m)-1)
        m$size <- 40
        m$group <- ifelse(m$unique_gene %in% src, "lions", "tigers")
        m<-subset(m, select = c(ID,unique_gene,size,group))
       
        m
        
        
        
        
    })
    
    MisLinks<- reactive({
        m<-dataSort()
        nodes<-MisNodes()
        
        m<-m%>% 
            left_join(nodes, by = c("aniA" = "unique_gene")) %>% rename(source = ID)
        m<- m%>% 
            left_join(nodes, by = c("aniB" = "unique_gene")) %>% rename(target = ID)
        
        m<-subset(m, select = c(source,target,total_score))
        m
    })
    
    output$force <- renderForceNetwork({
        l<-MisLinks()
        n<-MisNodes()
        
        ColourScale <- 'd3.scaleOrdinal()
            .domain(["lions", "tigers"])
           .range(["#4C3FBF","#EC9BC9"]);'
        
        
        MisLinks<-as.data.frame(l)
        MisNodes<-as.data.frame(n)
        forceNetwork(Links = MisLinks, Nodes = MisNodes, Source = "source",
                     Target = "target", 
                     Value = "total_score", 
                     NodeID = "unique_gene",
                     Nodesize = "size",
                     Group = "group",
                     fontSize = 15, # Font size
                     linkDistance = networkD3::JS("function(d) { return 50;}"),
                     linkWidth = networkD3::JS("function(d) { return Math.abs(d.value) *0.5 ; }"),
                     opacity = 0.6,
                     opacityNoHover = 1,
                     colourScale = JS(ColourScale))
    })

    
    
    

    
    output$table1 <- renderTable(dataSort())
    
    
    # Downloadable csv of selected dataset ----
    output$downloadData <- downloadHandler(
        filename = function() {
            paste(dataSort(), ".csv", sep = "")
        },
        content = function(file) {
            write.csv(dataSort(), file, row.names = FALSE)
        }
    )
    

    
    


    
    
}

# Create Shiny app ----
shinyApp(ui, server)
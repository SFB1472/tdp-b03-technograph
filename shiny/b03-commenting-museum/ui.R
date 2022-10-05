library(ggiraph)

load("data/heatmap_for.RData")


fluidPage(
  titlePanel("Technograph"),
  tabsetPanel(
    tabPanel("Perspective on domains",
               fluidRow(
                 column(2, ""),
                 column(8,
                        h3("In how many pages have systems been found so far?"),
                        plotOutput("getOverviewSitesWithSystems", height = "50px"),
                        
                        tags$hr(),
                        
                        h3("Detected Systems over Time"),
                        p("In the following graphic, three data sets are layered on top of each other."),
                        p("The lowest level shows if there is any data at all for this page and this year. This is coded in grey (no data available) or white (there are html-files in the dump from the archive)."),
                        p("Colors indicate which system has been detected. The shape indicates how the system has been found."),
                        p("You can hover over the data points to read the information directly."),
                        girafeOutput("getSnippetsOverTime", height = "700px"),
                        
                        tags$hr(),
                        
                        h3("Number of archived pages per year"),
                        p("This graph only shows if and how many html pages are included in the delivery from the internet archive for the respective domain."),
                        p(" In addition, the graph is divided into two parts: whether systems were found or not."),
                        girafeOutput("getSitesOverTime", height = "700px"),
                        
                        tags$hr(),
                        
                        h3("Get Details on a specific domain")#,
                        
                 ),
                 column(2, ""),
                 ),
                  fluidRow(
                    column(1, ""),
                    column(10,
                         plotOutput("getDetailOnDomain", width = "100%", height = "950px", inline = FALSE)),
                    column(1, ""))
             ),
    tabPanel(
      "Perspective on commenting systems",
      fluidRow(
        column(2, ""),
        column(8,
            ## grafik zeigt, auf wie viel unterschiedlichen seiten im jeweiligen jahr systems gefunden wurden.
            h3("Currently documented popularity of commenting systems found"),
            p("Years marked only with an outline, the system could be found as the company existed. In the marked year, however, nothing was discovered in the current data set."),
            plotOutput("getSystemsOverTime"),
            
            tags$hr(),
            
            h3("Documented systems that could possibly be found"),
            p("Lighter color indicates that the concrete date is unknown."),
            girafeOutput("getSystemsLifeTime", width = "100%", height = "1200px")
      ),
      column(2, "")
      
    )),
    tabPanel(
      "Temporary: heatmaps",
      fluidRow(
        column(2, ""),
        column(8,
        selectInput(inputId = "heatmap_for", label = "print data for domain: ", choices = heatmap_for, selected = "spiegel"),
        verbatimTextOutput("value3"),
        h3("How many sites do we have for the selected domain?"),
        plotOutput("getHistograms")
      ),
      column(2, "")
    )
  )
)
)


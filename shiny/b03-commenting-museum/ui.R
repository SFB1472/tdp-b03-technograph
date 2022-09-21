library(ggiraph)

load("data/heatmap_for.RData")


fluidPage(
  titlePanel("Technographer"),
  tabsetPanel(
    tabPanel("Perspective on domains",
               fluidRow(
                 column(2, ""),
                 column(8,
                        h3("Systems over Time"),
                        p("For every system found in the german dataset the corresponding domain is painted over time. Colors indicates which system has been detected. The shape indicates how the system has been found."),
                        p("Hover over the colored data to get all the info at first sight."),
                        girafeOutput("getSnippetsOverTime"),
                        tags$hr(),
                        h3("Amount of websites archived per month"),
                        girafeOutput("getSitesOverTime"),
                        tags$hr(),
                        h3("Get Details on a specific domain")#,
                        
                 ),
                 column(2, ""),
                 ),
                  fluidRow(
                    column(1, ""),
                    column(10,
                         plotOutput("getDetailOnDomain", width = "100%", height = "900px", inline = FALSE)),
                    column(1, ""))
             ),
    tabPanel(
      "Perspective on commenting systems",
          verticalLayout(
            
            h3("Currently documented popularity of commenting systems found in the german dataset"),
            girafeOutput("getSystemsOverTime"),
            tags$hr(),
            h3("Systems that could be found"),
            girafeOutput("getSystemsLifeTime")
      )
      
    ),
    tabPanel(
      "Temporary: heatmaps",
      fluidRow(
        column(2, ""),
        column(8,
        # tags$hr(),
        # selectInput("searchWord3", "Suchwort", freq_word_to_print, "Klimawandel"),
        selectInput(inputId = "heatmap_for", label = "print data for domain: ", choices = heatmap_for, selected = "spiegel"),
        verbatimTextOutput("value3"),
        h3("How many sites do we have for the selected domain?"),
        plotOutput("getHistograms")
      ),
      column(2, "")
    )
  )
))


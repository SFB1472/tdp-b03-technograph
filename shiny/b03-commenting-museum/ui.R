
library(ggiraph)

fluidPage(
  titlePanel("Technographer"),
  tabsetPanel(
    
    tabPanel("Perspective on domains",
             # sidebarLayout(
               # fluidRow(column( 12,
               
               # mainPanel(
             
               fluidRow(
                 column(2, ""),
                 column(8,
                        h3("Snippets over Time"),
                        p("For every sippet found in the german dataset the corresponding domain is painted over time. Colors indicates which snippet has been detected. Hover over the colored data to get all the info at first sight."),
                 # p(""),
                 girafeOutput("getSnippetsOverTime"),
                 # textOutput("table_caution"),
                 tags$hr(),
                 h3("Amount of websites archived per month"),
                 girafeOutput("getSitesOverTime"),
                 tags$hr()
                 ),
                 column(2, "")
                 )
             
                 # plotOutput("printhatebarsfacets")
                 # DT::dataTableOutput('strongestEmbeddingsPerPeriod')
                 # )
               # )
             ),
    tabPanel(
      "life time",
      sidebarLayout(
        sidebarPanel(
          p("Choose how to sort the commenting systems: "),
          # p("Worauf sich der erste bezieht, bestimmt ihr selbst über die Auswahl im Drop-Down-Menü."),
          # textInput("searchWord", "Suchwort", "Klimawandel"),
          selectInput("searchWord", "sort commenting systems ...", c("sort_life_span", "sort_name")),
          submitButton("request", icon("sync")),
          verbatimTextOutput("value"),
          br()#,
          # numericInput("number_words", value = 15, label = "Anzahl der angezeigten embeddings: \n (nur für das erste Diagramm relevant)")
        ),
        mainPanel(
          verticalLayout(
            
            girafeOutput("getSystemsOverTime")#,
            # tags$hr()#,
            # plotOutput("printOneHitWonder", width = "600px", inline = FALSE)#,
            # tags$hr(),
            # plotOutput("printembeddingsLines", width = "600px", inline = FALSE)
          )
        )
      )
    )
    # tabPanel(
    #   "Wortfrequenzen",
    #   sidebarLayout(
    #     sidebarPanel(
    #       p(""),
    #       # textInput("searchWord3", "Suchwort", "Klimawandel"),
    #       # selectInput("searchWord3", "Suchwort Klimawandel"),
    #       submitButton("abschicken", icon("refresh"))#,
    #       # verbatimTextOutput("value3")
    #       )
    #     ,
    #     mainPanel(
    #       verticalLayout(
    # 
    #         # plotOutput("printembeddingsLines", width = "600px", inline = FALSE)
    #       )
    #     )
    #   )
    # )
    )
)


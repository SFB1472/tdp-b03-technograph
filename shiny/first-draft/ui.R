words_to_extract <- read.csv("data/words_to_extract.csv") %>% select(word) %>% pull(.)
fluidPage(
  titlePanel("What's the context of the selected keywords?"),
  tabsetPanel(
    tabPanel(
      "keywords in context",
      sidebarLayout(
        sidebarPanel(
          p("Choose one of your categories via the drop-box-menu below."),
          # p("Worauf sich der erste bezieht, bestimmt ihr selbst über die Auswahl im Drop-Down-Menü."),
          # textInput("searchWord", "Suchwort", "Klimawandel"),
          selectInput("searchWord", "Kategorie", words_to_extract),
          submitButton("request", icon("refresh")),
          verbatimTextOutput("value"),
          br()#,
          # numericInput("number_words", value = 15, label = "Anzahl der angezeigten embeddings: \n (nur für das erste Diagramm relevant)")
         ),
        mainPanel(
          verticalLayout(
            DT::dataTableOutput("printhateindex"),
            tags$hr()#,
            # plotOutput("printOneHitWonder", width = "600px", inline = FALSE)#,
            # tags$hr(),
            # plotOutput("printembeddingsLines", width = "600px", inline = FALSE)
          )
        )
       )
    )#,
    
    # tabPanel("Der Hass über die Zeit",
    #          sidebarLayout(
    #            sidebarPanel(
    #              p("Das obere Diagramm zeigt, wieviel Hass über die Zeit in den beobachteten Gruppen zum Ausdruck gebracht wurde."),
    #              p("Zur Systematik: wurde eins unserer Wörter aus der Tabelle in einer Nachricht gefunden, gibt es einen Hass-Punkt. Je mehr Worte gefunden wurden, desto höher der Hass-Index pro Nachricht (siehe Tabelle)"),
    #              p("Für diese Diagramme wurden alle Hass-Werte addiert und durch das Nachrichtenaufkommen pro Tag geteilt.")
    #            ),
    #            mainPanel(
    #              tags$hr(),
    #              textOutput("table_caution"),
    #              tags$hr(),
    #              plotOutput("printhateLines"),
    #              tags$hr(),
    #              plotOutput("printhatebarsfacets")
    #              # DT::dataTableOutput('strongestEmbeddingsPerPeriod')
    #              )
    #            )
    #          )#,
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


library(needs)
needs(shiny, tidyverse, DBI, RPostgres, pool, dbplyr, ggforce, gdtools, ggtext, urltools, ggiraph, googlesheets4, MetBrewer)
loadNamespace("dbplyr")
# source("../../shiny-conf/config-secret.R")
source("global.R")
source("config/config.R")
source("config/config-graphic.R")
source("modules/modCrawlTime.R")
source("modules/myModule.R")
source("modules/selectSiteMod.R")
source("modules/findingsViz.R")
source("modules/findingsTable.R")

ui <- fluidPage(
  titlePanel("Technograph - Perspective on domains"),
  mainPanel(
    tabsetPanel(id = "domain",
      tabPanel("German",
               fluidRow(
                 column(2, ""),
                 column(10,
                        br(),
                        selectSiteModUI("site_to_load_first_tab", SELECT_MENU_SITE$German),
                        hr(),
                        tags$mark("Please be patient: loading the data will take some seconds"),
                        hr(),
                        h3("All traces of commenting functionality found or noted so far"),
                        p("Die Grafik zeigt für jeden Typ eines findings eine eigene Überschrift, abgetrennt durch einen grauen Balken. Mögliche Blöcke können sein: 'manual reseach' (momentan nur bei ksta), 'traces via form findings' und 'traces via snippets'. Gibt es beispielsweise keine traces via snippets, wird auch der Block erst gar nicht gezeichnet."),
                        p("Innerhalb eines Blockes von 'traces via form findings' werden alle übereinstimmenden Spuren mit der gleichen Farbe markiert."),
                        findingsVizUI("headerFindingsVis_first_tab"),
                        hr(),
                        h3("At what time were the pages archived?"),
                        modCrawlTimeUI("crawl_time_first_tab"),
                        hr(),
                        h3("Traces of commenting possiblities in form tags"),
                        p("Die Tabelle zeigt zusätzliche Informationen zu den 'traces via form tags'. Alle tags die innerhalb des gefundenen formtags genestet sind, werden hier gezeigt. Die Spalte 'text' enthält immer auch den Text aller nachfolgenden tags, d.h. um zu wissen, was die User der Seite textlich von dem form-tag wahrnehmen, ist alles schon im form-tag abzulesen."),
                        findingsTableUI("tableFindingsMod_first_tab")
                        )
                 )
               ),
      tabPanel("World",
               fluidRow(
                 column(2, ""),
                 column(10,
                        br(),
                        selectSiteModUI("site_to_load_sec_tab", SELECT_MENU_SITE$World),
                        hr(),
                        tags$mark("Please be patient: loading the data will take some seconds"),
                        hr(),
                        h3("All traces of commenting functionality found or noted so far"),
                        p("Die Grafik zeigt für jeden Typ eines findings eine eigene Überschrift, abgetrennt durch einen grauen Balken. Mögliche Blöcke können sein: 'manual reseach' (momentan nur bei ksta), 'traces via form findings' und 'traces via snippets'. Gibt es beispielsweise keine traces via snippets, wird auch der Block erst gar nicht gezeichnet."),
                        p("Innerhalb eines Blockes von 'traces via form findings' werden alle übereinstimmenden Spuren mit der gleichen Farbe markiert."),
                        findingsVizUI("headerFindingsVis_sec_tab"),
                        hr(),
                        h3("At what time were the pages archived?"),
                        modCrawlTimeUI("crawl_time_sec_tab"),
                        hr(),
                        h3("Traces of commenting possiblities in form tags"),
                        p("Die Tabelle zeigt zusätzliche Informationen zu den 'traces via form tags'. Alle tags die innerhalb des gefundenen formtags genestet sind, werden hier gezeigt. Die Spalte 'text' enthält immer auch den Text aller nachfolgenden tags, d.h. um zu wissen, was die User der Seite textlich von dem form-tag wahrnehmen, ist alles schon im form-tag abzulesen."),
                        findingsTableUI("tableFindingsMod_sec_tab")
                 )
               )
               ),
      tabPanel("Dutch",
               fluidRow(
                 column(2, ""),
                 column(10,
                        br(),
                        selectSiteModUI("site_to_load_third_tab", SELECT_MENU_SITE$Dutch),
                        hr(),
                        tags$mark("Please be patient: loading the data will take some seconds"),
                        hr(),
                        h3("All traces of commenting functionality found or noted so far"),
                        p("Die Grafik zeigt für jeden Typ eines findings eine eigene Überschrift, abgetrennt durch einen grauen Balken. Mögliche Blöcke können sein: 'manual reseach' (momentan nur bei ksta), 'traces via form findings' und 'traces via snippets'. Gibt es beispielsweise keine traces via snippets, wird auch der Block erst gar nicht gezeichnet."),
                        p("Innerhalb eines Blockes von 'traces via form findings' werden alle übereinstimmenden Spuren mit der gleichen Farbe markiert."),
                        findingsVizUI("headerFindingsVis_third_tab"),
                        hr(),
                        h3("At what time were the pages archived?"),
                        modCrawlTimeUI("crawl_time_third_tab"),
                        hr(),
                        h3("Traces of commenting possiblities in form tags"),
                        p("Die Tabelle zeigt zusätzliche Informationen zu den 'traces via form tags'. Alle tags die innerhalb des gefundenen formtags genestet sind, werden hier gezeigt. Die Spalte 'text' enthält immer auch den Text aller nachfolgenden tags, d.h. um zu wissen, was die User der Seite textlich von dem form-tag wahrnehmen, ist alles schon im form-tag abzulesen."),
                        findingsTableUI("tableFindingsMod_third_tab")
                 )
               ))
      )
    )
  )

server <- function(input, output, session) {
  
  # react_sphere <- reactiveValues()
  # observeEvent(input$domain, {
  #     print(paste("You clicked tab:", input$domain))
  #     react_sphere$sphere = input$domain
  # 
  #   })

  
  ### first Tab ---------------------------------------------------------------------------------------------
  site_to_load_tab_1 <- selectSiteModServer("site_to_load_first_tab")

  modCrawlTimeServer("crawl_time_first_tab", reactive(site_to_load_tab_1$selectSite))
  findingsVizServer("headerFindingsVis_first_tab", reactive(site_to_load_tab_1$selectSite), "German")
  findingsTableServer("tableFindingsMod_first_tab", reactive(site_to_load_tab_1$selectSite))
  
  
  ### second Tab ---------------------------------------------------------------------------------------------
  site_to_load_tab_2 <- selectSiteModServer("site_to_load_sec_tab")

  modCrawlTimeServer("crawl_time_sec_tab", reactive(site_to_load_tab_2$selectSite))
  findingsVizServer("headerFindingsVis_sec_tab", reactive(site_to_load_tab_2$selectSite), "World")
  findingsTableServer("tableFindingsMod_sec_tab", reactive(site_to_load_tab_2$selectSite))
  
  ### third Tab ---------------------------------------------------------------------------------------------
  site_to_load_tab_3 <- selectSiteModServer("site_to_load_third_tab")

  modCrawlTimeServer("crawl_time_third_tab", reactive(site_to_load_tab_3$selectSite))
  findingsVizServer("headerFindingsVis_third_tab", reactive(site_to_load_tab_3$selectSite), "Dutch")
  findingsTableServer("tableFindingsMod_third_tab", reactive(site_to_load_tab_3$selectSite))
}

shinyApp(ui = ui, server = server, onStart = function(){
  onStop(function() {
    poolClose(pool)
  })
})


## making tabs observeable: https://stackoverflow.com/questions/58402873/shiny-is-there-a-way-to-trigger-an-observeevent-by-switching-between-tabspane
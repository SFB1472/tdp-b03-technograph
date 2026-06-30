options(tidyverse.quiet = TRUE)
options(dplyr.summarise.inform = FALSE)
library(needs)
needs(shiny, tidyverse, DBI, RPostgres, pool, dbplyr, ggforce, gdtools, ggtext, urltools, ggiraph, googlesheets4, MetBrewer, gfonts, scales)
loadNamespace("dbplyr")
# source("../../shiny-conf/config-secret.R")
source("global.R")
source("config/config.R")
source("config/config-graphic.R")
source("modules/modSelectSphere.R")
source("modules/modSelectSite.R")
source("modules/modSelectSubdomain.R")
source("modules/msodSelectHash.R")
source("modules/tab_1_modAggregatedBarChart.R")
source("modules/tab_1_modTracesFound.R")
source("modules/tab_1_modArchivedSites.R")
source("modules/tab_2_modCrawlTime.R")
source("modules/tab_2_findingsViz.R")
source("modules/tab_2_formFindingsTable.R")
source("modules/tab_2_snippetFindingsTable.R")
source("modules/tab_3_modVizSystemsFound.R")
source("modules/tab_3_modSystemsLifetime.R")
source("modules/tab_4_historiograms.R")
# source("modules/tab_5_contextViz.R")
# source("modules/tab_5_compareHashesOV.R")
# source("modules/tab_5_compareHashes.R")

ui <- fluidPage(
  titlePanel("Technograph"),
  mainPanel(
    tabsetPanel(id = "domain",
                tabPanel(value = "tab_1", title = "Perspective on aggregated findings",
                         fluidRow(
                           column(2, ""),
                           column(10,
                                  br(),
                                  selectSphereModUI("sphere_to_load_1_tab"),
                                  br(),
                                  hr(),
                                  tags$mark("Please be patient: loading the data will take some time"),
                                  hr(),
                                  br(),
                                  h3("In how many pages have systems been found so far?"),
                                  tab_1_aggregatedBarchartUI("aggregated_bar_chart_tab1"),
                                  br(),
                                  hr(),
                                  h3("Detected Systems over Time"),
                                  p("In the following graphic, three data sets are layered on top of each other."),
                                  p("The lowest level shows if there is any data at all for this page and this year. This is coded in grey (no data available) or white (there are html-files in the dump from the archive)."),
                                  p("Colors indicate which system has been detected. The shape indicates how the system has been found."),
                                  p("You can hover over the data points to read the information directly."),
                                  tab_1_overviewTracesFoundUI("overview_traces_found_tab1"),
                                  br(),
                                  hr(),
                                  h3("Number of archived pages per year"),
                                  p("This graph only shows if and how many html pages are included in the delivery from the internet archive for the respective domain."),
                                  p(" In addition, the graph is divided into two parts: whether systems were found or not."),
                                  tab_1_archivedSitesUI("archivedSites_tab1")
                                )
                         )
                         ),
                tabPanel(value = "tab_2", title = "Perspective on domains",
                     fluidRow(
                       column(2, ""),
                       column(10,
                              br(),
                              selectSphereModUI("sphere_to_load_sec_tab"),
                              br(),
                              hr(),
                              tags$mark("Please be patient: loading the data will take some time"),
                              hr(),
                              br(),
                              selectSiteModUI("site_to_load_sec_tab"),
                              h3("All traces of commenting functionality found or noted so far"),
                              p("Die Grafik zeigt für jeden Typ eines findings eine eigene Überschrift, abgetrennt durch einen grauen Balken. Mögliche Blöcke können sein: 'manual reseach' (momentan nur bei ksta), 'traces via form findings' und 'traces via snippets'. Gibt es beispielsweise keine traces via snippets, wird auch der Block erst gar nicht gezeichnet."),
                              p("Innerhalb eines Blockes von 'traces via tag findings' werden alle übereinstimmenden Spuren mit der gleichen Farbe markiert."),
                              findingsVizUI("headerFindingsVis_sec_tab"),
                              hr(),
                              h3("At what time were the pages archived?"),
                              modCrawlTimeUI("crawl_time_sec_tab"),
                              hr(),
                              h3("Traces of commenting possiblities in tags"),
                              p("Die Tabelle zeigt zusätzliche Informationen zu den 'traces via form tags'. Alle 'Kinder' die innerhalb des Tags mit detektierten Spuren genestet sind, werden hier gezeigt. Die Spalte 'text' enthält immer auch den Text aller nachfolgenden tags."),
                              formFindingsTableUI("tableFindingsMod_sec_tab"),
                              h3("Traces of commenting systems"),
                              p("Die Tabelle zeigt zusätzliche Informationen zu den 'traces via snippets'."),
                              snippetFindingsTableUI("tableSnippetFindingsMod_sec_tab")
                       )
                     )
               ),
               tabPanel(value = "tab_3", title = "Perspective on systems",
                        fluidRow(
                          column(2, ""),
                          column(10,
                                 br(),
                                 selectSphereModUI("sphere_to_load_3_tab"),
                                 br(),
                                 hr(),
                                 h3("Currently documented popularity of commenting systems found"),
                                 p("Years marked only with an outline, the system could be found as the company existed. In the marked year, however, nothing was discovered in the current data set."),
                                 tab_3_modVizSystemFoundUI("systemFoundSphere"),
                                 br(),
                                 hr(),
                                 h3("Documented systems that could possibly be found"),
                                 p("Lighter color indicates that the concrete date is unknown."),
                                 tab_3_modSystemsLifetimeUI("systemsLifetimeViz")
                          )
                        )
               ),
               tabPanel(value = "tab_4", title = "Historiogram",
                        fluidRow(
                          column(2, ""),
                          column(10,
                                 br(),
                                 h3("Historiograms"),
                                 tab_4_historiogramUI("historiogram")
                                 )
                        )
                        ),
               tabPanel(value = "tab_5", title = "Work in progress: Context Analysis",
                        fluidRow(
                          column(1, ""),
                          column(10,
                                 br(),
                                 selectSphereModUI("sphere_to_load_tab_5"),
                                 br(),
                                 hr(),
                                 tags$mark("Please be patient: loading the data will take some time"),
                                 hr(),
                                 selectSiteModUI("site_to_load_tab_5"),
                                 br(),
                                 tab_5_contextVizUI("contextViz"),
                                 br(),
                                 h3("Compare Hashes"),
                                 p("Die folgenden Abschnitte werden mit Informationen gefüllt, wenn in Input-Felder Nummer von Hashes eingegeben werden. Diese Nummern können über mouse over die farbigen Punkte oben abgelesen werden. Es ist der Wert, der ganz am Ende steht."),
                                 br()
                                 ),
                          column(1, "")
                          ),
                          fluidRow(
                            column(4,
                                 selectSubdomainUI("tab_5_select_subdomain")),
                            column(4,
                                 inputHashUI("tab_5_compare_hash_1")),
                            column(4,
                                 inputHashUI("tab_5_compare_hash_2"))
                          ),
                          fluidRow(
                            column(1, ""),
                            column(10,
                                   br(),
                                   h4("Wie unterschiedlich umfangreich sind die codierten Datenbereiche?"),
                                   tab_5_compareHashesOVUI("tab_5_compareHashesOV"),
                                   br(),
                                   h4("Diese Zeilen sind nur in dem zuerst eingebenen Hash codiert"),
                                   tab_5_compareHashesUI("tab_5_compareHashes_1"),
                                   br(),
                                   h4("Die folgenden Zeilen sind nur in dem zweiten eingebenen Hash codiert"),
                                   tab_5_compareHashesUI("tab_5_compareHashes_2")
                                 ),
                            column(1, "")
                          )
                        )
               )
      
    )
  )

server <- function(input, output, session) {

  react_tab_activaded <- reactiveValues()
  observeEvent(input$domain, {
      # print(paste("You clicked tab:", input$domain))
    react_tab_activaded$tab = input$domain

    })
  
  # Tab "aggregated data" --------------------------------------
  
  sphere_to_load_tab_1 <- selectSphereModServer("sphere_to_load_1_tab")
  tab_1_aggregatedBarchartServer("aggregated_bar_chart_tab1", reactive(react_tab_activaded$tab), reactive(sphere_to_load_tab_1$selectSphere))
  tab_1_overviewTracesFoundServer("overview_traces_found_tab1", reactive(react_tab_activaded$tab), reactive(sphere_to_load_tab_1$selectSphere))
  tab_1_archivedSitesServer("archivedSites_tab1", reactive(react_tab_activaded$tab), reactive(sphere_to_load_tab_1$selectSphere))
  #
  # # # Tab "perspective on domains" -------------------------------
  sphere_to_load_tab_2 <- selectSphereModServer("sphere_to_load_sec_tab")
  site_to_display <- selectSiteModServer("site_to_load_sec_tab", reactive(sphere_to_load_tab_2$selectSphere))

  findingsVizServer("headerFindingsVis_sec_tab", reactive(react_tab_activaded$tab), reactive(site_to_display$selectSite), reactive(sphere_to_load_tab_2$selectSphere))
  modCrawlTimeServer("crawl_time_sec_tab", reactive(react_tab_activaded$tab), reactive(site_to_display$selectSite))
  formFindingsTableServer("tableFindingsMod_sec_tab", reactive(react_tab_activaded$tab), reactive(site_to_display$selectSite))
  snippetFindingsTableServer("tableSnippetFindingsMod_sec_tab", reactive(react_tab_activaded$tab), reactive(site_to_display$selectSite))
  #
  # Tab "perspective on systems" ----------------------------------
  sphere_to_load_tab_3 <- selectSphereModServer("sphere_to_load_3_tab")
  tab_3_modVizSystemFoundServer("systemFoundSphere", reactive(react_tab_activaded$tab), reactive(sphere_to_load_tab_3$selectSphere))
  tab_3_modSystemsLifetimeServer("systemsLifetimeViz")
  #
  # # # Tab "historiogram" ------------------------
  tab_4_historiogramServer("historiogram", reactive(react_tab_activaded$tab))
  
  # Tab "context analysis -----------------
  # sphere_to_load_tab_5 <- selectSphereModServer("sphere_to_load_tab_5")
  # site_to_display_tab_5 <- selectSiteModServer("site_to_load_tab_5", reactive(sphere_to_load_tab_5$selectSphere))
  # select_subdomain_tab_5 <- selectSubdomainServer("tab_5_select_subdomain", reactive(site_to_display_tab_5$selectSite))
  # 
  # tab_5_contextVizServer("contextViz", reactive(react_tab_activaded$tab), reactive(site_to_display_tab_5$selectSite), reactive(sphere_to_load_tab_5$selectSphere))
  # selectSubdomainServer("tab_5_select_subdomain", reactive(site_to_display_tab_5$selectSite))
  # compare_hash_1 <- inputHashServer("tab_5_compare_hash_1")
  # compare_hash_2 <- inputHashServer("tab_5_compare_hash_2")
  # tab_5_compareHashesOV <- tab_5_compareHashesOVServer("tab_5_compareHashesOV", reactive(react_tab_activaded$tab), reactive(select_subdomain_tab_5$selectSubdomain), reactive(compare_hash_1$selectHash), reactive(compare_hash_2$selectHash))
  # tab_5_compareHashes_1 <- tab_5_compareHashesServer("tab_5_compareHashes_1", reactive(react_tab_activaded$tab), reactive(select_subdomain_tab_5$selectSubdomain), reactive(compare_hash_1$selectHash), reactive(compare_hash_2$selectHash))
  # tab_5_compareHashes_2 <- tab_5_compareHashesServer("tab_5_compareHashes_2", reactive(react_tab_activaded$tab), reactive(select_subdomain_tab_5$selectSubdomain), reactive(compare_hash_2$selectHash), reactive(compare_hash_1$selectHash))
}

shinyApp(ui = ui, server = server, onStart = function(){
  onStop(function() {
    poolClose(pool)
  })
})


## making tabs observeable: https://stackoverflow.com/questions/58402873/shiny-is-there-a-way-to-trigger-an-observeevent-by-switching-between-tabspane
fluidPage(
  titlePanel("Technograph"),
  tabsetPanel(
    tabPanel("Perspective on domains",
               fluidRow(
                 column(2, ""),
                 column(8,
                        h3("Overview"),
                        selectSiteModUI("site_to_load"),
                        findingsVizUI("headerFindingsVis"),
                        h3("Traces of commenting possiblities in form tags"),
                        findingsTableUI("tableFindingsMod")
                        )
                 )
             )
    )
  )
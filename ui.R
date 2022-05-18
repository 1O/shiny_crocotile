library(waiter)

ui <- fluidPage(

    ## some CSS
    tags$head(
             tags$link(rel = "stylesheet", type = "text/css", href = "local.css")
         ),

    ## App title ----
    titlePanel(title = div(
                   tags$img(src = 'data/crocotile.svg', width = 200),
                   'crocoTile'
               ),
               windowTitle = 'eShape crocoTile'
               ),

    ## Main panel for displaying outputs ----
    mainPanel(

        useAttendant(),
        useWaitress(),
        verbatimTextOutput('log'),
        fluidRow(
            column(4),
            column(8,
                   tabsetPanel(
                       tabPanel("site data",
                                conditionalPanel( condition = "output.shapes_loaded",
                                                 div(id = 'div_site_picker',
                                                     shiny::selectizeInput(inputId = 'site_picker',
                                                                           label = ' ',
                                                                           choices = NULL)
                                                     ),
                                                 div(id = 'div_map',
                                                     leaflet::leafletOutput('plot')
                                                     ),
                                                 uiOutput('site_summary')
                                                 ),
                                div(id = 'loading', class = 'well',
                                    p(strong('loading geodata'),
                                      ' (this can take up to 10 seconds but 
                                                          is required only once per visit)'),
                                    attendantBar('loading-bar',
                                                 max = 100, color = "success",
                                                 striped = TRUE, animated = TRUE
                                                 ) 
                                    )
                                ),
                       tabPanel(HTML("what&rsquo;s this?"), 
                                includeHTML('./www/about.html')
                                ),
                       tabPanel("about",
                                div(
                                    includeHTML("./www/credits.html")
                                )
                                )
                   )

                   ),
            column(4
                   )
        )
    )
)



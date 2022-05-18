library(waiter)

ui <- fluidPage(
    ## App title ----
    titlePanel(title = div(
                   tags$img(src = 'data/crocotile.svg', width = 200),
                   'crocoTile'
                           ),
               windowTitle = 'eShape crocoTile'
               ),


    sidebarLayout(
        ## Sidebar panel for inputs ----
        sidebarPanel(
            shiny::selectizeInput(inputId = 'site_picker',
                                  label = 'select site',
                                  choices = NULL),
            leaflet::leafletOutput('plot')
        ),

        ## Main panel for displaying outputs ----
        mainPanel(

            useAttendant(),
            useWaitress(),
            verbatimTextOutput('log'),
            fluidRow(
                column(8,
                       div(id = 'loading',
                           p(strong('loading geodata'),
                             ' (this can take up to 10 seconds but is required only once per visit)'),
                           attendantBar('loading-bar',
                                        max = 100, color = "success",
                                        striped = TRUE, animated = TRUE
                                        ) 
                           ),
                       uiOutput('summary_table'),
                       uiOutput('site_summary')
                       ),
                column(4,
                       div(class = 'card text-white bg-grey  d-inline-block',
                           h2('about this tool'),
                           includeHTML('./www/about.html'),
                           hr(),
                           includeHTML("./www/credits.html")
                           )
                       )
            )

        )
    )
)



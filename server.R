
sapply(c('sf',
         'leaflet',
         'waiter',
         'lwgeom',
         'dplyr',
         'rmarkdown',
         'htmlwidgets'
         ), function(p) library(p, character.only = TRUE))

session = shiny::getDefaultReactiveDomain()

server <- function(input, output, session) {
    ## enter browser on error:
    ## options(shiny.error = browser)
    ##
    testmode = FALSE


    session = shiny::getDefaultReactiveDomain()
    source('./helpers.R')

    ## test_features <- generate_test_features()
    ## read static shp files into list:
    resource_base <- './www/data'

    att_loading <- Attendant$new("loading-bar")
    att_calculating <- Attendant$new("calculating-bar")

    removeUI('#calculating') ## show when calculation starts

    shiny::withProgress({
        list('nuts_countries_3857',
             'treshold_areas_caseC',
             'nat2k_habitat_diversity'
             ) %>%
            purrr::imap(~ {
                assign(x = .x, 
                       value = readRDS(file = file.path(resource_base, paste0(.x,'.rds'))),
                       envir = .GlobalEnv)
                att_loading$set(.y * 10/3) ## max: 100
                incProgress(1/.y, detail = paste("loading data: ", .x))
            })
    })


    static_shapes <- list(
        'eo_tiles' = 'sentinel-2-grid',
        'nat2k_polys' = 'nat2k_epsg3857_coarse',
        'nat2k_bboxes' = 'nat2k_bboxes_epsg3857',
        'deims_sites' = 'deims_boundaries_eu',
        'nat2k_coverage' = 'nat2k_bboxes_coverage_epsg3857'        
    )



    shapes_loaded <- reactiveVal()
    shapes_loaded(FALSE)

    static_shapes <- 
        static_shapes %>%
        purrr::imap(~ {
            progress = grep(.y, names(static_shapes))/length(static_shapes)
            att_loading$set(10 + 90 * progress) ## max: 100
            incProgress(progress, message = 'loading geoinformation (once only)',
                        detail = paste('feature:', .x)
                        )
            read_sf(dsn = file.path(resource_base,
                                    paste0(.x, ifelse(testmode,'_test',''), '.shp'))) %>%
                st_transform(.,3857)
        })
    shapes_loaded(TRUE)
    output$shapes_loaded <- reactive(shapes_loaded())
    outputOptions(output, "shapes_loaded", suspendWhenHidden = FALSE)


    removeUI('#loading')

    static_shapes$deims_sites <- static_shapes$deims_sites %>%
        mutate(orig_id = id,
               id = row_number()
               )


    shiny::updateSelectizeInput(session = session,
                                inputId = 'site_picker',
                                choices = static_shapes$deims_sites %>% pull(id, title),
                                selected = 1
                                )

    
    site = reactive(static_shapes$deims_sites[input$site_picker,])
    site_name = reactive(site()$title)


    ## grep('Atelier Al', sites$title)
    ## sites[325,]
    waitress <- Waitress$new("#site_summary",
                             theme = "overlay", infinite = TRUE)

   withProgress({ 
        case_type = reactive(diagnose_case(site() %>% st_geometry,
                                           nat2k_coverage = static_shapes$nat2k_coverage
                                           )
                             )
        incProgress(1, message = 'diagnosing site category ...')
    })

    withProgress({
        results = reactive({
            req(input$site_picker)
            waitress$start(h3("calculating..."))
            output$results_ready <- reactive(FALSE)
            res <- handle_case(feat = site() %>% st_geometry,
                               case_type = case_type(),
                               nat2k_bboxes = static_shapes$nat2k_bboxes,
                               nat2k_polys = static_shapes$nat2k_polys,
                               nat2k_habitat_diversity = nat2k_habitat_diversity,
                               nat2k_coverage = static_shapes$nat2k_coverage,
                               eo_tiles = static_shapes$eo_tiles
                        )
            ## convert bbox to polygon if necessary:
            if(!is.null(res$bbox)) res$bbox <- st_as_sfc(res$bbox)
            waitress$close()
            output$results_ready <- reactive(TRUE)
            return(res)
        })
        incProgress(.1, message = 'calculating features ...')
    })

    output$site_type <- shiny::renderUI(span('site classification:', strong(case_type())))
    shape_dir_name <- file.path(tempdir(),'shapes')


    observeEvent(results(),{

        ## output$log <- renderPrint(results())

        ## refresh directory for generated shape files:
        unlink(shape_dir_name, recursive = TRUE, force = TRUE)
        dir.create(shape_dir_name)
        results() %>%
            names %>%
            purrr::walk(~{
                try(sf::write_sf(results()[[.x]], dsn = file.path(shape_dir_name, paste0(.x, '.shp'))))
            })

        ## write README:
        generate_readme(site_name = site()$title,
                        site_id = gsub('^.*/','',site()$orig_id),
                        relevant_eo_tile = results()$relevant_eo_tile$name,
                        file_stub = file.path(shape_dir_name, "README")
                        )

    },
    ignoreInit = FALSE, ignoreNULL = TRUE
    )

    output$site_summary <- renderUI(
        div(class = 'well',
            tags$ul(
                     tags$li('site:',
                             strong(a(href = site()$orig_id, target = '_blank', site_name()))
                             ),
                     tags$li('site category:', strong(case_type()),
                             actionLink('modal_explain_site_category', strong("?"), class="btn-info")
                             ),
                     tags$li(length(results()$overlapping_eo_tiles$Name),
                             ' overlapping SENTINEL tiles:', strong(paste(results()$overlapping_eo_tiles$Name,
                                                                          collapse = ', '))
                             ),
                     tags$li('most relevant tile:', strong(results()$relevant_eo_tile$Name)
                             )
                 ),
            tags$hr(),
            downloadButton("downloadData", "Download", class="btn-success") 
        )
    )
    


    observeEvent(input$modal_explain_site_category, {
        showModal(modalDialog(
            title = "site categories",
            p(h2("A"), HTML(
                           "<strong>point</strong>: a site for which only the centroid has been reported.  The nearest Natura 2000 area is considered representative for this site, unless there is no such area within a 100 x 100 km<sup>2</sup> square around this site."
                       )),
            p(h2("B"), HTML(
                           "<strong>scattered</strong> site: comprised by several small areas spread across a comparatively large surface."
                       )),
            p(h2("C"), HTML(
                           "<strong>vast</strong> site: site boundaries cover a large part of the country."
                       )),
            p(h2("D"), HTML(
                           "<strong>standard</strong> site: none of A&ndash;C"
                       )),
            hr(),
            div( a(href="data/Methods.pdf", target="_blank",
                   "detailed description"), "of classification and processing"
                ),
            easyClose = TRUE
        )
        )
    })
    
    






    output$results_ready <- reactive(FALSE)
    outputOptions(output, 'results_ready', suspendWhenHidden = FALSE)
    output$plot <- renderLeaflet({
       site <-  results()[['site']] %>% st_transform(4326)

       the_map <- leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
           addProviderTiles(providers$Stamen.Terrain, #TonerLite,
                            options = providerTileOptions(noWrap = TRUE)
                            ) %>%
           addPolygons(data = site, options = list(fill = FALSE, weight = 3)) %>%
           addLabelOnlyMarkers(data = site %>% st_centroid,
                               label = site()$title,
                               labelOptions = list(permanent = TRUE)
                               )



       features_to_plot <- c('overlapping_eo_tiles',
                             'relevant_eo_tile',
                             'bbox', 'buffered_site', 'clipped_site'
                             )

       feature_polygon_options <- sapply(features_to_plot, function(x) NULL)
       feature_polygon_options$overlapping_eo_tiles <- list(fill = FALSE,
                                                            color = 'black',
                                                            weight = 1) ## weight: stroke width
       feature_polygon_options$relevant_eo_tile <- list(fill = FALSE,
                                                        stroke = TRUE,
                                                        weight = 2,
                                                        color = 'black')
       feature_polygon_options$clipped_site <- list(fill = TRUE, fillColor = 'blue', stroke = FALSE)
       feature_polygon_options$bbox <- list(fill = FALSE, color = 'green', weight = 3)


       sapply(features_to_plot, function(e){
                    feature <- results()[[e]]

                    ## print('Klasse:', class(feature))
                    if(!is.null(feature)) {
                        ## if(e == 'bbox') feature <- feature %>% st_as_sfc %>% st_cast('POLYGON')
                        the_map <<- 
                            the_map %>%
                            addPolygons(data = feature %>% st_transform(4326),
                                        group = e,
                                        options = feature_polygon_options[[e]],
                                        labelOptions = c(permanent = TRUE)
                                        )
                        
                        if(length(grep('relevant_eo_tile', e))){
                            the_map <<- the_map %>%
                                addLabelOnlyMarkers(data = feature,
                                                    lng = get_corner(feature)$lng,
                                                    lat = get_corner(feature)$lat,
                                                    label = ~paste('tile: ', Name, collapse = ''),
                                                    labelOptions = list(permanent = TRUE)
                                )

                        }

                    }
                })


       the_map %>% addLayersControl(
                       ## baseGroups = c("OSM (default)", "Toner", "Toner Lite"),
                       overlayGroups = features_to_plot,
                       options = layersControlOptions(collapsed = TRUE)
                   ) %>%
           onRender(
               "function(el, x) {
          L.control.zoom({position:'topright'}).addTo(this);
        }")
    })

    output$tempdir_path <- renderPrint(shape_dir_name)
    output$downloadData <- downloadHandler(contentType = 'application/zip',
                                           filename = function() {
                                               paste0("data-", Sys.Date(), ".zip")
                                           },
                                           content = function(file) {
                                               zip::zip(zipfile = file,
                                                        ## zipfile = file.path(shape_dir_name,'shapes.zip'),
                                                        files = dir(shape_dir_name, full.names = TRUE),
                                                        mode = 'cherry-pick'
                                                        )
                                               }
                                           )
    
}





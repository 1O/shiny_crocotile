## extracts zipped shp and returns it as sf:
## zip_name = file name without extension ".zip"
read_sf_from_zip <- function(zip_name,res_base = './www/data'){
    ## empty tmp directory:
    file.remove(list.files(tempdir(),
                           full.names = T, pattern = ".*")
                )

    zip_path <- file.path(res_base,paste0(zip_name,'.zip'))
    unzip(zip_path, overwrite = TRUE, exdir = tempdir())
    shp_path <- file.path(
        tempdir(),
        list.files(tempdir(),'\\.shp')[1]
    )
    sf::read_sf(shp_path) %>%
        st_transform(3857)
}


## generate some test features:

generate_test_features <- function(resource_base = './www/data/'){
    test_features <- list(
        pointInTile33TVN = st_point(c(14,47)) %>% st_sfc(crs = 4326), # in Carinthia/Austria
        pointInMultipleNaturaBBs = st_point(c(1715085, 6163138)) %>% st_sfc(crs = 3857),
        pointMiddleOfNowhere = st_point(c(43.29092044, 19.86346448)) %>% st_sfc(crs = 4326),
        bbox_across_eo_tiles = read_sf_from_zip('bbox_across_eo_tiles'),
        caseA = read_sf_from_zip('lammi'),
        svartberget = read_sf_from_zip('svartberget'),        
        caseC = read_sf_from_zip('zone_atelier_alps'),
        caseD = read_sf_from_zip('Donana')
    )
    test_features$caseB  <-  test_features$svartberget %>%
        ## st_geometry %>%
        st_cast('POLYGON') %>% .[c(1,10),] %>% st_union


    test_features
}


get_corner <- function(feature){
    bbox <- feature %>% st_transform(3857) %>% st_bbox
    XY <- st_sfc(st_point(bbox[c(1,4)])) %>% st_set_crs(3857) %>%
        st_transform(4326) %>% st_coordinates
    data.frame(lng = XY[1], lat = XY[2])
}



get_bounding_circle <- function(feat){
    feat <- feat %>% st_transform(.,3857) %>%
        st_union %>% st_convex_hull

    perimeter <- 
        feat %>% st_transform(3857) %>%
        st_union %>% st_cast('MULTILINESTRING') %>% st_length

    circle <- list()
    circle$feature <- 
        if(feat %>% st_area){
            feat %>% lwgeom::st_minimum_bounding_circle(.)
        } else {
            ## make a tiny dot from zero-area point feature:
            feat %>% st_buffer(1e-3)
        }
    circle$diam <- 2 * sqrt(st_area(circle$feature)/pi)
    circle$perimeter <- perimeter
    circle
}



get_covering_eo_tiles <- function(feat, eo_tiles){
    feat <- feat %>% st_geometry %>% st_transform(3857)
    z <- st_overlaps(feat, eo_tiles, sparse = TRUE) %>% unlist
    eo_tiles <- eo_tiles[z,]
    if(nrow(eo_tiles) > 0){
        covering_eo_tiles <- eo_tiles %>%
            rowwise %>%
            mutate(overlap_area = st_crop(feat,geometry) %>% st_area) %>%
            arrange(overlap_area)
    } else { covering_eo_tiles <- NULL }
    covering_eo_tiles
}

get_eo_tile_with_largest_overlap <- function(feat, eo_tiles){
    get_covering_eo_tiles(feat, eo_tiles) %>% tail(1)
}


## tests whether a geometries of a multi... feature 
## are scattered over a large area:
is_strongly_scattered <- function(feat){   
    feat <- feat %>% st_union

    ## don't proceed if feature has no area:
    if(!(feat %>% st_geometry %>% st_convex_hull %>% st_area)){ 
        return(list(
            result = FALSE,
            buffered_feat = feat
        ))
    }

    bounding_circle <- feat %>%
        st_convex_hull %>% 
        get_bounding_circle(.)
    polygon_perims_length <- 
        if(feat %>% st_area){
            feat %>%
                st_union %>%
                st_cast('MULTILINESTRING') %>% # (3)
                st_length %>% as.double
        } else {
            ## perim for circle with area A = (4*A*pi)^.5
            ## in case, only centroids are given, assume a minimum plot
            ## area of half a hectare (5000 m²)
            min_plot_area <- 5000
            min_plot_perimeter <- sqrt(4*min_plot_area*pi)
            ## buffer point feature(s) to cover at least
            feat <- feat %>% st_buffer(sqrt(min_plot_area/pi))
            ## return minimum perimeter times separate plots:
            subplot_count <- feat %>% st_cast('POLYGON') %>% st_geometry %>% length
            min_plot_perimeter * subplot_count
        } ## tiny but not zero, otherwise div by zero below
    R <- as.double(bounding_circle$diam) / polygon_perims_length #
    strongly_scattered <- R >= 1
    ## if R >= 1, proceed with each single poly of multipolygon:
    if(R >= 1){
        polys <- feat %>% st_cast('POLYGON')
    }



    return(list(
        result = strongly_scattered,
        buffered_feat = feat
    ))
    
    ## accepts a feature of two or more elements,
    ## scales radius by all elements' cumulative perimeter 
    ## and decides from this ratio whether the feature is scattered    
}

## decide whether this site covers a very large area
check_vast_area <- function(feat, country_shapes ){
    feat <- st_transform(feat, 3857)
    feat_area <- 1e-6 * st_area(feat) %>% as.double %>% sum # (1)
    country_codes_affected <- # (2)
        country_shapes$CNTR_CODE[st_intersects(feat, country_shapes) %>% unlist]


    treshold_area <- treshold_areas_caseC %>% # (3)
        filter(CNTR_CODE %in% country_codes_affected) %>%
        pull(area) %>% max(na.rm = TRUE)    


    list(
        is_vast_area = feat_area > treshold_area
    )

    ## takes a feature and checks whether its area (1) exceeds a treshold (3)
    ## (2) checks which countries are overlapped by this feature
    ## (3) country-wise tresholds are (tentatively) predefined as each country's
    ## 80th percentile of NUTS2 areas; maximum of affected countries' threshold decides
}


## diagnoses cases A-D, returns X if no valid feature supplied
diagnose_case <- function(feat, nat2k_coverage){
    if(is.null(feat)){
        return('X')
    }

    g <- feat %>% st_geometry %>% st_transform(3857)
    cat(paste("\n hier die nat2k coverage:", nrow(nat2k_coverage),"\n\n"))
    ## feature outside Natura 2000
    ## if(!is_in_nat2k_regimen(feat, nat2k_coverage)){ 
    ##     return('Y')
    ## }


    ## feature with subunits (multipolygon etc.)?
    is_multi <- g %>% st_geometry_type %>% grepl('MULTI',.) 

    diagnosis <- 
        if(!is_multi && st_geometry_type(g) == 'POINT'){
            'A'
        } else if(is_multi && (is_strongly_scattered(g) %>% .$result)){
            ## alternatively consider F-Test
            'B'
        } else if(check_vast_area(g, country_shapes = nuts_countries_3857)$is_vast_area) {
            cat('decided C')
            'C'
        } else {
            cat('decided D')
            'D'
        }
    diagnosis
}



## helper to construct a searchbeam: circular or (as suggested by Ioannis/Lefteris) square.
## Last argument: diameter / edge length of beam
get_searchbeam <- function(feat, beam_shape = 'circle', size = 1e3){ 
    cat('entering get searchbeam')

    tryCatch({
        feat <- feat %>%
            st_geometry(.) %>% st_transform(.,3857) %>%
            st_centroid
    },error = function(e)cat(file = stderr(), 'feature passt ned'))
    
    
    beam <- if(beam_shape == 'circle'){
                tryCatch(
                {
                    st_buffer(feat,size/2)
                },
                error = function(e)cat(file = stderr(), paste('buffern misslungen', feat$name))               
                )
            } else {
                tryCatch( 
                {
                    ## else default to square searchbeam:  
                    tryCatch({
                        point_mat <- 0.5 * size * ## half edge length
                            cbind(c(-1,1,1,-1,-1),c(-1,-1,1,1,-1)) +
                            ## thats the point: if, as in case B, the feature has
                            ## more polygons,
                            ## merge them, so that only 1 centroid is produced!
                            ## actually the search beam shouldn't apply for a multiple site thing at all
                            rep(st_coordinates(st_centroid(feat)),each = 5)
                    }, error = function(e)cat(file = stderr(), paste('failed with point matrix:',e)))
                    tryCatch({
                        st_polygon(list(point_mat))
                    }, error = function(e)cat(file = stderr(), 'polygon isslungen'))
                }
              , error = function(e)cat(file = stderr(), 'failed constructing square buffer')
                )
            }
    beam  #%>% st_sfc %>% st_set_crs(.,3857)
}


get_treshold_ratio  <- function(feature){
    feature  <- feature %>% st_geometry %>% st_transform(3857) %>% st_union
    perimeter <- feature %>% st_cast('MULTILINESTRING') %>% st_length %>% as.double
    bcircle <- lwgeom::st_minimum_bounding_circle(feature)
    diameter <- bcircle %>% st_bbox %>% .[c(3,1)] %>% dist %>% as.double
    diameter / perimeter
}

get_majority_tile <- function(feat,eo_tiles){
    feat <- feat %>% st_union %>% st_transform(3857)
    z <- c(feat %>% st_overlaps(eo_tiles),
           feat %>% st_within(eo_tiles)) %>% unlist %>% unique
    relevant_eo_tiles <- eo_tiles[z,]
    relevant_eo_tiles %>%
        mutate(overlap = st_crop(feat, geometry) %>% st_area) %>%
        arrange(overlap) %>% tail(1)
}

get_nearest_nat2k <- function(feat, nat2k_bboxes, nat2k_polys){
    xy <- feat %>% st_centroid %>% st_geometry %>% st_coordinates %>% as.vector
    searchbeam <- 
        ((rbind(c(-1,1,1,-1,-1), c(-1,-1,1,1,-1)) * 500) + xy) %>% t %>%
        st_linestring %>% st_cast('POLYGON')

    covering_indices <- 
        st_covered_by(searchbeam, nat2k_bboxes, sparse = TRUE) %>%
        unlist
    
    if(length(covering_indices)){ ## any bbox covers feature
        nearby_nat2k_polys <- nat2k_polys[covering_indices,]
        nearest_nat2k_index <- st_nearest_feature(
            feat %>% st_centroid,
            nearby_nat2k_polys %>% st_centroid
        )
        nearest_nat2k_polygon  <- if(length(covering_indices)){
                                      nearby_nat2k_polys[nearest_nat2k_index,]
                                  } else {
                                      NULL
                                  }
    } else {
        nearest_nat2k_polygon <- NULL
    }
    nearest_nat2k_polygon
}


buffer_feature <- function(feat){
    feat <- feat %>% st_transform(3857) %>%
        st_geometry %>% st_union %>% st_convex_hull
    perimeter <- feat %>% st_length %>% as.double
    boundary_circle <- lwgeom::st_minimum_bounding_circle(feat)
}

buffer_by_area_single <- function(feature, zoom){
    feature <- feature %>% st_geometry %>% st_transform(3857)
    ## if feature is point, buffer by 1 metre:
    if(!st_area(feature)) feature <- st_buffer(feature, 1)
    ## secant approximation taken from {cmna}
    secant <- 
        function (f, x, tol = 0.001, m = 100){
            i <- 0
            oldx <- x
            oldfx <- f(x)
            x <- oldx + 10 * tol
            while (abs(x - oldx) > tol) {
                i <- i + 1
                if (i > m)
                    stop("No solution found")
                fx <- f(x)
                newx <- x - fx * ((x - oldx)/(fx - oldfx))
                oldx <- x
                oldfx <- fx
                x <- newx
            }
            return(x)
        }
    bbox <- st_bbox(feature)
    bbox_diagonal <- sqrt(dist(bbox[c(1,3)])^2 + dist(bbox[c(2,4)])^2)
    start_value <- abs(bbox_diagonal * (sqrt(zoom) - 1))
    start_area <- st_area(feature) %>% as.double
    target_area <- start_area * zoom
    buffer_dist <- secant(f = function(buffer_dist){
        as.double(st_area(st_buffer(feature, buffer_dist))) - target_area
    }, x = start_value, tol = .01, m = 20)
    st_buffer(feature, buffer_dist) %>%
        st_geometry
}


buffer_by_area_group <- function(feature, zoom){    
    feature %>%
        st_geometry %>%
        st_transform(3857) %>%
        as_tibble %>%
        mutate(ID = row_number(), .before = 1) %>%
        rowwise %>%
        mutate(poly = list(st_cast(geometry, 'POLYGON'))) %>%
        unnest(poly) %>%
        rowwise %>%
        mutate(
            poly_buffered = buffer_by_area_single(poly, zoom)
        ) %>%
        group_by(ID) %>%
        summarise(across(contains('poly'), st_union))
}


## return nearest Natura2000 boundary with above helper
## or return nothing when no Nat2k within e.g. 100×100 km2
## (as suggested by Ioannis et al. for Case A):
get_nearest_n2k_old_version <- function(feat, nat2k_polys, nat2k_bboxes){
    feat <- feat %>%
        st_union %>%
        get_bounding_circle(.) %>%
        .$feature
    tryCatch({
        ## keep only SITECODE from attribute table (performance)
        naturas <- nat2k_polys %>% select(SITECODE)
        bboxes <- nat2k_bboxes %>% select(SITECODE)
    }, error = function(e)cat(file = stderr(), 'error hugo'))
    get_sitecodes_of_matches <- function(feat, polys){
        tryCatch({        
            feat <- feat #%>% st_transform(.,3857) %>% st_set_crs(.,3857)
            polys <- polys# %>% st_transform(.,3857) %>% st_set_crs(.,3857)
            polys$SITECODE %>% .[st_within(feat, polys) %>% unlist %>% unique]
        }, error = function(e)cat(file = stderr(), paste('error trying get_sitecodes_of_matches', feat$name)))
    }

    get_nearest_poly_from_candidates <- function(candidate_sitecodes){
        tryCatch({
            closeby_naturas <- naturas %>% 
                filter(SITECODE %in% candidate_sitecodes)
            st_nearest_feature(feat, closeby_naturas) %>% 
                closeby_naturas$SITECODE[.]
        },
        error = function(e) paste('error trying get_nearest_poly_from_candidates')
        )
    }

    ## which (if any) Natura 2000 bounding boxes cover the site?
    bbox_discoveries <- get_sitecodes_of_matches(feat, bboxes)

    if(!length(bbox_discoveries)){
        ## if site is not covered (even by) a Natura2000 bounding box
        ## retry search within an circle or square buffer around site:

        ## tryCatch({
        searchbeam <- get_searchbeam(feat,
                                     'square', # one of 'circle' or 'square'
                                     1e5) ## diameter or edge length in m
        ## }, error =  function(e)cat(file = stderr(), paste('error karl:',e)))
        bbox_discoveries_with_searchbeam <- 
            get_sitecodes_of_matches(searchbeam, bboxes)
        
        ifelse(bbox_discoveries_with_searchbeam,
               get_nearest_poly_from_candidates(bbox_discoveries_with_searchbeam),
               NA) ## if buffering site doesn't help, return NA
        
    } else {
        ## else, if site is at least within a Natura 2000 bounding box
        get_nearest_poly_from_candidates(bbox_discoveries)
    }
}

## dies checken !!!!!!!!!!!!!!!!!!!!
is_in_nat2k_regimen <- function(feat, nat2k_coverage){
    feat <- feat %>% st_geometry %>% st_transform(3857) 
    cat("achtung")
    coverage <- nat2k_coverage %>% st_transform(3857)
    overlaps  <- st_overlaps(coverage, feat, sparse = TRUE) %>%
        unlist %>% length %>% as.logical
    within  <-  st_within(feat, coverage, sparse = TRUE) %>%  unlist %>%
        length %>% as.logical
    overlaps | within
}


get_relevant_nat2k_for_vast_area <- function(feat,
                                             nat2k_bboxes,
                                             nat2k_polys,
                                             nat2k_habitat_diversity,
                                             eo_tiles                                             
                                             ){
    feat <- feat %>% st_transform(3857) %>% st_union %>% st_geometry
    ## filter relevant bounding boxes of nat2k polygons
    z <- c(
        ## uncomment to include cross-border nat2k areas:
        ## feat %>% st_overlaps(nat2k_bboxes) %>% unlist,
        st_covers(feat, nat2k_bboxes, sparse = TRUE) %>% unlist %>% unique
    )

    ## preselect relevant nat2polygons for performance
    preselected_nat2k_polys <- nat2k_polys [z,]

    z <- c(
        ## uncomment to include cross-border nat2k areas:
        ## feat %>% st_overlaps(preselected_nat2k_polys),
        feat %>% st_covers(preselected_nat2k_polys, sparse = TRUE)
    ) %>% unlist %>% unique


    
    relevant_nat2k_polys <- preselected_nat2k_polys[z,] %>%
        left_join(nat2k_habitat_diversity, by = c('SITECODE'='SITECODE'))
    
    total_nat2k_area <- relevant_nat2k_polys %>% pull(SITEAREA) %>% sum(na.rm = TRUE) %>% as.double



    ## large_biodiverse_nat2k_polys <- 
    relevant_nat2k_polys <- 
        relevant_nat2k_polys %>%    
        filter(
        (!is.na(SITEAREA)) & (SITEAREA > median(SITEAREA, na.rm = TRUE))
        )  %>%        
        arrange(desc(HABITATCOUNT)) %>%
        mutate(CUMAREA = cumsum(SITEAREA))

    large_biodiverse_nat2k_polys <- if(relevant_nat2k_polys$CUMAREA[1] >  .1 * total_nat2k_area){
                                        ## return first entry, if this nat2k poly 
                                        ## already exceeds the required area
                                        relevant_nat2k_polys[1,]
                                    } else {
                                        relevant_nat2k_polys %>%
                                            filter(CUMAREA < .1 * total_nat2k_area)
                                    }



    ## find majority EO tile containing most of the relevant nat2ks:

    large_biodiverse_nat2k_polys <- 
        large_biodiverse_nat2k_polys %>% st_geometry %>% st_union

   
    relevant_eo_tile <- 
        get_eo_tile_with_largest_overlap(feat = large_biodiverse_nat2k_polys, 
                                         eo_tiles = eo_tiles)
    ## now select all nat2k sites within the relevant tile!    
    ## return nat2k polygons falling within the majority eo_tile:
    z <- st_overlaps(relevant_eo_tile,
                     large_biodiverse_nat2k_polys,
                     sparse = TRUE) %>%
        unlist %>% unique

    selected_nat2k_polys <- 
        relevant_nat2k_polys[z,] %>% as_tibble

    ## convert selected_nat2k_polys from MULTIPOLYGON to POLYGON
    ## for performance:
    selected_nat2k_polys_shards <- 
        selected_nat2k_polys %>%
        rowwise %>%
        mutate(geometry_new = list(geometry %>% st_cast('POLYGON'))) %>%
        unnest(geometry_new) %>%
        select(SITECODE, geometry_new)



    zoom  <- 1 + get_treshold_ratio(selected_nat2k_polys_shards$geometry_new)
    selected_nat2k_polys_buffered <- 
        buffer_by_area_group(selected_nat2k_polys_shards$geometry_new, zoom)[['geometry_buffered']]


    bbox <- selected_nat2k_polys_buffered
    ## st_bbox(selected_nat2k_polys_buffered ) %>% st_as_sfc
    ## skip the buffering step - TODO: how to buffer by percent area?
    ## create bounding circle
    ## bounding_circle <- get_bounding_circle(relevant_nat2k_polys)
    ## buffer_perc <- 100 * bounding_circle$diam / bounding_circle$perimeter


    list(
        site = feat,
        selected_nat2k_polys = selected_nat2k_polys$geometry,
        selected_nat2k_polys_buffered = selected_nat2k_polys_shards,
        selected_nat2k_polys_shards = selected_nat2k_polys_shards,
        relevant_eo_tile = relevant_eo_tile,
        large_biodiverse_nat2k_polys = large_biodiverse_nat2k_polys,        
        relevant_nat2k_polys = relevant_nat2k_polys,
        relevant_eo_tile = relevant_eo_tile,
        bbox = bbox
    )
}


emit_progress  <- function(i, n){
}

## reads shape files AND emits progress (between 0 and 1)
readOGR_with_progress <-
function (dsn, layer, verbose = TRUE, p4s = NULL, stringsAsFactors = as.logical(NA), 
    drop_unsupported_fields = FALSE, pointDropZ = FALSE, dropNULLGeometries = TRUE, 
    useC = TRUE, disambiguateFIDs = FALSE, addCommentsToPolygons = TRUE, 
    encoding = NULL, use_iconv = FALSE, swapAxisOrder = FALSE, 
    require_geomType = NULL, integer64 = "no.loss", GDAL1_integer64_policy = FALSE, 
    morphFromESRI = NULL, dumpSRS = FALSE, enforce_xy = NULL, 
    D3_if_2D3D_points = FALSE, missing_3D = 0) 
{
    if (missing(dsn)) 
        stop("missing dsn")
    stopifnot(is.character(dsn))
    stopifnot(length(dsn) == 1L)
    if (length(dsn) == 1 && file.exists(dsn)) 
        dsn <- enc2utf8(normalizePath(dsn))
    if (nchar(dsn) == 0) 
        stop("empty name")
    if (missing(layer)) {
        layers <- ogrListLayers(dsn = dsn)
        if (length(layers) == 0L) 
            stop("missing layer")
        if (length(layers) > 0L) 
            layer <- c(layers[1])
        if (length(layers) > 1L) 
            warning("First layer ", layer, " read; multiple layers present in\n", 
                dsn, ", check layers with ogrListLayers()")
    }
    else layer <- enc2utf8(layer)
    if (nchar(layer) == 0) 
        stop("empty name")
    integer64 <- match.arg(integer64, c("allow.loss", "warn.loss", 
        "no.loss"))
    int64 <- switch(integer64, allow.loss = 1L, warn.loss = 2L, 
        no.loss = 3L)
    if (GDAL1_integer64_policy) 
        int64 <- 4L
    stopifnot(is.logical(use_iconv))
    stopifnot(length(use_iconv) == 1)
    if (!is.null(encoding)) {
        stopifnot(is.character(encoding))
        stopifnot(length(encoding) == 1)
    }
    WKB <- c("wkbPoint", "wkbLineString", "wkbPolygon", "wkbMultiPoint", 
        "wkbMultiLineString", "wkbMultiPolygon", "wkbGeometryCollection")
    if (!is.null(require_geomType)) {
        stopifnot(is.character(require_geomType) && length(require_geomType) == 
            1)
        m_require_geomType <- match(require_geomType, WKB)
        stopifnot(!is.na(m_require_geomType) || m_require_geomType <= 
            3)
    }
    if (is.na(stringsAsFactors)) {
        stringsAsFactors <- if (getRversion() < "4.1.0") 
            default.stringsAsFactors()
        else FALSE
    }
    suppressMessages(ogr_info <- ogrInfo(dsn = dsn, layer = layer, 
        encoding = encoding, use_iconv = use_iconv, swapAxisOrder = swapAxisOrder, 
        require_geomType = require_geomType, morphFromESRI = morphFromESRI, 
        dumpSRS = dumpSRS, D3_if_2D3D_points = D3_if_2D3D_points))
    HAS_FEATURES <- TRUE
    if (!ogr_info$have_features) {
        if (dropNULLGeometries) {
            stop("no features found")
        }
        else {
            warning("no features found; proceeding to atttributes only")
            HAS_FEATURES <- FALSE
        }
    }
    if (is.null(ogr_info$nListFields)) 
        nListFields <- 0
    else nListFields <- ogr_info$nListFields
    if (ogr_info$nitems > 0) {
        nodata_flag <- FALSE
        if (strsplit(getGDALVersionInfo(), " ")[[1]][2] < "2") {
            keep <- ogr_info$iteminfo$typeName %in% c("Integer", 
                "Real", "String", "Date", "Time", "DateTime", 
                "IntegerList", "RealList", "StringList")
        }
        else {
            keep <- ogr_info$iteminfo$typeName %in% c("Integer", 
                "Real", "String", "Date", "Time", "DateTime", 
                "IntegerList", "RealList", "StringList", "Integer64", 
                "Integer64List")
        }
        if (nListFields > 0) 
            ListFields <- as.integer(ogr_info$iteminfo$maxListCount)
        if (drop_unsupported_fields) {
            iflds <- as.integer((1:ogr_info$nitems) - 1)
            iflds <- iflds[keep]
            fldnms <- ogr_info$iteminfo$name[keep]
            if (nListFields > 0) 
                ListFields <- ListFields[keep]
            if (any(!keep)) 
                warning(paste("Fields dropped:", paste(ogr_info$iteminfo$name[!keep], 
                  collapse = " ")))
        }
        else {
            if (any(!keep)) 
                stop(paste("Unsupported field type:", paste(ogr_info$iteminfo$typeName[!keep], 
                  collapse = " ")))
            iflds <- as.integer((1:ogr_info$nitems) - 1)
            fldnms <- ogr_info$iteminfo$name
        }
        int64_found <- grep("Integer64", ogr_info$iteminfo$typeName)
    }
    else {
        int64_found <- integer(0L)
        nodata_flag <- TRUE
        iflds <- integer(0)
    }
    fids <- ogrFIDs(dsn = dsn, layer = layer)
    if (attr(fids, "i") != attr(fids, "nf")) {
        retain <- 1:attr(fids, "i")
        afids <- 0:(attr(fids, "nf") - 1)
        deleted <- afids[!(afids %in% fids[retain])]
        warning(paste("Deleted feature IDs: ", paste(deleted, 
            collapse = ", ")))
        fids <- fids[retain]
    }
    else {
        retain <- NULL
    }
    attributes(fids) <- NULL
    non_unique_fids <- max(table(fids)) > 1
    if (non_unique_fids) {
        if (disambiguateFIDs) {
            fids <- seq_along(fids)
        }
        else {
            stop("FIDs not unique")
        }
    }
    if (verbose) {
        cat("OGR data source with driver:", ogr_info$driver, 
            "\n")
        cat("Source: \"", dsn, "\", layer: \"", layer, "\"", 
            "\n", sep = "")
        cat("with", length(fids), "features")
        if (!is.null(attr(ogr_info, "require_geomType"))) 
            cat(";\nSelected", attr(ogr_info, "require_geomType"), 
                "feature type, with", sum(attr(ogr_info, "keepGeoms")), 
                "rows")
        cat("\n")
        cat("It has", length(iflds), "fields")
        if (nListFields > 0) 
            cat(", of which", nListFields, "list fields")
        cat("\n")
        if (length(int64_found > 0L)) {
            if (GDAL1_integer64_policy) {
                cat("Integer64 fields read as doubles: ", paste(fldnms[int64_found], 
                  collapse = " "), "\n")
            }
            else {
                if (integer64 == "no.loss") {
                  cat("Integer64 fields read as strings: ", paste(fldnms[int64_found], 
                    collapse = " "), "\n")
                }
                else {
                  cat("Integer64 fields read as signed 32-bit integers: ", 
                    paste(fldnms[int64_found], collapse = " "), 
                    "\n")
                }
            }
        }
    }
    if (!use_iconv && !is.null(encoding) && ogr_info$driver == 
        "ESRI Shapefile") {
        oSE <- getCPLConfigOption("SHAPE_ENCODING")
        tull <- setCPLConfigOption("SHAPE_ENCODING", encoding)
    }
    if (nodata_flag) {
        dlist <- list(FID = as.integer(fids))
    }
    else {
        attr(iflds, "nListFields") <- as.integer(nListFields)
        nflds <- length(iflds)
        if (nListFields > 0) {
            attr(iflds, "ListFields") <- ListFields
            nflds <- nflds + sum(ListFields) - nListFields
            fldnms1 <- NULL
            for (i in seq(along = ListFields)) {
                if (ListFields[i] == 0) 
                  fldnms1 <- c(fldnms1, fldnms[i])
                else fldnms1 <- c(fldnms1, paste(fldnms[i], 1:ListFields[i], 
                  sep = ""))
            }
            stopifnot(length(fldnms1) == nflds)
            fldnms <- fldnms1
        }
        attr(iflds, "nflds") <- as.integer(nflds)
        attr(iflds, "int64") <- as.integer(int64)
        dlist <- .Call("ogrDataFrame", as.character(dsn), enc2utf8(as.character(layer)), 
            as.integer(fids), iflds, PACKAGE = "rgdal")
        names(dlist) <- make.names(fldnms, unique = TRUE)
        if (use_iconv && !is.null(encoding)) {
            for (i in seq(along = dlist)) {
                if (is.character(dlist[[i]])) {
                  dlist[[i]] <- iconv(dlist[[i]], from = encoding)
                }
            }
        }
    }
    if (!use_iconv && !is.null(encoding) && ogr_info$driver == 
        "ESRI Shapefile") {
        tull <- setCPLConfigOption("SHAPE_ENCODING", oSE)
    }
    data <- data.frame(dlist, row.names = fids, stringsAsFactors = stringsAsFactors)
    rm(dlist)
    gc(verbose = FALSE)
    if (!HAS_FEATURES) {
        return(data)
    }
    prj <- ogr_info$p4s
    if (!is.null(p4s)) {
        if (!is.na(prj)) {
            warning("p4s= argument given as: ", p4s, "\n and read as: ", 
                c(prj), "\n read string overridden by given p4s= argument value")
        }
    }
    else {
        p4s <- prj
    }
    if (!is.na(p4s) && nchar(p4s) == 0) 
        p4s <- as.character(NA)
    oCRS <- new("CRS", projargs = p4s)
    if (new_proj_and_gdal()) 
        comment(oCRS) <- ogr_info$wkt2
    geometry <- .Call("R_OGR_CAPI_features", as.character(dsn), 
        enc2utf8(as.character(layer)), comments = addCommentsToPolygons, 
        PACKAGE = "rgdal")
    if (is.null(retain)) {
        eType <- geometry[[4]]
        with_z <- geometry[[6]]
        isNULL <- as.logical(geometry[[7]])
        gFeatures <- geometry[[5]]
        gComments <- geometry[[8]]
    }
    else {
        eType <- geometry[[4]][retain]
        with_z <- geometry[[6]][retain]
        isNULL <- as.logical(geometry[[7]])[retain]
        gFeatures <- geometry[[5]][retain]
        gComments <- geometry[[8]][retain]
    }
    rm(geometry)
    gc(verbose = FALSE)
    if (any(isNULL)) {
        eType <- eType[!isNULL]
        with_z <- with_z[!isNULL]
    }
    elevated <- attr(ogr_info$with_z, "elevated")
    u_with_z <- unique(sort(with_z))
    if (length(u_with_z) != 1L) {
        if (elevated) {
            if (pointDropZ) {
                warning("Dropping mixed third dimension")
                u_with_z <- 0L
            }
            else {
                warning("Setting any missing third dimension to ", 
                  missing_3D)
                u_with_z <- 1L
            }
        }
        else {
            stop(paste("Multiple # dimensions:", paste((u_with_z + 
                2), collapse = ":")))
        }
    }
    if (u_with_z < 0 || u_with_z > 1) 
        stop(paste("Invalid # dimensions:", (u_with_z + 2)))
    eType[eType == 5L] <- 2L
    eType[eType == 6L] <- 3L
    u_eType <- unique(sort(eType))
    t_eType <- table(eType)
    if (is.null(require_geomType)) {
        keepGeoms <- NULL
        if (length(u_eType) > 1L) 
            stop(paste("Multiple incompatible geometries:", paste(paste(WKB[as.integer(names(t_eType))], 
                t_eType, sep = ": "), collapse = "; ")))
    }
    else {
        if (!require_geomType %in% WKB[as.integer(names(t_eType))]) 
            stop(require_geomType, "not in", WKB[as.integer(names(t_eType))])
        u_eType <- match(require_geomType, WKB)
        keepGeoms <- WKB[eType] == require_geomType
        message("NOTE: keeping only ", sum(keepGeoms), " ", require_geomType, 
            " of ", length(keepGeoms), " features\n")
    }
    if (length(gFeatures) != length(fids)) 
        stop("Feature mismatch")
    if (any(isNULL)) {
        if (dropNULLGeometries) {
            warning(paste("Dropping null geometries:", paste(which(isNULL), 
                collapse = ", ")))
            gFeatures <- gFeatures[!isNULL]
            data <- data[!isNULL, , drop = FALSE]
            fids <- fids[!isNULL]
            gComments <- gComments[!isNULL]
        }
        else {
            warning(paste("Null geometries found:", paste(which(isNULL), 
                collapse = ", ")))
            warning("dropNULLGeometries FALSE, returning only data for null-geometry features")
            return(data[isNULL, , drop = FALSE])
        }
    }
    if (!is.null(require_geomType)) {
        gFeatures <- gFeatures[keepGeoms]
        data <- data[keepGeoms, , drop = FALSE]
        fids <- fids[keepGeoms]
        gComments <- gComments[keepGeoms]
    }
    if (u_eType == 1) {
        if (u_with_z == 0 || pointDropZ) {
            if (swapAxisOrder) {
                coords <- do.call("rbind", lapply(gFeatures, 
                  function(x) c(x[[1]][[2]], x[[1]][[1]])))
            }
            else {
                coords <- do.call("rbind", lapply(gFeatures, 
                  function(x) c(x[[1]][[1]], x[[1]][[2]])))
            }
        }
        else {
            if (elevated) {
                if (swapAxisOrder) {
                  coords <- do.call("rbind", lapply(gFeatures, 
                    function(x) c(x[[1]][[2]], x[[1]][[1]], ifelse(length(x[[1]]) == 
                      2L, missing_3D, x[[1]][[3]]))))
                }
                else {
                  coords <- do.call("rbind", lapply(gFeatures, 
                    function(x) c(x[[1]][[1]], x[[1]][[2]], ifelse(length(x[[1]]) == 
                      2L, missing_3D, x[[1]][[3]]))))
                }
            }
            else {
                if (swapAxisOrder) {
                  coords <- do.call("rbind", lapply(gFeatures, 
                    function(x) c(x[[1]][[2]], x[[1]][[1]], ifelse(length(x[[1]]) == 
                      2L, missing_3D, x[[1]][[3]]))))
                }
                else {
                  coords <- do.call("rbind", lapply(gFeatures, 
                    function(x) c(x[[1]][[1]], x[[1]][[2]], ifelse(length(x[[1]]) == 
                      2L, missing_3D, x[[1]][[3]]))))
                }
            }
        }
        row.names(data) <- NULL
        res <- SpatialPointsDataFrame(coords = coords, data = data, 
            proj4string = oCRS)
    }
    else if (u_eType == 2) {
        if (u_with_z != 0) 
            warning("Z-dimension discarded")
        n <- length(gFeatures)
        lnList <- vector(mode = "list", length = n)
        for (i in 1:n) {
          emit_progress(i, n)
            iG <- gFeatures[[i]]
            m <- length(iG)
            lnlist <- vector(mode = "list", length = m)
            for (j in 1:m) {
                jG <- iG[[j]]
                if (swapAxisOrder) {
                  lnlist[[j]] <- Line(cbind(jG[[2]], jG[[1]]))
                }
                else {
                  lnlist[[j]] <- Line(cbind(jG[[1]], jG[[2]]))
                }
            }
            lnList[[i]] <- Lines(lnlist, ID = as.character(fids[i]))
        }
        SL <- SpatialLines(lnList, proj4string = oCRS)
        res <- SpatialLinesDataFrame(SL, data)
    }
    else if (u_eType == 3) {
        if (u_with_z != 0) 
            warning("Z-dimension discarded")
        if (useC) {
            n <- length(gFeatures)
            plList <- vector(mode = "list", length = n)
            for (i in 1:n) {
          emit_progress(i, n)

                iG <- gFeatures[[i]]
                if (swapAxisOrder) {
                  iG <- lapply(iG, function(x) {
                    tmp <- x[[1]]
                    x[[1]] <- x[[2]]
                    x[[2]] <- tmp
                  })
                }
                if (addCommentsToPolygons) {
                  thisPL <- Polygons(.Call("make_Polygonlist", 
                    iG, gComments[[i]], PACKAGE = "rgdal"), ID = as.character(fids[i]))
                  comment(thisPL) <- paste(gComments[[i]], collapse = " ")
                }
                else {
                  thisPL <- Polygons(.Call("make_Polygonlist", 
                    iG, NULL, PACKAGE = "rgdal"), ID = as.character(fids[i]))
                }
                plList[[i]] <- thisPL
            }
        }
        else {
            n <- length(gFeatures)
            plList <- vector(mode = "list", length = n)
            for (i in 1:n) {
          emit_progress(i, n)
                iG <- gFeatures[[i]]
                if (swapAxisOrder) {
                  iG <- lapply(iG, function(x) {
                    tmp <- x[[1]]
                    x[[1]] <- x[[2]]
                    x[[2]] <- tmp
                  })
                }
                m <- length(iG)
                pllist <- vector(mode = "list", length = m)
                for (j in 1:m) {
                  jG <- iG[[j]]
                  cmat <- cbind(jG[[1]], jG[[2]])
                  if (!identical(cmat[1, ], cmat[nrow(cmat), 
                    ])) {
                    cmat <- rbind(cmat, cmat[1, ])
                    warning(paste("Ring closed in Polygons", 
                      i, "Polygon", j))
                  }
                  t0 <- try(pllist[[j]] <- Polygon(cmat), silent = TRUE)
                  if (inherits(t0, "try-error")) {
                    stop("i: ", i, ", j: ", j, ", Polygon error exit")
                  }
                }
                thisPL <- Polygons(pllist, ID = as.character(fids[i]))
                if (addCommentsToPolygons) {
                  comment(thisPL) <- paste(gComments[[i]], collapse = " ")
                  if (!isTRUE(all.equal(as.logical(gComments[[i]]), 
                    sapply(slot(thisPL, "Polygons"), slot, "hole")))) 
                    warning("comment/hole mismatch, geometry:", 
                      i)
                }
                plList[[i]] <- thisPL
            }
        }
        rm(gFeatures)
        gc(verbose = FALSE)
        SP <- SpatialPolygons(plList, proj4string = oCRS)
        rm(plList)
        gc(verbose = FALSE)
        res <- SpatialPolygonsDataFrame(SP, data, match.ID = FALSE)
    }
    else stop(paste("Incompatible geometry:", u_eType))
    res
}

handle_case <- function(feat,
                        case_type = 'X',
                        nat2k_bboxes,
                        nat2k_polys,
                        nat2k_habitat_diversity,
                        nat2k_coverage,
                        eo_tiles){


    z <- c(st_overlaps(feat, eo_tiles, sparse = TRUE) %>% unlist,
           st_contains(feat, eo_tiles, sparse = TRUE) %>% unlist,
           st_within(feat, eo_tiles, sparse = TRUE) %>% unlist
           ) %>% unique
    overlapping_eo_tiles <- eo_tiles[z,]

    switch(case_type,
           'A' = {
               nearest_nat2k <- get_nearest_nat2k(feat, nat2k_bboxes, nat2k_polys)
               if(is.null(nearest_nat2k)){
                   relevant_eo_tile <- get_nearest_nat2k(feat, eo_tiles, eo_tiles) 
                   nearest_nat2k  <- NULL
                   nearest_nat2k_buffered <- NULL
                   bbox <- st_bbox(relevant_eo_tile) %>% st_as_sfc
                   clipped_site <- bbox                   
               } else {
                   bounding_circle <- get_bounding_circle(nearest_nat2k)
                   nat2k_perimeter <- nearest_nat2k %>% 
                       st_geometry %>%
                       st_cast('MULTILINESTRING') %>%
                       st_length
                   buffer_perc <- bounding_circle$diam / nat2k_perimeter %>% as.double
                   buffer_dist <- bounding_circle$diam * as.double(buffer_perc)
                   nearest_nat2k_buffered <- nearest_nat2k %>% st_buffer(buffer_dist)
                   bbox <- nearest_nat2k_buffered %>% 
                       st_convex_hull %>% ## needed for bbox around multipolygons
                       st_bbox %>% st_as_sfc
                   relevant_eo_tile <- eo_tiles[unlist(st_intersects(bbox, eo_tiles)),] %>%
                       mutate(bbox_share = st_crop(geometry, bbox) %>% st_area) %>%
                       arrange(desc(bbox_share)) %>% .[1,]
                   clipped_site <- st_crop(relevant_eo_tile, bbox)
               }
               return(list(site = feat,
                           nearest_nat2k = nearest_nat2k,
                           nearest_nat2k_buffered = nearest_nat2k_buffered,
                           bbox = bbox,
                           clipped_site = clipped_site,
                           overlapping_eo_tiles = overlapping_eo_tiles,
                           relevant_eo_tile = relevant_eo_tile
                           ))                
           },
           'B' = {
               zoom <- 1 + get_treshold_ratio(feat)
               if(FALSE){
                   ## skip case 3a (R >= 1)
                   ## until clarification what to do with the single polys
                   ##            )
               } else { # if R < 1
                   res <- buffer_by_area_group(feat, zoom)
                   buffered_site <- res$poly_buffered
                   relevant_eo_tile <- get_majority_tile(buffered_site, eo_tiles)
                   clipped_site = st_crop(buffered_site, relevant_eo_tile)
                   bbox <- st_bbox(clipped_site) ## %>% st_as_sfc
               }
               return(list(site = feat,
                           buffered_site = buffered_site,
                           bbox = bbox,
                           overlapping_eo_tiles = overlapping_eo_tiles,
                           relevant_eo_tile = relevant_eo_tile,
                           clipped_site = clipped_site
                           )
                      )
           },
           'C' = {
               if(is_in_nat2k_regimen(feat, nat2k_coverage)){

                   res <- get_relevant_nat2k_for_vast_area(feat = feat, 
                                                           nat2k_bboxes = nat2k_bboxes,
                                                           nat2k_polys = nat2k_polys,
                                                           nat2k_habitat_diversity = nat2k_habitat_diversity,
                                                           eo_tiles = eo_tiles
                                                           )
                   res$bbox <- st_bbox(res$site)
                   res$overlapping_eo_tiles = overlapping_eo_tiles
                   return(res)
               } else {
                   res <- list(overlapping_eo_tiles = overlapping_eo_tiles,
                               bbox = st_bbox(feat),
                               site = feat
                               )
                   return(res)
               }
           },
           'D' = {
               relevant_eo_tile <- get_majority_tile(feat, eo_tiles)
               clipped_site  <- feat %>% st_crop(relevant_eo_tile)
               bbox = st_bbox(clipped_site)
               return(list(site = feat,
                           overlapping_eo_tiles = overlapping_eo_tiles,
                           relevant_eo_tile = relevant_eo_tile,
                           clipped_site = clipped_site,
                           bbox = bbox
                           )
                      )
           },
           'X' = {
               return()
           },
           'Y' = { ## site outside Natura 2000
               bbox  <- st_bbox(feat)
               return(list(site = feat,
                           overlapping_eo_tiles = overlapping_eo_tiles
                           )
                      )
           },
           cat('no matches')
           )
}

generate_readme <- function(site_name, site_id,
                            relevant_eo_tile,
                            file_stub, ...){
    md_file_name <- paste0(file_stub, '.md')
    con_md <- file(md_file_name)

    writeLines(text = c(
                   "---",
                   sprintf("title: EO-tile for DEIMS site %s", site_name),
                   "author: CrocoTile, an e-shape tool",
                   "output: html_document",
                   "---",
                   "",
                   "## Results",
                   "| param | value |",
                   "|:-------|:----|",
                   sprintf("|site name|%s|", site_name),
                   sprintf("|DEIMS ID|%s|", site_id),
                   sprintf("|web|[visit site on deims.org](https://deims.org/%s)|", site_id, site_id),
                   sprintf("|date|%s|", Sys.time()),
                   sprintf("|relevant EO tile|%s|", relevant_eo_tile),
                   "-----",
                   "## Concept",
                   "- Ioannis Manakos (imanakos@iti.gr)",
                   "- Eleftherios Katsikis (lefkats@iti.gr)",
                   "\n\n(both: Center of Research & Technology Hellas (CERTH)/ITI",
                   "see also Methods.pdf",
                   "\n\n## License",
                   "[Attribution 4.0 International (CC BY 4.0)](https://creativecommons.org/licenses/by/4.0/)",
                   "\n\n## Funding",
                   "CrocoTile is an **[e-shape](https://e-shape.eu/)** tool.
 e-shape has received funding from the [**European Union\'s H2020**
 research and innovation programme](https://cordis.europa.eu/article/id/421806-e-shape-shaping-the-eurogeo-initiative-and-delivering-eo-derived-benefits-with-and-for-users) under grant agreement 820852"
 ), 
 con = con_md)
    close(con_md)
    rmarkdown::render(md_file_name)
    unlink(md_file_name)
}

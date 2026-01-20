library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(RColorBrewer)
library(htmltools)
library(here)
library(bslib)
library(shinyBS)
library(stringr)
library(purrr)

# -------------------------------
# 1) Load & prepare spatial data
# -------------------------------
# LSOA boundaries (change the path and the join field name to match your file)
map_data <- st_read(here("Data","map_data.shp")) 

#check geometry exists - should return FALSE

any(is.na(sf::st_geometry(map_data)))

# check CRS

sf::st_crs(map_data)



# -----------------------------------------------------------
# 2) Load SSOT patient data and GP Practice Locations and make points
# -----------------------------------------------------------

SSOT_data <- read.csv(here("Outputs","SSOT_data.csv"))

SSOT_Practices <- read.csv(here("Data","SSOT_GP_Practice_Locations.csv"))


# Coerce coords to numeric in case they came in as chars/factors
SSOT_Practices$Longitude_1m <- suppressWarnings(as.numeric(SSOT_Practices$Longitude_1m))
SSOT_Practices$Latitude_1m  <- suppressWarnings(as.numeric(SSOT_Practices$Latitude_1m))



pr_sf <- sf::st_as_sf(SSOT_Practices, coords = c("Longitude_1m","Latitude_1m"), crs=4326, remove = FALSE)



# --------------------------------------------------------------------------------------------------------------------
# 3) Build nested colors (PCNs within Sub‑ICBs) using data frames - uses RColorBrewer
# purrr is the tidyverse package for iteration, list-processing, mapping functions over objects - used here for map()
# --------------------------------------------------------------------------------------------------------------------


# --------------------------------------
# 3A) Distinct mapping of PCN to Sub-ICB
# --------------------------------------
pcn_map <- map_data |>
  st_drop_geometry() |>
  distinct(Sub_ICB, PCN_NAME)

# -------------------------------
# 3B) Define per-SubICB palette selections (sequential)
#    Keys must match SUB_ICB_LOCATION_NAME exactly
# -------------------------------
subicb_palette_map <- c(
  "Stoke-on-Trent" = "Greens",
  "North Staffordshire" = "Purples",
  "Cannock Chase" = "Oranges",
  "East Staffordshire" = "RdPu",
  "Stafford and Surrounds" = "Greys",
  "South East Staffs and Seisdon Peninsular" = "Blues"
)

# Fallback if a Sub-ICB isn’t in subicb_palette_map
fallback_palette <- "Set3"  # or choose a neutral sequential like "Greys"

# --------------------------------------------------------------------------------------------------------------------------------------------------------------
# 3C) Helper: generate n colors from a brewer sequential palette. 
#    Handles n > brewer limit using colorRampPalette - it gives a scale of colours that complement the chosen colour ramp, even if you need additional colours.
#    sequential max sizes vary; 9 is common in RColorBrewer
#   Function inputs = pal_name (the name of a sequential colour palette - in this case subicb_palette_map), n = num of colours in final palette
#   max_n - finds the palettes max supported size (brewer.pal.info is a df in RColorBrewer listing all palettes and max number of colors they support)
#   base - brewer.pal returns the num of colours between 3 and palettes max - the min(max_n, max(3_n) makes sure colours don't exceed max if max_n > palette)
#   colorRampPalette(base) (n) - smoothly interpolates to get exactly n colours
#   output is a character vector of hex colours
# --------------------------------------------------------------------------------------------------------------------------------------------------------------

make_seq_palette <- function(pal_name, n) {
  max_n <- max(brewer.pal.info[pal_name, "max"])
  base <- brewer.pal(min(max_n, max(3, n)), pal_name)
  colorRampPalette(base)(n)
}

# -------------------------------------------
# 3D) Build colors per Sub-ICB using the map
# Create a df(pcn_palette_df) where each row of pcn_map gets assigned a colour but
# colours are assigned separately for each SUB_ICB_LOCATION_NAME,
# each ICB can have its own palette,
# palettes can be sequential (RColorBrewer) or fall back to a default,
# palettes are expanded smoothly to match the number of rows in that group,
# colours are reversed (via rev()) so high → low or low → high ordering flips.
#
# group_split divides pcn_map into a list of data frames - one df per sub_icb_Location_Name and each df goes through next steps independendly
# map_df is from purrr and loops over the list of data frames, runs the subsequent code on each df and then binds them back together into a single df
# subicb - the subicb in each individual df is the same so just take the name from the first row
# pal_name - look up the palette for that subicb from the subicb_palette_map - if it doesn't have one use the fallback_palette
# n - takes the number of rows in the data frame i.e. how many pcns are in each subicb
# pal - building the colour palette - if the pal_name is valid use the make_seq_palette otherwise use fallback
# reverse the colour palette so stronger colours are first - so if subicb has 4 pcns it'll take the darkest 4 colours in the sequential colour palette 
# df - return the modified df - map_df() binds all the separate df together
# -------------------------------------------
pcn_palette_df <- pcn_map |>
  group_split(Sub_ICB) |>
  map_df(function(df) {
    subicb <- df$Sub_ICB[1]
    pal_name <- subicb_palette_map[[subicb]]
    if (is.null(pal_name)) pal_name <- fallback_palette
    
    n <- nrow(df)
    # For sequential brewer palettes, prefer Greens/Purples/etc.
    # If fallback is "Set3" (qualitative), still ok; just extends.
    pal <- if (pal_name %in% rownames(brewer.pal.info)) {
      make_seq_palette(pal_name, n)
    } else {
      # In case pal_name is not a valid brewer palette string
      colorRampPalette(brewer.pal(8, fallback_palette))(n)
    }
    
    df$color <- rev(pal)
    df
  })

# --- Helpers for grouped UI (place after pcn_map is created) ---
safe_id <- function(x) gsub("[^A-Za-z0-9]", "_", x)

# Split PCNs by SubICB for building UI controls
subicb_groups <- split(pcn_map$PCN_NAME, pcn_map$Sub_ICB)

# Stable input IDs per group
group_ids <- setNames(safe_id(names(subicb_groups)), names(subicb_groups))








# -------------------------------
# 4) Functions: union polygons + grouped legend
# -------------------------------
# Union selected LSOAs into one polygon per PCN
build_union <- function(pcns, threshold) {
  if (length(pcns) == 0) return(NULL)
  
  # Filter once then group by PCN and union
  sub <- map_data |> 
    filter(PCN_NAME %in% pcns,
           !is.na(Cum_Perc),
           Cum_Perc <= threshold)
  
  if (nrow(sub) == 0) return(NULL)
  
  # Robust union by PCN (keeps sf class and avoids named vectors)
  
  polys <- sub |>
    group_by(PCN_NAME, Sub_ICB) |>
    summarise(geometry = sf::st_union(geometry), .groups = "drop") |>
    sf::st_transform(4326)  # ensure coordinates are truly WGS84 lon/lat
  
  
  if (nrow(polys) == 0) return(NULL)
  polys
}


# Sum patients for the same filter used in build_union()



library(scales)  # for comma formatting

# Sum patients for the same filter used in build_union()
compute_pcn_patients <- function(pcns, threshold, patient_col = "Patients") {
  # Validate column exists
  if (!patient_col %in% names(map_data)) {
    warning(sprintf("Column '%s' not found in map_data; returning NA counts.", patient_col))
    return(
      map_data %>%
        st_drop_geometry() %>%
        filter(PCN_NAME %in% pcns,
               !is.na(Cum_Perc),
               Cum_Perc <= threshold) %>%
        distinct(PCN_NAME, Sub_ICB) %>%
        mutate(patients = NA_integer_)
    )
  }
  
  # Aggregate patient totals by PCN (respecting threshold and current filter)
  map_data %>%
    st_drop_geometry() %>%
    filter(PCN_NAME %in% pcns,
           !is.na(Cum_Perc),
           Cum_Perc <= threshold) %>%
    group_by(PCN_NAME, Sub_ICB) %>%
    summarise(patients = sum(.data[[patient_col]], na.rm = TRUE), .groups = "drop")
}









# Create grouped HTML legend (Sub‑ICB -> list of PCNs with swatches)
make_grouped_legend <- function(pal_df) {
  groups <- split(pal_df, pal_df$Sub_ICB)
  
  container <- tags$div(
    style = "background:white;padding:10px 12px;border-radius:4px;
             box-shadow:0 1px 4px rgba(0,0,0,0.2);max-height:320px;overflow:auto;",
    tags$div(style="font-weight:600;margin-bottom:6px;", "PCNs by Sub‑ICB"),
    lapply(groups, function(df) {
      tags$div(
        style = "margin-bottom:8px;",
        tags$div(style="font-weight:600;margin-bottom:4px;", df$Sub_ICB[1]),
        tags$div(
          lapply(seq_len(nrow(df)), function(i) {
            tags$div(
              style = "display:flex;align-items:center;margin:2px 0;",
              tags$span(
                style = paste0("background:", df$color[i],
                               ";width:12px;height:12px;display:inline-block;",
                               "margin-right:6px;border:1px solid #555;")
              ),
              tags$span(df$PCN_NAME[i])
            )
          })
        )
      )
    })
  )
  
  HTML(as.character(container))
}


# Fit map to the bbox of an sf object


# England WGS84 bbox (lon/lat; generous margins to include coastal edges)
##ENGLAND_BBOX <- list(
##  xmin = -6.0,   # west (Cornwall margin, includes Isles of Scilly)
##  ymin = 49.9,   # south coast
##  xmax = 2.1,    # east (beyond Kent/Norfolk)
##  ymax = 55.9    # north (just above England's northern border)
##)


# Always fit the Leaflet map to England, regardless of layer data

##fit_to_england <- function(map_id = "map", padding = 0.0) {
##  bb <- ENGLAND_BBOX
##  if (padding > 0) {
##    bb$xmin <- bb$xmin - padding
##    bb$ymin <- bb$ymin - padding
##    bb$xmax <- bb$xmax + padding
##    bb$ymax <- bb$ymax + padding
##  }
##  leaflet::leafletProxy(map_id) %>%
##    leaflet::fitBounds(
##      lng1 = bb$xmin, lat1 = bb$ymin,
##      lng2 = bb$xmax, lat2 = bb$ymax
##    )
##  invisible(NULL)
##} 



# --- West Midlands bbox and view helpers (EPSG:4326) ---

WEST_MIDLANDS_BBOX <- list(
  xmin = -3.20,  # west
  ymin = 51.95,  # south
  xmax = -1.30,  # east
  ymax = 53.10   # north
)

# Expand a bbox asymmetrically to create more space top-right (legend side)
expand_bbox_asymmetric <- function(bb, pad_bl = c(0.02, 0.02), pad_tr = c(0.25, 0.28)) {
  w <- bb[["xmax"]] - bb[["xmin"]]
  h <- bb[["ymax"]] - bb[["ymin"]]
  bb[["xmin"]] <- bb[["xmin"]] - w * pad_bl[1]
  bb[["ymin"]] <- bb[["ymin"]] - h * pad_bl[2]
  bb[["xmax"]] <- bb[["xmax"]] + w * pad_tr[1]
  bb[["ymax"]] <- bb[["ymax"]] + h * pad_tr[2]
  bb
}

# Fit to a bbox with asymmetric padding
fit_bounds_asymmetric <- function(map_id = "map", bb,
                                  pad_bl = c(0.02, 0.02),
                                  pad_tr = c(0.25, 0.28)) {
  bb2 <- expand_bbox_asymmetric(bb, pad_bl = pad_bl, pad_tr = pad_tr)
  leaflet::leafletProxy(map_id) %>%
    leaflet::fitBounds(
      lng1 = bb2[["xmin"]], lat1 = bb2[["ymin"]],
      lng2 = bb2[["xmax"]], lat2 = bb2[["ymax"]]
    )
  invisible(NULL)
}

# Fit to West Midlands with asymmetric padding so polygons sit bottom-left
fit_to_west_midlands <- function(map_id = "map",
                                 pad_bl = c(0.02, 0.02),
                                 pad_tr = c(0.25, 0.28)) {
  fit_bounds_asymmetric(map_id, WEST_MIDLANDS_BBOX, pad_bl = pad_bl, pad_tr = pad_tr)
}

# Fit to current layer bounds (snug), else fallback to WM
fit_to_layer_bounds <- function(map_id = "map", sf_layer,
                                pad_bl = c(0.02, 0.02),
                                pad_tr = c(0.25, 0.28)) {
  if (is.null(sf_layer) || nrow(sf_layer) == 0) {
    return(fit_to_west_midlands(map_id, pad_bl = pad_bl, pad_tr = pad_tr))
  }
  bb <- as.list(sf::st_bbox(sf::st_transform(sf_layer, 4326)))
  fit_bounds_asymmetric(map_id, bb, pad_bl = pad_bl, pad_tr = pad_tr)
}






# --- Helpers to improve visibility ---


# Compute luminance (0 = black, 1 = white) for hex colours
luminance_hex <- function(hex) {
  rgb <- grDevices::col2rgb(hex) / 255
  srgb_to_linear <- function(c) ifelse(c <= 0.03928, c/12.92, ((c + 0.055)/1.055)^2.4)
  lin <- srgb_to_linear(rgb)
  as.numeric(0.2126*lin[1,] + 0.7152*lin[2,] + 0.0722*lin[3,])
}

# Style sf polygons by luminance: stronger borders for light fills
style_polys <- function(sf_obj, color_col = "color") {
  stopifnot(color_col %in% names(sf_obj))
  sf_obj |>
    dplyr::mutate(
      lum = luminance_hex(.data[[color_col]]),
      stroke_weight = dplyr::case_when(
        lum >= 0.80 ~ 3.2,
        lum >= 0.70 ~ 2.4,
        TRUE        ~ 1.6
      ),
      stroke_col = dplyr::case_when(
        lum >= 0.80 ~ "#1F1F1F",
        lum >= 0.70 ~ "#2B2B2B",
        TRUE        ~ "#444444"
      )
    )
}

# Optional: add a subtle glow underlay to separate polygons from basemap
add_glow_underlay <- function(map_id, sf_obj) {
  leaflet::leafletProxy(map_id) %>%
    leaflet::addPolygons(
      data = sf_obj,
      fillColor   = "transparent",
      fillOpacity = 0,
      color       = "#000000",
      weight      = 3.5,       # slightly thicker than main stroke
      opacity     = 0.25,
      smoothFactor = 0
    )
}




# Normalise Practice name/code column names, depending on what your CSV uses
if (!"Practice_Name" %in% names(pr_sf) && "PRACTICE_NAME_PROPER" %in% names(pr_sf)) {
  pr_sf <- pr_sf |> rename(Practice_Name = PRACTICE_NAME_PROPER)
}
if (!"Practice_Code" %in% names(pr_sf) && "PRACTICE_CODE" %in% names(pr_sf)) {
  pr_sf <- pr_sf |> rename(Practice_Code = PRACTICE_CODE)
}


# Join the PCN Colour Palette to the GP Practices

pr_coloured <- pr_sf |> 
  left_join(pcn_palette_df |> select(Sub_ICB, PCN_NAME, color), by =
              c("Sub_ICB","PCN_NAME")) |> 
  mutate(
    
    # Fallback colours if a practice's PCN isn't in the palette (e.g., out-of-area)
    color = ifelse(is.na(color), "#9E9E9E", color),  # neutral grey fill
    lum   = luminance_hex(color),
    stroke_col = dplyr::case_when(
      lum >= 0.80 ~ "#1F1F1F",
      lum >= 0.70 ~ "#2B2B2B",
      TRUE        ~ "#444444"
    )
  )





# Builds a clean data.frame for Leaflet markers:
# - drops sf geometry
# - coerces lon/lat to numeric
# - coerces text fields to character
# - filters out any rows with NA/non-finite coords


make_pr_markers_df <- function(df) {
  # 1) Start from a plain data.frame (no sf geometry)
  df2 <- tryCatch(sf::st_drop_geometry(df), error = function(e) df)
  
  # 2) Ensure required columns exist
  required <- c("Longitude_1m","Latitude_1m","Practice_Name","Practice_Code",
                "PCN_NAME","Sub_ICB","color","stroke_col")
  for (nm in required) if (!nm %in% names(df2)) df2[[nm]] <- NA
  
  # 3) Coerce lon/lat to numeric, text to character
  df2$Longitude_1m  <- suppressWarnings(as.numeric(df2$Longitude_1m))
  df2$Latitude_1m   <- suppressWarnings(as.numeric(df2$Latitude_1m))
  df2$Practice_Name <- as.character(df2$Practice_Name)
  df2$Practice_Code <- as.character(df2$Practice_Code)
  df2$PCN_NAME      <- as.character(df2$PCN_NAME)
  df2$Sub_ICB       <- as.character(df2$Sub_ICB)
  
  # 4) Colour fields: force to character, then fill blanks/NA
  df2$color      <- as.character(df2$color)
  df2$stroke_col <- as.character(df2$stroke_col)
  df2$color[is.na(df2$color) | df2$color == ""]             <- "#9E9E9E"
  df2$stroke_col[is.na(df2$stroke_col) | df2$stroke_col==""]<- "#444444"
  
  # 5) If lon/lat somehow are still list or character, flatten & re-coerce
  if (is.list(df2$Longitude_1m)) df2$Longitude_1m <- suppressWarnings(as.numeric(unlist(df2$Longitude_1m)))
  if (is.list(df2$Latitude_1m))  df2$Latitude_1m  <- suppressWarnings(as.numeric(unlist(df2$Latitude_1m)))
  if (is.character(df2$Longitude_1m)) df2$Longitude_1m <- suppressWarnings(as.numeric(df2$Longitude_1m))
  if (is.character(df2$Latitude_1m))  df2$Latitude_1m  <- suppressWarnings(as.numeric(df2$Latitude_1m))
  
  # 6) Drop rows with bad coords
  df2 <- df2[is.finite(df2$Longitude_1m) & is.finite(df2$Latitude_1m), , drop = FALSE]
  
  # 7) Flatten any remaining list columns (defensive)
  is_list <- vapply(df2, is.list, logical(1))
  if (any(is_list)) {
    list_cols <- names(df2)[is_list]
    df2[list_cols] <- lapply(df2[list_cols], function(x) {
      tryCatch(unlist(x, use.names = FALSE), error = function(e) as.character(x))
    })
  }
  
  df2
}


  


# -------------------------------
# 7) UI
# -------------------------------


# --- 7) UI (updated) ---

ui <- fluidPage(
  theme = bslib::bs_theme(version = 5), # Bootstrap 5
  
  titlePanel(
    div(
      style = "display:flex; align-items:center; gap:40px;",
      # Bigger, crisp logo; use SVG if available
      tags$img(
        src = "logo.png", alt = "NHS Midlands & Lancashire CSU",
        style = "height:48px; width:auto; display:block;"
      ),
      tags$span(
        "SSOT PCN Boundaries by GP Patient Registration Coverage - Oct 2025",
        style = "font-weight:600;"
      )
    )
  ),
  
  
  # DEBUG BANNER — place right after titlePanel
  #tags$div(
  #  style = "background:#fff3cd;color:#664d03;border:1px solid #ffecb5;padding:6px;margin-bottom:8px;",
  #  paste("Working dir:", getwd()),
  #  tags$br(),
  #  paste("www/ contents:", paste(list.files("www"), collapse = ", ")),
  #  tags$br(),
  #  paste("logo.png exists:", file.exists("www/logo.png"))
  #),
  
  
  # Add Information box
  
  actionLink("info_btn", label = NULL, icon = icon("circle-info"),
             class = "btn btn-link", 
             style = "position:absolute; right:18px; top:8px; font-size:18px;"),
  
  
  # Add CSS: scrollable content + sticky footer button
  tags$style(HTML("
    /* Reduce base font in the whole sidebar */
    .sidebarPanel, .sidebarPanel .checkbox label, .sidebarPanel .control-label {
      font-size: 8px;        /* try 13px if you want smaller */
      line-height: 1.4;
    }
    /* bsCollapse panel headers (Bootstrap 3 style classes still present sometimes) */
    .panel-group .panel-heading {
      padding: 6px 10px;
    }
    .panel-title, .panel-title a {
      font-size: 12px;
      font-weight: 600;
      color: #2b2b2b;
      text-decoration: none;
    }
    .panel-body {
      padding: 8px 10px;
    }

    /* Scrollable area inside sidebar */
    .sidebar-scroll {
      max-height: 75vh;       /* adjust to taste or match your map height */
      overflow-y: auto;
      padding-right: 8px;     /* avoids scrollbar overlapping text */
    }

    /* Sticky footer inside sidebar (keeps button visible) */
    .sidebar-sticky-footer {
      position: sticky;
      bottom: 0;
      background: #fff;
      padding: 8px 0;
      box-shadow: 0 -4px 8px rgba(0,0,0,0.05); /* subtle separation */
    }
  ")),
  
  sidebarLayout(
    sidebarPanel(
      # Scrollable content (everything except the update button)
      div(class = "sidebar-scroll",
          # Global on/off
          checkboxInput("all_toggle", "Select all PCNs", TRUE),
          
          # Grouped PCN selectors built dynamically per SubICB
          uiOutput("grouped_pcn_ui"),
          
          tags$hr(),
          sliderInput("threshold", "Proportion of Patients (%):",
                      min = 50, max = 98, value = 98, step = 1),
          # checkboxInput("show_debug", "Show diagnostics in console", value = FALSE),
          
          tags$hr(),
          # Display note requested
          helpText(
            "Note: PCN catchments can be selected to show where at least 50% and up to 98% of their patients reside, based on data at Oct 2025.",
            "In the legend, ABC PCN and Rugeley & Great Haywood PCN are assigned to the Sub-ICB with the majority of their practices, although their catchments are defined on their total patient list."
          )
      ),
      # Sticky footer with the Update Map button (always visible)
      div(class = "sidebar-sticky-footer",
          actionButton("update", "Update Map", class = "btn btn-primary", width = "100%")
      )
    ),
    mainPanel(
      leafletOutput("map", height = 1000)
    )
  )
)


# -------------------------------
# 8) Server (cleaned)
# -------------------------------
server <- function(input, output, session) {
  
  # 8a) Base map
  
  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(worldCopyJump = FALSE)) %>%
      addProviderTiles("CartoDB.Positron", options = providerTileOptions(opacity = 0.6)) %>%
      setView(lng = -2.18, lat = 53.00, zoom = 6)  # Birmingham centre-ish
    # Optionally restrict panning to England:
    # setMaxBounds(lng1 = -6.0, lat1 = 49.9, lng2 = 2.1, lat2 = 55.9)
    
    # Optional: restrict panning to West Midlands bounds
    # setMaxBounds(lng1 = WEST_MIDLANDS_BBOX$xmin, lat1 = WEST_MIDLANDS_BBOX$ymin,
    #              lng2 = WEST_MIDLANDS_BBOX$xmax, lat2 = WEST_MIDLANDS_BBOX$ymax)
    
    
  })
  
  
  # 8b) Build grouped PCN UI (one bsCollapsePanel per Sub-ICB)
  output$grouped_pcn_ui <- renderUI({
    panels <- lapply(names(subicb_groups), function(g) {
      gid <- group_ids[[g]]
      shinyBS::bsCollapsePanel(
        title = g,
        checkboxGroupInput(
          paste0("pcn_", gid), label = NULL,
          choices  = unname(sort(subicb_groups[[g]])),
          selected = unname(sort(subicb_groups[[g]]))
        ),
        value = paste0("acc_", gid)
      )
    })
    do.call(
      shinyBS::bsCollapse,
      c(panels, list(id = "subicb_acc", multiple = TRUE, open = NULL))
    )
  })
  
  # 8c) Reactively gather all selected PCNs across groups
  selected_pcns <- reactive({
    unlist(lapply(names(subicb_groups), function(g) {
      gid <- group_ids[[g]]
      input[[paste0("pcn_", gid)]]
    }), use.names = FALSE)
  })
  
  # 8d) Per-SubICB toggle logic (if you have toggle buttons per group)
  observe({
    lapply(names(subicb_groups), function(g) {
      gid <- group_ids[[g]]
      observeEvent(input[[paste0("toggle_", gid)]], {
        sel <- if (isTRUE(input[[paste0("toggle_", gid)]])) subicb_groups[[g]] else character(0)
        updateCheckboxGroupInput(session, paste0("pcn_", gid), selected = sort(sel))
      }, ignoreInit = TRUE)
    })
  })
  
  # 8e) Global select-all toggle: affects every group
  observeEvent(input$all_toggle, {
    lapply(names(subicb_groups), function(g) {
      gid <- group_ids[[g]]
      sel <- if (isTRUE(input$all_toggle)) subicb_groups[[g]] else character(0)
      updateCheckboxGroupInput(session, paste0("pcn_", gid), selected = sort(sel))
      updateCheckboxInput(session, paste0("toggle_", gid), value = isTRUE(input$all_toggle))
    })
  }, ignoreInit = TRUE)
  
  
  # 8e1) Information Box
  
  observeEvent(input$info_btn, {
    showModal(modalDialog(
      title = "About this app",
      easyClose = TRUE,
      footer = modalButton("Close"),
      size = "l",
      tagList(
        tags$p("This application allows users to visualise PCN boundaries based on the proportion of patients registered at October 2025. For each Lower Layer Super Output Area (LSOA), the total number of patients registered at a PCN are summed.  The LSOAs are then sorted in descending order of patients, so LSOAs with the highest density of patients are displayed first. Cumulative patient counts are calculated as well as the percentage of the total PCN population those represent."),  
        tags$p("The cumulative percentage is used in the threshold slider.  Users can select the proportion of patients they wish to view, and the underlying data at LSOA is merged to produce a single boundary per PCN. The number of patients within the boundary is displayed in the map labels."),
        tags$ul(
          tags$li("Coverage threshold: adjustable to show where at least 50% and up to 98% of patients reside."),
          tags$li("Palettes: per Sub‑ICB, harmonised via RColorBrewer."),
          tags$li("Data source: ",
                  tags$a(
                    href = "https://digital.nhs.uk/data-and-information/publications/statistical/patients-registered-at-a-gp-practice",
                    target = "_blank",
                    "NHS England GP Registered Patients"
                  ),
                  "(Oct 2025)."
          ))
      ),
      tags$hr(),
      tags$p(
        strong("Contact: "), "NHS Midlands & Lancashire CSU — BI Analytics Hub.",
        tags$br(),
        "Email: ", tags$a(href="mailto:analytics.mlcsu@nhs.net", "analytics.mlcsu@nhs.uk")
      )
    )
    )
  })
  
  
  
  
  # 8f) Initial render: all PCNs at 100% + grouped legend
  
  
  observeEvent(TRUE, {
    pcns0  <- sort(unique(map_data$PCN_NAME))
    polys0 <- build_union(pcns0, 98)
    
    leafletProxy("map") %>%
      clearControls() %>%
      addControl(make_grouped_legend(pcn_palette_df), position = "topright")
    
    if (!is.null(polys0)) {
      polys0 <- polys0 %>%
        dplyr::left_join(pcn_palette_df, by = c("Sub_ICB", "PCN_NAME")) %>%
        style_polys(color_col = "color")
      
      
      
      # Compute patient totals for the same filter
      totals0 <- compute_pcn_patients(pcns0, threshold = 98, patient_col = "PCN_Pat")  # <-- change name if needed
      
      # Join totals onto polygons
      polys0 <- polys0 %>%
        left_join(totals0, by = c("Sub_ICB", "PCN_NAME")) %>%
        style_polys(color_col = "color")  # adaptive stroke from earlier
      
      
      
      
      # Optional glow underlay:
      # add_glow_underlay("map", polys0)
      
      leafletProxy("map") %>%
        addPolygons(
          data        = polys0,
          group       = "PCN Polygons",
          fillColor   = ~color,          # use palette for fill
          fillOpacity = 0.20,            # make fill visible
          color       = ~stroke_col,     # adaptive border
          weight      = ~stroke_weight,  # thicker for lighter fills
          opacity     = 1,
          # Hover label with patient count
          label = ~sprintf(
            "%s\nPatients: %s",
            PCN_NAME, ifelse(is.na(patients), "n/a", scales::comma(patients))
          ),
          labelOptions = labelOptions(
            noHide = FALSE,
            direction = "auto",
            style = list(
              "font-weight" = "600",
              "color" = "#1f1f1f",
              "background" = "rgba(255,255,255,0.9)",
              "border" = "1px solid #ccc",
              "padding" = "4px 6px"
            )
          ),
          popup = ~paste0(
            "<b>PCN:</b> ", PCN_NAME,
            "<br><b>Sub‑ICB:</b> ", Sub_ICB,
            ifelse(is.na(patients), "", paste0("<br><b>Patients:</b> ", scales::comma(patients)))
          ),
          highlightOptions = highlightOptions(weight = 4, color = "#000000", opacity = 1, bringToFront = TRUE)
        )
      
      
      
      # Practices for the initial view (all PCNs)
      pr0 <- pr_coloured |> 
        filter(PCN_NAME %in% pcns0)
      
      pr0_markers <- make_pr_markers_df(pr0)
        
  
      
      
      # ===== DEBUG (markers) =====
      expected_cols <- c("Longitude_1m","Latitude_1m","Practice_Name",
                         "Practice_Code","PCN_NAME","Sub_ICB","color","stroke_col")
      
      cat("\n--- DEBUG (markers) ---\n")
      cat("nrow =", nrow(pr0_markers), "\n")
      cat("class(lon/lat) =", class(pr0_markers$Longitude_1m), "/", class(pr0_markers$Latitude_1m), "\n")
      cat("is.list(lon/lat) =", is.list(pr0_markers$Longitude_1m), "/", is.list(pr0_markers$Latitude_1m), "\n")
      cat("anyNA(lon/lat)  =", anyNA(pr0_markers$Longitude_1m), "/", anyNA(pr0_markers$Latitude_1m), "\n")
      cat("names(pr0_markers):\n"); print(names(pr0_markers))
      
      # Glimpse, but only selecting columns that actually exist (avoids subsetting errors)
      suppressWarnings({
        print(dplyr::glimpse(dplyr::select(pr0_markers, dplyr::any_of(expected_cols)), width = 120))
      })
      # ===========================
      ``
      
      
      
      
      
          
      leafletProxy("map") %>%
        addCircleMarkers(
          data        = pr0_markers,
          group       = "GP Practices",     # <--- markers group
          lng         = ~Longitude_1m,
          lat         = ~Latitude_1m,
          radius      = 6,
          fillColor   = ~color,
          fillOpacity = 0.95,
          color       = ~stroke_col,
          weight      = 1,
          opacity     = 1,
          label       = ~sprintf(
            "%s\n(%s)",
            ifelse(!is.na(Practice_Name), Practice_Name, Practice_Code),
            ifelse(!is.na(PCN_NAME), PCN_NAME, "PCN Unknown")
          ),
          labelOptions = labelOptions(
            noHide = FALSE, direction = "auto",
            style   = list("font-weight" = "600", "color" = "#1f1f1f",
                           "background" = "rgba(255,255,255,0.95)",
                           "border"     = "1px solid #ccc", "padding" = "3px 5px")
          ),
          
          popup = ~paste0(
            "<b>Practice:</b> ", htmltools::htmlEscape(ifelse(!is.na(Practice_Name), Practice_Name, Practice_Code)),
            ifelse(!is.na(Practice_Code), paste0("<br><b>ODS:</b> ", Practice_Code), ""),
            ifelse(!is.na(PCN_NAME),      paste0("<br><b>PCN:</b> ", htmltools::htmlEscape(PCN_NAME)), ""),
            ifelse(!is.na(Sub_ICB),       paste0("<br><b>Sub‑ICB:</b> ", htmltools::htmlEscape(Sub_ICB)), "")
          ),
          
          clusterOptions = markerClusterOptions(disableClusteringAtZoom = 13)  # optional clustering
        ) %>%
        addLayersControl(
          overlayGroups = c("PCN Polygons","GP Practices"),
          options = layersControlOptions(collapsed = FALSE)
        )
      
      
      
      fit_to_layer_bounds("map", polys0)
      
    } else {
      fit_to_west_midlands("map")
    }
  }, once = TRUE)
  
  
  
  # 8g) Update map on button click
  observeEvent(input$update, {
    req(length(selected_pcns()) > 0)
    
    # Build union for selected PCNs at chosen threshold
    polys <- build_union(selected_pcns(), input$threshold)
    
    leafletProxy("map") %>%
     # clearShapes() %>%
      clearGroup("PCN Polygons") %>% 
      clearGroup("GP Practices") %>% 
      clearControls() %>%
      addControl(
        make_grouped_legend(pcn_palette_df %>% dplyr::filter(PCN_NAME %in% selected_pcns())),
        position = "topright"
      )
    
    
    if (!is.null(polys)) {
      polys <- polys %>%
        dplyr::left_join(pcn_palette_df, by = c("Sub_ICB", "PCN_NAME")) %>%
        style_polys(color_col = "color")
      
      totals <- compute_pcn_patients(selected_pcns(), threshold = input$threshold, patient_col = "PCN_Pat")   
      
      
      polys <- polys %>%
        left_join(totals, by = c("Sub_ICB", "PCN_NAME")) %>%
        style_polys(color_col = "color")
      
      
      # Optional glow:
      # add_glow_underlay("map", polys)
      
      leafletProxy("map") %>%
        addPolygons(
          data        = polys,
          group       = "PCN Polygons",
          fillColor   = ~color,
          fillOpacity = 0.20,
          color       = ~stroke_col,
          weight      = ~stroke_weight,
          opacity     = 1,
          label = ~sprintf(
            "%s\nPatients: %s",
            PCN_NAME, ifelse(is.na(patients), "n/a", scales::comma(patients))
          ),
          labelOptions = labelOptions(
            noHide = FALSE,
            direction = "auto",
            style = list(
              "font-weight" = "600",
              "color" = "#1f1f1f",
              "background" = "rgba(255,255,255,0.9)",
              "border" = "1px solid #ccc",
              "padding" = "4px 6px"
            )
          ),
          popup = ~paste0(
            "<b>PCN:</b> ", PCN_NAME,
            "<br><b>Sub‑ICB:</b> ", Sub_ICB,
            
            ifelse(is.na(patients), "", paste0("<br><b>Patients:</b> ", scales::comma(patients)))
          ),
          highlightOptions = highlightOptions(weight = 4, color = "#000000", opacity = 1, bringToFront = TRUE)
        )
      
      
  
      # Practices for selected PCNs -> build a clean markers df in one go
      pr_sel_markers <- make_pr_markers_df(
        pr_coloured %>% dplyr::filter(PCN_NAME %in% selected_pcns())
      )
      
      
      # ==== DEBUG (update) ====
      cat("\n--- DEBUG (update markers) ---\n")
      cat("nrow(pr_sel_markers) =", nrow(pr_sel_markers), "\n")
      cat("class(lon/lat) =", class(pr_sel_markers$Longitude_1m), "/", class(pr_sel_markers$Latitude_1m), "\n")
      cat("is.list(lon/lat) =", is.list(pr_sel_markers$Longitude_1m), "/", is.list(pr_sel_markers$Latitude_1m), "\n")
      cat("anyNA(lon/lat)  =", anyNA(pr_sel_markers$Longitude_1m), "/", anyNA(pr_sel_markers$Latitude_1m), "\n")
      print(dplyr::glimpse(pr_sel_markers[, c("Longitude_1m","Latitude_1m","Practice_Name",
                                              "Practice_Code","PCN_NAME","Sub_ICB","color","stroke_col")],
                           width = 120))
      # ========================
      ``
      
      # ===== DEBUG (markers) =====
      expected_cols <- c("Longitude_1m","Latitude_1m","Practice_Name",
                         "Practice_Code","PCN_NAME","Sub_ICB","color","stroke_col")
      
      cat("\n--- DEBUG (markers) ---\n")
      cat("nrow =", nrow(pr_sel_markers), "\n")
      cat("class(lon/lat) =", class(pr_sel_markers$Longitude_1m), "/", class(pr_sel_markers$Latitude_1m), "\n")
      cat("is.list(lon/lat) =", is.list(pr_sel_markers$Longitude_1m), "/", is.list(pr_sel_markers$Latitude_1m), "\n")
      cat("anyNA(lon/lat)  =", anyNA(pr_sel_markers$Longitude_1m), "/", anyNA(pr_sel_markers$Latitude_1m), "\n")
      cat("names(pr_sel_markers):\n"); print(names(pr_sel_markers))
      
      # Glimpse, but only selecting columns that actually exist (avoids subsetting errors)
      suppressWarnings({
        print(dplyr::glimpse(dplyr::select(pr_sel_markers, dplyr::any_of(expected_cols)), width = 120))
      })
      # ===========================
      ``
      
      
      leafletProxy("map") %>%
        addCircleMarkers(
          data        = pr_sel_markers,
          group       = "GP Practices",
          lng         = ~Longitude_1m,
          lat         = ~Latitude_1m,
          radius      = 6,
          fillColor   = ~color,       # from your PCN palette join
          fillOpacity = 0.95,
          color       = ~stroke_col,  # outline based on luminance
          weight      = 1,
          opacity     = 1,
          # Use PRACTICE_NAME_PROPER for the label (fallback to ODS if name is missing)
          label       = ~sprintf(
            "%s\n(%s)",
            ifelse(!is.na(Practice_Name), Practice_Name, Practice_Code),
            ifelse(!is.na(PCN_NAME), PCN_NAME, "PCN unknown")
          ),
          labelOptions = labelOptions(
            noHide = FALSE, direction = "auto",
            style   = list("font-weight" = "600", "color" = "#1f1f1f",
                           "background" = "rgba(255,255,255,0.95)",
                           "border"     = "1px solid #ccc", "padding" = "3px 5px")
          ),
          popup = ~paste0(
            "<b>Practice:</b> ", htmltools::htmlEscape(ifelse(!is.na(Practice_Name), Practice_Name, Practice_Code)),
            ifelse(!is.na(Practice_Code), paste0("<br><b>ODS:</b> ", Practice_Code), ""),
            ifelse(!is.na(PCN_NAME),      paste0("<br><b>PCN:</b> ", htmltools::htmlEscape(PCN_NAME)), ""),
            ifelse(!is.na(Sub_ICB),       paste0("<br><b>Sub‑ICB:</b> ", htmltools::htmlEscape(Sub_ICB)), "")
          ),
          clusterOptions = markerClusterOptions(disableClusteringAtZoom = 13)
        ) %>%
        addLayersControl(
          overlayGroups = c("PCN Polygons","GP Practices"),
          options = layersControlOptions(collapsed = FALSE)
        )
      
      
      
      
      
       
      fit_to_layer_bounds("map", polys)
      
    } else {
      showNotification("No polygons for the selected PCNs/threshold. Try 100% or check data.", type = "message")
      fit_to_west_midlands("map")
    }
  })
}

# This section gets added to the final app.R file which is a copy of this script, saved lose in the A0195_SSOT_PCN folder.  Running the launch from here in the Scripts folder means the ML logo doesn't load
# -------------------------------
# 6) Launch
# -------------------------------
options(shiny.launch.browser = TRUE)
shinyApp(ui, server)




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
lsoa_shapes <- st_read(here("Data","LSOA_(Dec_2021)_Boundaries_Full_Clipped_EW_(BFC).shp")) 

#check geometry exists - should return FALSE

any(is.na(sf::st_geometry(lsoa_shapes)))

# check CRS

sf::st_crs(lsoa_shapes)


# Then transform to WGS84 for Leaflet
lsoa_shapes <- st_transform(lsoa_shapes, 4326)

# Check CRS is now 4326
st_crs(lsoa_shapes)

#check geometry exists now transformed to WGS84
any(is.na(sf::st_geometry(lsoa_shapes)))


# -----------------------------------------------------------
# 2) Load SSOT patient data and recode SUB_ICB_LOCATION_NAME
# -----------------------------------------------------------

SSOT_data <- read.csv(here("Outputs","SSOT_data.csv"))

# 2A) Normalize Sub-ICB names With a named vector and dplyr::recode
subicb_rename <- c(
  "NHS Staffordshire and Stoke-on-Trent ICB - 05W" = "Stoke-on-Trent",
  "NHS Staffordshire and Stoke-on-Trent ICB - 05G" = "North Staffordshire",
  "NHS Staffordshire and Stoke-on-Trent ICB - 04Y" = "Cannock Chase",
  "NHS Staffordshire and Stoke-on-Trent ICB - 05D" = "East Staffordshire",
  "NHS Staffordshire and Stoke-on-Trent ICB - 05Q" = "South East Staffs and Seisdon Peninsular",
  "NHS Staffordshire and Stoke-on-Trent ICB - 05V" = "Stafford and Surrounds"
)

# 2B) Replace the original Sub_ICB name with the vector from (2A). !!! expands or splices values from the named vector sub_icb_rename so you only have to change them once  

SSOT_data <- SSOT_data  |> 
  mutate(
    SUB_ICB_LOCATION_NAME =
      recode(SUB_ICB_LOCATION_NAME, !!!subicb_rename, .default = SUB_ICB_LOCATION_NAME)
  )


# 2C) Title Case manipulation

title_case_preserve <- function(x,
                                acronyms = c("NHS", "ICB", "PCN", "GP", "GPs", "CQC", "GPP", "PVC","ABC","HIPC"),
                                keep_small_words_lower = c("and","or","of","the","in","on","at","for","to","by","with")) {
  # 1) Normalize spacing
  x <- str_squish(x)
  
  # 2) Lowercase everything first, then title case words
  #    Use a regex-friendly approach to handle hyphens/apostrophes
  tc <- str_to_title(str_to_lower(x))
  
  # 3) Keep common small words in lowercase (except at start)
  #    Split and recombine to control first-word capitalization
  tc <- str_replace_all(tc, "\\s+", " ")
  tc <- vapply(str_split(tc, " "), function(words) {
    if (length(words) == 0) return("")
    # First word stays as-is (title cased)
    words[1] <- words[1]
    if (length(words) > 1) {
      words[-1] <- ifelse(
        str_to_lower(words[-1]) %in% keep_small_words_lower,
        str_to_lower(words[-1]),
        words[-1]
      )
    }
    paste(words, collapse = " ")
  }, character(1))
  
  # 4) Re-capitalize specified acronyms anywhere they occur
  #    Match whole words, including around punctuation (start/end/boundaries)
  for (ac in acronyms) {
    # Build a boundary-safe pattern (handles parentheses, commas, hyphens)
    pattern <- paste0("(?i)(?<=^|[^A-Za-z])(", ac, ")(?=[^A-Za-z]|$)")
    tc <- str_replace_all(tc, pattern, toupper(ac))
  }
  
  # 5) Ensure trailing "PCN" stays uppercase when it appears at the end,
  #    optionally with punctuation like ")"
  #    Examples: "… pcn", "… pcn)", "… pcn -"
  tc <- str_replace(tc, "(?i)\\bpcn\\b\\)?\\s*$", function(m) toupper(m))
  
  # 6) Fix title case around hyphens/apostrophes (e.g., "O'Connor", "Well-being")
  #    Apply a light touch—this preserves previously enforced acronyms
  tc <- str_replace_all(tc, "(?<=\\b\\w)'’", ~ str_to_upper(.x))
  tc <- str_replace_all(tc, "(?<=\\b\\w)-(\\w)", ~ str_to_title(.x))
  
  return(tc)
}






# 2D) Change PCN_NAME from CAPS to Title Case

SSOT_data <- SSOT_data |> 
  mutate(PCN_NAME = title_case_preserve(PCN_NAME))
                         
                         



# -----------------------------------------------------------------------------------------------------------------------------------------
# 3) Join SSOT patient data to the LSOA boundaries and filter LSOAs up to 98% of patients 
# st_make_valid() function from sf package that repairs broken spatial geometries e.g. slivers, polygons with holes touching a boundary etc
# -----------------------------------------------------------------------------------------------------------------------------------------

map_data <- lsoa_shapes  |> 
  left_join(SSOT_data, by = c("LSOA21CD" = "LSOA_CODE"))  |>    
  filter(!is.na(PCN_NAME))  |>                                  # keep only rows mapped to a PCN
  filter(substr(LSOA21CD, 1, 1) == "E") |>
  filter(CumulativePercent < 98) |> 
  st_make_valid()



# -----------------------------------------------------------------------------------------------------
# 4) A basic check if you have LSOAs in one file which have not joined to the LSOA spatial file
# this not needed as I filtered for LSOAs beginning with E only
# -----------------------------------------------------------------------------------------------------    


# Drop geometry from the LSOA layer so we can do a simple attribute join
##lsoa_attrs <- lsoa_shapes |> 
##  st_drop_geometry() |> 
##  select(LSOA21CD, everything())   # <-- replace with your actual LSOA code column

# Safe join that preserves all SSOT rows
##SSOT_join <- SSOT_data |> 
##  left_join(lsoa_attrs, by = c("LSOA_CODE" = "LSOA21CD"))  # <-- change key name if needed

##nrow(SSOT_join)  

# Find which SSOT rows did NOT match any LSOA
##ssot_unmatched <- anti_join(SSOT_data, lsoa_attrs, by = c("LSOA_CODE" = "LSOA21CD"))
##nrow(ssot_unmatched)  

# Inspect a few and tally by PCN/ICB if useful
##head(ssot_unmatched, 10)
##ssot_unmatched |> count(PCN_NAME, sort = TRUE)


# --------------------------------------------------------------------------------------------------------------------
# 5) Build nested colors (PCNs within Sub‑ICBs) using data frames - uses RColorBrewer
# purrr is the tidyverse package for iteration, list-processing, mapping functions over objects - used here for map()
# --------------------------------------------------------------------------------------------------------------------


# --------------------------------------
# 5A) Distinct mapping of PCN to Sub-ICB
# --------------------------------------
pcn_map <- map_data |>
  st_drop_geometry() |>
  distinct(SUB_ICB_LOCATION_NAME, PCN_NAME)

# -------------------------------
# 5B) Define per-SubICB palette selections (sequential)
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
# 5C) Helper: generate n colors from a brewer sequential palette. 
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
# 5D) Build colors per Sub-ICB using the map
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
  group_split(SUB_ICB_LOCATION_NAME) |>
  map_df(function(df) {
    subicb <- df$SUB_ICB_LOCATION_NAME[1]
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
subicb_groups <- split(pcn_map$PCN_NAME, pcn_map$SUB_ICB_LOCATION_NAME)

# Stable input IDs per group
group_ids <- setNames(safe_id(names(subicb_groups)), names(subicb_groups))



# -------------------------------
# 6) Functions: union polygons + grouped legend
# -------------------------------
# Union selected LSOAs into one polygon per PCN
build_union <- function(pcns, threshold) {
  if (length(pcns) == 0) return(NULL)
  
  # Filter once then group by PCN and union
  sub <- map_data |> 
    filter(PCN_NAME %in% pcns,
           !is.na(CumulativePercent),
           CumulativePercent <= threshold)
  
  if (nrow(sub) == 0) return(NULL)
  
  # Robust union by PCN (keeps sf class and avoids named vectors)
  
  polys <- sub |>
    group_by(PCN_NAME, SUB_ICB_LOCATION_NAME) |>
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
               !is.na(CumulativePercent),
               CumulativePercent <= threshold) %>%
        distinct(PCN_NAME, SUB_ICB_LOCATION_NAME) %>%
        mutate(patients = NA_integer_)
    )
  }
  
  # Aggregate patient totals by PCN (respecting threshold and current filter)
  map_data %>%
    st_drop_geometry() %>%
    filter(PCN_NAME %in% pcns,
           !is.na(CumulativePercent),
           CumulativePercent <= threshold) %>%
    group_by(PCN_NAME, SUB_ICB_LOCATION_NAME) %>%
    summarise(patients = sum(.data[[patient_col]], na.rm = TRUE), .groups = "drop")
}


  






# Create grouped HTML legend (Sub‑ICB -> list of PCNs with swatches)
make_grouped_legend <- function(pal_df) {
  groups <- split(pal_df, pal_df$SUB_ICB_LOCATION_NAME)
  
  container <- tags$div(
    style = "background:white;padding:10px 12px;border-radius:4px;
             box-shadow:0 1px 4px rgba(0,0,0,0.2);max-height:320px;overflow:auto;",
    tags$div(style="font-weight:600;margin-bottom:6px;", "PCNs by Sub‑ICB"),
    lapply(groups, function(df) {
      tags$div(
        style = "margin-bottom:8px;",
        tags$div(style="font-weight:600;margin-bottom:4px;", df$SUB_ICB_LOCATION_NAME[1]),
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
        dplyr::left_join(pcn_palette_df, by = c("SUB_ICB_LOCATION_NAME", "PCN_NAME")) %>%
        style_polys(color_col = "color")
  
      
      
    # Compute patient totals for the same filter
    totals0 <- compute_pcn_patients(pcns0, threshold = 98, patient_col = "PCN_Patients")  # <-- change name if needed
      
    # Join totals onto polygons
    polys0 <- polys0 %>%
    left_join(totals0, by = c("SUB_ICB_LOCATION_NAME", "PCN_NAME")) %>%
    style_polys(color_col = "color")  # adaptive stroke from earlier
      
      
      
          
      # Optional glow underlay:
      # add_glow_underlay("map", polys0)
      
      leafletProxy("map") %>%
        addPolygons(
          data        = polys0,
          fillColor   = ~color,          # use palette for fill
          fillOpacity = 0.90,            # make fill visible
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
            "<br><b>Sub‑ICB:</b> ", SUB_ICB_LOCATION_NAME,
            ifelse(is.na(patients), "", paste0("<br><b>Patients:</b> ", scales::comma(patients)))
          ),
          highlightOptions = highlightOptions(weight = 4, color = "#000000", opacity = 1, bringToFront = TRUE)
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
      clearShapes() %>%
      clearControls() %>%
      addControl(
        make_grouped_legend(pcn_palette_df %>% dplyr::filter(PCN_NAME %in% selected_pcns())),
        position = "topright"
      )
    
    
    if (!is.null(polys)) {
      polys <- polys %>%
        dplyr::left_join(pcn_palette_df, by = c("SUB_ICB_LOCATION_NAME", "PCN_NAME")) %>%
        style_polys(color_col = "color")
      
      totals <- compute_pcn_patients(selected_pcns(), threshold = input$threshold, patient_col = "PCN_Patients")   
      
      
      polys <- polys %>%
        left_join(totals, by = c("SUB_ICB_LOCATION_NAME", "PCN_NAME")) %>%
        style_polys(color_col = "color")
      
      
      # Optional glow:
      # add_glow_underlay("map", polys)
      
      leafletProxy("map") %>%
        addPolygons(
          data        = polys,
          fillColor   = ~color,
          fillOpacity = 0.50,
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
            "<br><b>Sub‑ICB:</b> ", SUB_ICB_LOCATION_NAME,
            
            ifelse(is.na(patients), "", paste0("<br><b>Patients:</b> ", scales::comma(patients)))
          ),
          highlightOptions = highlightOptions(weight = 4, color = "#000000", opacity = 1, bringToFront = TRUE)
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
#options(shiny.launch.browser = TRUE)
#shinyApp(ui, server)




### Drone Spotcheck Image Processing - Shiny App
### Interactive tool for processing and analyzing drone imagery

#######################################
## AUTO-INSTALL REQUIRED PACKAGES
## This section checks and installs any missing packages
#######################################

# List of required packages
required_packages <- c(
  "shiny", "shinydashboard", "tidyverse", "terra", "tidyterra",
  "magick", "lubridate", "exifr", "leaflet", "DT",
  "shinyjs", "shinyWidgets", "plotly", "base64enc"
)

# Function to check and install packages
install_if_missing <- function(packages) {
  new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
  
  if(length(new_packages) > 0) {
    cat("📦 Installing missing packages:", paste(new_packages, collapse=", "), "\n")
    cat("This may take a few minutes...\n\n")
    
    install.packages(new_packages, 
                    repos = "https://cloud.r-project.org/",
                    dependencies = TRUE,
                    quiet = FALSE)
    
    cat("\n✅ Package installation complete!\n\n")
  } else {
    cat("✅ All required packages are already installed!\n\n")
  }
}

# Run the installation check
cat("🔍 Checking for required packages...\n")
install_if_missing(required_packages)

# Load libraries
library(shiny)
library(shinydashboard)
library(tidyverse)
library(terra)
library(tidyterra)
library(magick)
library(lubridate)
library(exifr)
library(leaflet)
library(DT)
library(shinyjs)
library(shinyWidgets)
library(plotly)

# UI Definition
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "Drone Spotcheck Processor"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("📁 Setup & Process", tabName = "process", icon = icon("cogs")),
      menuItem("🗺️ Map View", tabName = "map", icon = icon("map")),
      menuItem("📊 Data Explorer", tabName = "data", icon = icon("table")),
      menuItem("🖼️ Image Gallery", tabName = "gallery", icon = icon("images")),
      menuItem("📈 Statistics", tabName = "stats", icon = icon("chart-bar"))
    )
  ),
  
  dashboardBody(
    useShinyjs(),
    tags$head(
      tags$style(HTML("
        .content-wrapper { background-color: #ecf0f5; }
        .box { border-radius: 8px; box-shadow: 0 2px 4px rgba(0,0,0,0.1); }
        .progress-bar { background-color: #3c8dbc; }
        .info-box { border-radius: 8px; cursor: pointer; transition: transform 0.2s; }
        .info-box:hover { transform: translateY(-2px); box-shadow: 0 4px 8px rgba(0,0,0,0.2); }
        .image-comparison { display: flex; justify-content: space-around; }
        .image-container { text-align: center; padding: 10px; }
      "))
    ),
    
    tabItems(
      # Tab 1: Process
      tabItem(tabName = "process",
        fluidRow(
          box(
            title = "⚙️ Configuration", status = "primary", solidHeader = TRUE, width = 12,
            fluidRow(
              column(4,
                textInput("location", "Location Name:", value = "SR3_spotcheck",
                         placeholder = "e.g., SR3_spotcheck")
              ),
              column(4,
                selectInput("drone_model", "Drone Model:", 
                           choices = c("M3E" = "M3E", "Other" = "Other"),
                           selected = "M3E")
              ),
              column(4,
                numericInput("crop_size", "Crop Size (pixels):", 
                            value = 1800, min = 100, max = 5000, step = 100),
                helpText("📐 Pixel size for standard 50×50cm quadrat.",
                        "Estimate from calibration photo.",
                        "Default: 1800px for M3E at 1m altitude (normal camera, not zoom).")
              )
            )
          )
        ),
        
        fluidRow(
          box(
            title = "🚀 Processing Pipeline", status = "info", solidHeader = TRUE, width = 12,
            p("Click the steps below to process your drone images:"),
            br(),
            fluidRow(
              column(3,
                actionBttn("btn_rename", "Step 1: Rename Photos", 
                          style = "fill", color = "primary", size = "lg", block = TRUE,
                          icon = icon("file-signature"))
              ),
              column(3,
                actionBttn("btn_exif", "Step 2: Extract EXIF", 
                          style = "fill", color = "success", size = "lg", block = TRUE,
                          icon = icon("camera"))
              ),
              column(3,
                actionBttn("btn_crop", "Step 3: Crop Images", 
                          style = "fill", color = "warning", size = "lg", block = TRUE,
                          icon = icon("crop"))
              ),
              column(3,
                actionBttn("btn_export", "Step 4: Export Data", 
                          style = "fill", color = "danger", size = "lg", block = TRUE,
                          icon = icon("file-export"))
              )
            ),
            br(),
            verbatimTextOutput("process_log")
          )
        ),
        
        fluidRow(
          valueBoxOutput("box_photos", width = 3),
          valueBoxOutput("box_processed", width = 3),
          valueBoxOutput("box_altitude", width = 3),
          valueBoxOutput("box_coverage", width = 3)
        )
      ),
      
      # Tab 2: Map
      tabItem(tabName = "map",
        fluidRow(
          box(
            title = "🗺️ Photo Locations", status = "primary", solidHeader = TRUE, width = 12,
            leafletOutput("map", height = "600px"),
            br(),
            p("Click on markers to view photo details")
          )
        )
      ),
      
      # Tab 3: Data Explorer
      tabItem(tabName = "data",
        fluidRow(
          box(
            title = "📊 Metadata Table", status = "primary", solidHeader = TRUE, width = 12,
            downloadButton("download_data", "Download CSV", class = "btn-info"),
            br(), br(),
            DTOutput("data_table")
          )
        )
      ),
      
      # Tab 4: Gallery
      tabItem(tabName = "gallery",
        fluidRow(
          box(
            title = "🖼️ Image Gallery", status = "primary", solidHeader = TRUE, width = 12,
            sliderInput("gallery_select", "Select Image:", min = 1, max = 10, value = 1, 
                       step = 1, width = "100%"),
            uiOutput("image_display")
          )
        )
      ),
      
      # Tab 5: Statistics
      tabItem(tabName = "stats",
        fluidRow(
          box(
            title = "📈 Processing Statistics", status = "primary", solidHeader = TRUE, width = 6,
            plotlyOutput("altitude_plot")
          ),
          box(
            title = "⏱️ Time Distribution", status = "info", solidHeader = TRUE, width = 6,
            plotlyOutput("time_plot")
          )
        ),
        fluidRow(
          box(
            title = "📐 Image Dimensions", status = "success", solidHeader = TRUE, width = 12,
            plotlyOutput("dimension_plot")
          )
        )
      )
    )
  )
)

# Server Logic
server <- function(input, output, session) {
  
  # Reactive values to store data
  rv <- reactiveValues(
    spotcheck = NULL,
    exif_data = NULL,
    photos_renamed = FALSE,
    exif_extracted = FALSE,
    images_cropped = FALSE,
    data_exported = FALSE,
    log = "Ready to process images...\n"
  )
  
  # Check for existing work on startup (run once)
  observe({
    isolate({
      if(rv$log == "Ready to process images...\n") {
        log_msg <- ""
        
        # Check if photos already renamed
        if(dir.exists("raw_sorted_newname")) {
          renamed_files <- list.files("raw_sorted_newname", pattern = "\\.JPG$")
          if(length(renamed_files) > 0) {
            rv$photos_renamed <- TRUE
            log_msg <- paste0(log_msg, "✅ Found ", length(renamed_files), " renamed photos\n")
          }
        }
        
        # Check for existing CSV files to reload data
        csv_files <- list.files(pattern = "^SR3_spotcheck_[0-9]{8}\\.csv$")
        if(length(csv_files) > 0) {
          tryCatch({
            spotcheck <- read.csv(csv_files[1], stringsAsFactors = FALSE)
            if("TIMESTAMP" %in% colnames(spotcheck)) {
              spotcheck$TIMESTAMP <- as.POSIXct(spotcheck$TIMESTAMP)
            }
            rv$spotcheck <- spotcheck
            rv$exif_extracted <- TRUE
            log_msg <- paste0(log_msg, "✅ Loaded existing data from ", csv_files[1], "\n")
            updateSliderInput(session, "gallery_select", max = nrow(spotcheck))
          }, error = function(e) {
            # Silently fail if CSV can't be loaded
          })
        }
        
        # Check if images already cropped
        if(dir.exists("quads_50cm") || dir.exists("quads_square")) {
          crop_50cm <- if(dir.exists("quads_50cm")) length(list.files("quads_50cm", pattern = "\\.JPG$")) else 0
          crop_square <- if(dir.exists("quads_square")) length(list.files("quads_square", pattern = "\\.JPG$")) else 0
          
          if(crop_50cm > 0 || crop_square > 0) {
            rv$images_cropped <- TRUE
            log_msg <- paste0(log_msg, "✅ Found cropped images (50cm: ", crop_50cm, ", square: ", crop_square, ")\n")
          }
        }
        
        # Check if data files exist
        data_files <- list.files(pattern = "^SR3_spotcheck_[0-9]{8}_data\\.csv$")
        if(length(data_files) > 0) {
          rv$data_exported <- TRUE
          log_msg <- paste0(log_msg, "✅ Found exported data files\n")
        }
        
        if(log_msg != "") {
          rv$log <- paste0(rv$log, "\n🔍 Checking existing work...\n", log_msg, "\nYou can continue from any step!\n")
        }
      }
    })
  })
  
  # Step 1: Rename Photos
  observeEvent(input$btn_rename, {
    withProgress(message = 'Renaming photos...', value = 0, {
      
      tryCatch({
        list_photo <- list.files("raw", full.names = TRUE, pattern = "\\.JPG$")
        
        if(length(list_photo) == 0) {
          rv$log <- paste0(rv$log, "❌ ERROR: No JPG files found in 'raw' folder\n")
          showNotification("No JPG files found in 'raw' folder", type = "error")
          return()
        }
        
        dir.create("raw_sorted_newname", showWarnings = FALSE)
        
        for(i in 1:length(list_photo)) {
          incProgress(1/length(list_photo), detail = paste("Processing", i, "of", length(list_photo)))
          
          filename <- basename(list_photo[i])
          date_str <- substr(filename, 5, 12)
          new_filename <- paste0(input$location, "_", date_str, "_", sprintf("%03d", i), ".JPG")
          new_path <- file.path("raw_sorted_newname", new_filename)
          file.copy(list_photo[i], new_path, overwrite = TRUE)
        }
        
        rv$photos_renamed <- TRUE
        rv$log <- paste0(rv$log, "✅ Step 1 Complete: Renamed ", length(list_photo), " photos\n")
        showNotification(paste("Successfully renamed", length(list_photo), "photos!"), type = "message")
        
      }, error = function(e) {
        rv$log <- paste0(rv$log, "❌ ERROR: ", e$message, "\n")
        showNotification(paste("Error:", e$message), type = "error")
      })
    })
  })
  
  # Step 2: Extract EXIF
  observeEvent(input$btn_exif, {
    withProgress(message = 'Extracting EXIF data...', value = 0, {
      
      tryCatch({
        incProgress(0.3, detail = "Reading photos...")
        
        # Check if renamed folder exists
        if(!dir.exists("raw_sorted_newname")) {
          rv$log <- paste0(rv$log, "⚠️ Step 2 requires renamed photos. Please run Step 1 first.\n")
          showNotification("Please run Step 1 first to rename photos", type = "warning")
          return()
        }
        
        list_photo <- list.files("raw_sorted_newname", full.names = TRUE)
        
        if(length(list_photo) == 0) {
          rv$log <- paste0(rv$log, "❌ ERROR: No photos found in 'raw_sorted_newname' folder\n")
          showNotification("No photos found in 'raw_sorted_newname' folder", type = "error")
          return()
        }
        
        incProgress(0.3, detail = "Extracting metadata...")
        exif_data_df <- exifr::read_exif(list_photo, 
                                   tags = c("FileName", "DateTimeOriginal", "GPSLatitude", 
                                          "GPSLongitude", "RelativeAltitude", 
                                          "ImageWidth", "ImageHeight", "Model"),
                                   recursive = FALSE)
        
        exif_data_df <- as.data.frame(exif_data_df)
        exif_data_df <- exif_data_df %>% 
          select(FileName, DateTimeOriginal, GPSLatitude, GPSLongitude, 
                RelativeAltitude, ImageWidth, ImageHeight, Model)
        
        incProgress(0.3, detail = "Processing metadata...")
        colnames(exif_data_df) <- c("FILENAME", "TIMESTAMP", "LATITUDE", "LONGITUDE",
                                     "RELATIVEALTITUDE", "IMAGEWIDTH", "IMAGEHEIGHT", "MODEL")
        exif_data_df$TIMESTAMP <- as.POSIXct(exif_data_df$TIMESTAMP, format="%Y:%m:%d %H:%M:%S")
        exif_data_df$RELATIVEALTITUDE <- as.numeric(exif_data_df$RELATIVEALTITUDE)
        
        # Create spotcheck data
        spotcheck <- exif_data_df
        spotcheck$SITE_ID <- gsub(".JPG", "", spotcheck$FILENAME)
        spotcheck$SITE_ID <- sub(".*_([0-9]{3})$", "\\1", spotcheck$SITE_ID) %>% as.numeric()
        spotcheck <- spotcheck %>% arrange(SITE_ID)
        spotcheck$path <- paste0("raw_sorted_newname/", spotcheck$FILENAME)
        spotcheck$quad_path <- gsub("raw_sorted_newname", "quads", spotcheck$path)
        
        rv$spotcheck <- spotcheck
        rv$exif_data <- exif_data_df
        rv$exif_extracted <- TRUE
        
        incProgress(0.1, detail = "Done!")
        rv$log <- paste0(rv$log, "✅ Step 2 Complete: Extracted EXIF from ", nrow(spotcheck), " photos\n")
        showNotification("EXIF data extracted successfully!", type = "message")
        
        # Update slider range
        updateSliderInput(session, "gallery_select", max = nrow(spotcheck))
        
      }, error = function(e) {
        rv$log <- paste0(rv$log, "❌ ERROR: ", e$message, "\n")
        showNotification(paste("Error:", e$message), type = "error")
      })
    })
  })
  
  # Step 3: Crop Images
  observeEvent(input$btn_crop, {
    withProgress(message = 'Cropping images...', value = 0, {
      
      tryCatch({
        # Load data if not already in memory
        if(is.null(rv$spotcheck)) {
          csv_files <- list.files(pattern = paste0("^", input$location, "_[0-9]{8}\\.csv$"))
          if(length(csv_files) > 0) {
            spotcheck <- read.csv(csv_files[1], stringsAsFactors = FALSE)
            if("TIMESTAMP" %in% colnames(spotcheck)) {
              spotcheck$TIMESTAMP <- as.POSIXct(spotcheck$TIMESTAMP)
            }
            rv$spotcheck <- spotcheck
          } else {
            rv$log <- paste0(rv$log, "⚠️ Step 3 requires metadata. Please run Step 2 first.\n")
            showNotification("Please run Step 2 first to extract EXIF data", type = "warning")
            return()
          }
        }
        
        dir.create("quads_50cm", showWarnings = FALSE)
        dir.create("quads_square", showWarnings = FALSE)
        
        spotcheck <- rv$spotcheck
        
        for(i in 1:nrow(spotcheck)) {
          incProgress(1/nrow(spotcheck), detail = paste("Processing", i, "of", nrow(spotcheck)))
          
          img_width <- spotcheck$IMAGEWIDTH[i]
          img_height <- spotcheck$IMAGEHEIGHT[i]
          drone <- spotcheck$MODEL[i]
          
          img <- image_read(spotcheck$path[i])
          
          # Method 1: Fixed crop
          if(drone == "M3E" || input$drone_model == "M3E") {
            crop_size <- input$crop_size
            x_offset <- (img_width - crop_size)/2
            y_offset <- (img_height - crop_size)/2
            
            img_crop_50cm <- image_crop(img, paste0(crop_size, "x", crop_size, "+", x_offset, "+", y_offset))
            quad_path_50cm <- gsub("raw_sorted_newname", "quads_50cm", spotcheck$path[i])
            image_write(img_crop_50cm, path = quad_path_50cm, format = "jpg")
          }
          
          # Method 2: Square crop
          square_size <- min(img_width, img_height)
          x_offset <- (img_width - square_size)/2
          y_offset <- (img_height - square_size)/2
          
          img_crop_square <- image_crop(img, paste0(square_size, "x", square_size, "+", x_offset, "+", y_offset))
          quad_path_square <- gsub("raw_sorted_newname", "quads_square", spotcheck$path[i])
          image_write(img_crop_square, path = quad_path_square, format = "jpg")
        }
        
        rv$images_cropped <- TRUE
        rv$log <- paste0(rv$log, "✅ Step 3 Complete: Cropped ", nrow(spotcheck), " images\n")
        showNotification("Images cropped successfully!", type = "message")
        
      }, error = function(e) {
        rv$log <- paste0(rv$log, "❌ ERROR: ", e$message, "\n")
        showNotification(paste("Error:", e$message), type = "error")
      })
    })
  })
  
  # Step 4: Export Data
  observeEvent(input$btn_export, {
    tryCatch({
      # Load data if not already in memory
      if(is.null(rv$spotcheck)) {
        csv_files <- list.files(pattern = paste0("^", input$location, "_[0-9]{8}\\.csv$"))
        if(length(csv_files) > 0) {
          spotcheck <- read.csv(csv_files[1], stringsAsFactors = FALSE)
          if("TIMESTAMP" %in% colnames(spotcheck)) {
            spotcheck$TIMESTAMP <- as.POSIXct(spotcheck$TIMESTAMP)
          }
          rv$spotcheck <- spotcheck
        } else {
          rv$log <- paste0(rv$log, "⚠️ Step 4 requires metadata. Please run Step 2 first.\n")
          showNotification("Please run Step 2 first to extract EXIF data", type = "warning")
          return()
        }
      }
      
      spotcheck <- rv$spotcheck
      
      # Export metadata
      output_date <- unique(sub(".*_([0-9]{8})_.*", "\\1", spotcheck$FILENAME))[1]
      output_filename <- paste0(input$location, "_", output_date, ".csv")
      write.csv(spotcheck, file = output_filename)
      
      # Create data collection template
      spotcheck_data <- spotcheck %>% 
        select(SITE_ID, FILENAME, LATITUDE, LONGITUDE, TIMESTAMP)
      
      spotcheck_data$SG_PRESENCE <- ""
      spotcheck_data$SG_COVER <- ""
      spotcheck_data$SUBSTRATE <- ""
      spotcheck_data$CR <- ""
      spotcheck_data$CS <- ""
      spotcheck_data$HO <- ""
      spotcheck_data$HU <- ""
      spotcheck_data$SI <- ""
      spotcheck_data$EA <- ""
      spotcheck_data$TH <- ""
      spotcheck_data$TC <- ""
      spotcheck_data$ZC <- ""
      spotcheck_data$TOTAL <- ""
      spotcheck_data$AL_COVER <- ""
      spotcheck_data$EPI_COVER <- ""
      spotcheck_data$COMMENT <- ""
      
      output_data_filename <- paste0(input$location, "_", output_date, "_data.csv")
      write.csv(spotcheck_data, file = output_data_filename, row.names = FALSE)
      
      rv$data_exported <- TRUE
      rv$log <- paste0(rv$log, "✅ Step 4 Complete: Exported data files\n",
                      "   • ", output_filename, "\n",
                      "   • ", output_data_filename, "\n")
      showNotification("Data exported successfully!", type = "message")
      
    }, error = function(e) {
      rv$log <- paste0(rv$log, "❌ ERROR: ", e$message, "\n")
      showNotification(paste("Error:", e$message), type = "error")
    })
  })
  
  # Process log output
  output$process_log <- renderText({
    rv$log
  })
  
  # Value boxes
  output$box_photos <- renderValueBox({
    count <- if(!is.null(rv$spotcheck)) nrow(rv$spotcheck) else 0
    valueBox(count, "Photos Processed", icon = icon("camera"), color = "blue")
  })
  
  output$box_processed <- renderValueBox({
    status <- if(rv$data_exported) "Complete" else "In Progress"
    valueBox(status, "Status", icon = icon("check-circle"), 
            color = if(rv$data_exported) "green" else "yellow")
  })
  
  output$box_altitude <- renderValueBox({
    avg_alt <- if(!is.null(rv$spotcheck)) {
      round(mean(rv$spotcheck$RELATIVEALTITUDE, na.rm = TRUE), 1)
    } else 0
    valueBox(paste0(avg_alt, " m"), "Avg Altitude", icon = icon("plane"), color = "purple")
  })
  
  output$box_coverage <- renderValueBox({
    coverage <- if(!is.null(rv$spotcheck)) {
      paste0(nrow(rv$spotcheck) * 0.25, " m²")
    } else "0 m²"
    valueBox(coverage, "Coverage Area", icon = icon("map"), color = "orange")
  })
  
  # Map output
  output$map <- renderLeaflet({
    if(is.null(rv$spotcheck)) {
      # Return empty map if no data
      return(
        leaflet() %>%
          addProviderTiles(providers$Esri.WorldImagery) %>%
          setView(lng = 0, lat = 0, zoom = 2) %>%
          addControl(html = "<div style='background: white; padding: 10px; border-radius: 5px;'>No data loaded yet. Please complete Step 2 to extract EXIF data.</div>",
                    position = "topright")
      )
    }
    
    spotcheck <- rv$spotcheck
    
    leaflet(spotcheck) %>%
      addProviderTiles(providers$Esri.WorldImagery) %>%
      addCircleMarkers(
        lng = ~LONGITUDE,
        lat = ~LATITUDE,
        radius = 6,
        color = "#3c8dbc",
        fillColor = "#3c8dbc",
        fillOpacity = 0.7,
        popup = ~paste0(
          "<b>Site ID:</b> ", SITE_ID, "<br>",
          "<b>Filename:</b> ", FILENAME, "<br>",
          "<b>Time:</b> ", TIMESTAMP, "<br>",
          "<b>Altitude:</b> ", round(RELATIVEALTITUDE, 1), " m<br>",
          "<b>Lat:</b> ", round(LATITUDE, 6), "<br>",
          "<b>Lon:</b> ", round(LONGITUDE, 6)
        ),
        label = ~as.character(SITE_ID)
      ) %>%
      addScaleBar(position = "bottomleft")
  })
  
  # Data table
  output$data_table <- renderDT({
    req(rv$spotcheck)
    
    rv$spotcheck %>%
      select(SITE_ID, FILENAME, TIMESTAMP, LATITUDE, LONGITUDE, RELATIVEALTITUDE, MODEL) %>%
      datatable(
        options = list(pageLength = 25, scrollX = TRUE),
        rownames = FALSE,
        filter = 'top'
      ) %>%
      formatRound(c('LATITUDE', 'LONGITUDE', 'RELATIVEALTITUDE'), 3)
  })
  
  # Download handler
  output$download_data <- downloadHandler(
    filename = function() {
      paste0(input$location, "_", format(Sys.Date(), "%Y%m%d"), "_metadata.csv")
    },
    content = function(file) {
      write.csv(rv$spotcheck, file, row.names = FALSE)
    }
  )
  
  # Image gallery
  output$image_display <- renderUI({
    if(is.null(rv$spotcheck)) {
      return(p("No data loaded yet. Please complete Step 2 to extract EXIF data."))
    }
    
    idx <- input$gallery_select
    spotcheck <- rv$spotcheck
    
    if(idx > nrow(spotcheck)) return(NULL)
    
    original_path <- spotcheck$path[idx]
    crop_50cm_path <- gsub("raw_sorted_newname", "quads_50cm", original_path)
    crop_square_path <- gsub("raw_sorted_newname", "quads_square", original_path)
    
    # Function to create base64 encoded image
    encode_image <- function(path) {
      if(file.exists(path)) {
        img_data <- image_read(path)
        img_resized <- image_resize(img_data, "800x800")
        temp_file <- tempfile(fileext = ".jpg")
        image_write(img_resized, temp_file, format = "jpg", quality = 85)
        encoded <- base64enc::base64encode(temp_file)
        unlink(temp_file)
        return(paste0("data:image/jpeg;base64,", encoded))
      }
      return(NULL)
    }
    
    tagList(
      h4(paste("Site ID:", spotcheck$SITE_ID[idx], "—", spotcheck$FILENAME[idx])),
      fluidRow(
        column(4,
          h5("Original Image"),
          if(file.exists(original_path)) {
            tags$img(src = encode_image(original_path), width = "100%", 
                    style = "border: 2px solid #ddd; border-radius: 4px;")
          } else {
            p("Image not found")
          }
        ),
        column(4,
          h5("50cm Crop (1800x1800)"),
          if(file.exists(crop_50cm_path)) {
            tags$img(src = encode_image(crop_50cm_path), width = "100%", 
                    style = "border: 2px solid #3c8dbc; border-radius: 4px;")
          } else {
            p("Image not yet cropped. Please complete Step 3.")
          }
        ),
        column(4,
          h5("Square Crop"),
          if(file.exists(crop_square_path)) {
            tags$img(src = encode_image(crop_square_path), width = "100%", 
                    style = "border: 2px solid #00a65a; border-radius: 4px;")
          } else {
            p("Image not yet cropped. Please complete Step 3.")
          }
        )
      )
    )
  })
  
  # Statistics plots
  output$altitude_plot <- renderPlotly({
    req(rv$spotcheck)
    
    p <- ggplot(rv$spotcheck, aes(x = SITE_ID, y = RELATIVEALTITUDE)) +
      geom_line(color = "#3c8dbc", size = 1) +
      geom_point(color = "#3c8dbc", size = 3) +
      labs(x = "Site ID", y = "Altitude (m)", title = "Altitude by Site") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$time_plot <- renderPlotly({
    req(rv$spotcheck)
    
    p <- ggplot(rv$spotcheck, aes(x = TIMESTAMP)) +
      geom_histogram(fill = "#00a65a", color = "white", bins = 20) +
      labs(x = "Time", y = "Count", title = "Photo Capture Timeline") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$dimension_plot <- renderPlotly({
    req(rv$spotcheck)
    
    plot_ly(rv$spotcheck, x = ~IMAGEWIDTH, y = ~IMAGEHEIGHT, 
            text = ~paste("Site:", SITE_ID), type = 'scatter', mode = 'markers',
            marker = list(size = 10, color = '#f39c12', 
                         line = list(color = 'white', width = 2))) %>%
      layout(title = "Image Dimensions",
             xaxis = list(title = "Width (px)"),
             yaxis = list(title = "Height (px)"))
  })
}

# Run the application
shinyApp(ui = ui, server = server)

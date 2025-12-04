# Drone Spotcheck Shiny App

An interactive R Shiny application for processing drone spotchecks.

## Prerequisites

### 1. R and RStudio
- Download and install from: https://posit.co/download/rstudio-desktop/

### 2. ExifTool (Required for extracting photo metadata)

**Windows Installation Steps:**

1. **Download ExifTool:**
   - Go to: https://exiftool.org/
   - Download the Windows Executable (e.g., `exiftool-12.70.zip`)

2. **Extract and Rename:**
   - Extract the downloaded zip file
   - You will find a file named `exiftool(-k).exe`
   - Rename it to `exiftool.exe` (remove the `(-k)` part)

3. **Choose Installation Location:**
   - **Option A (Recommended):** Place in `C:\Windows\` (requires admin rights)
   - **Option B:** Place in a folder like `C:\exiftool\` and add to PATH (see step 4)

4. **Add to PATH Environment Variable (if using Option B):**
   - Right-click on "This PC" or "My Computer" → Properties
   - Click "Advanced system settings" → "Environment Variables"
   - Under "System variables", find and select "Path" → Click "Edit"
   - Click "New" and add the folder path (e.g., `C:\exiftool\`)
   - Click "OK" to close all windows

5. **Verify Installation:**
   - Open Command Prompt (Win + R, type `cmd`, press Enter)
   - Type: `exiftool -ver`
   - You should see the version number (e.g., `12.70`)
   - If you see an error, restart your computer and try again

## Setup

Before running the app, place your raw drone images in the `raw` folder in the project directory.

## How to Run the App

1. Double click on `Spotchecks.Rproj` to open the project in RStudio
2. Open `drone_spotcheck_shiny_app.R` from the files pane (bottom right corner)
3. Click on "Run App" in the top right of the script pane
4. A pop-up window will appear with the interactive app

## Features & Functionality

### Automatic Package Management
The app automatically checks and installs all required R packages from CRAN when first launched, including:
- `shiny`, `shinydashboard` - Web application framework
- `tidyverse` - Data manipulation and visualization
- `terra`, `tidyterra` - Spatial data processing
- `magick` - Image processing and cropping
- `lubridate` - Date/time handling
- `exifr` - EXIF metadata extraction
- `leaflet` - Interactive mapping
- `DT` - Interactive data tables
- `shinyjs`, `shinyWidgets`, `plotly`, `base64enc` - UI enhancements

### Processing Pipeline

#### **Step 0: Detect Duplicates** 🔍
Identify and manage duplicate images taken at the same location or in quick succession.

**Features:**
- **Automatic Detection**: Groups photos by proximity (distance threshold in meters) OR time (seconds)
- **Interactive Map Viewer**: Visualize duplicate groups on a satellite map with color-coded markers
- **Distance Calculation**: See maximum distance between photos in each group
- **Adjustable Thresholds**: Customize distance (1-50m) and time (5-120s) sensitivity
- **Image Comparison**: View large previews (800x800px) of duplicate candidates side-by-side
- **Full-Size Zoom**: Click any image to open full resolution in a popup window
- **Manual Selection**: Choose the best image from each duplicate group
- **Refresh Detection**: Re-analyze with new settings without re-scanning photos

**Outputs:**
- `raw_filtered/` folder with selected non-duplicate images

#### **Step 1: Rename Photos** 📝
Systematically rename photos for consistent organization.

**Features:**
- Uses filtered photos (if duplicates were removed) or raw photos
- Generates standardized filenames: `{Location}_{Date}_{Number}.JPG`
- Sequential numbering for easy reference
- Preserves original files in source folder

**Outputs:**
- `raw_sorted_newname/` folder with renamed images

#### **Step 2: Extract EXIF** 📷
Extract comprehensive metadata from drone images.

**Features:**
- GPS coordinates (latitude, longitude)
- Capture timestamp
- Flight altitude (relative)
- Image dimensions
- Drone model information
- Automatic data type conversion and validation

**Outputs:**
- In-memory database of all photo metadata
- Ready for mapping and analysis

#### **Step 3: Crop Images** ✂️
Create standardized quadrat crops for analysis.

**Features:**
- **Two Cropping Methods:**
  1. **50cm Crop**: Fixed pixel size (default 1800x1800) for standard quadrats
  2. **Square Crop**: Maximum centered square based on image dimensions
- **Adjustable Crop Size**: Configure pixel dimensions (100-5000px) for different altitudes
- **Drone-Specific Settings**: Preconfigured for M3E and other models
- **Progress Tracking**: Real-time processing status

**Outputs:**
- `quads_50cm/` folder with fixed-size crops
- `quads_square/` folder with maximum square crops

#### **Step 4: Export Data** 💾
Generate data collection templates and metadata files.

**Features:**
- **Metadata CSV**: Complete photo information (coordinates, time, altitude, etc.)
- **Data Collection Template**: Pre-structured CSV with fields for:
  - Seagrass presence and cover
  - Substrate type
  - Species codes (CR, CS, HO, HU, SI, EA, TH, TC, ZC)
  - Cover percentages (algae, epiphytes)
  - Comments field
- **Automatic Dating**: Files timestamped with capture date

**Outputs:**
- `{Location}_{Date}.csv` - Full metadata
- `{Location}_{Date}_data.csv` - Data collection template

### Interactive Visualization Tabs

#### **🗺️ Map View**
- Satellite imagery basemap (Esri WorldImagery)
- High zoom capability (up to level 25) for detailed inspection
- Color-coded markers for each photo location
- Clickable popups showing:
  - Site ID and filename
  - Capture time and altitude
  - GPS coordinates
- Scale bar for distance reference

#### **📊 Data Explorer**
- Sortable, filterable data table
- All metadata fields displayed
- Search functionality across all columns
- Export filtered data to CSV
- Pagination for large datasets

#### **🖼️ Image Gallery**
- Side-by-side comparison of original and cropped images
- Slider navigation through all sites
- Three views per site:
  - Original image
  - 50cm crop preview
  - Square crop preview
- Site ID and filename labels

#### **📈 Statistics**
- **Altitude Profile**: Line plot showing flight altitude variation across sites
- **Time Distribution**: Histogram of photo capture timeline
- **Image Dimensions**: Scatter plot of photo width vs height
- Interactive plotly charts with zoom and pan

### Additional Features

- **Resume Processing**: Automatically detects existing work and allows continuation from any step
- **Progress Indicators**: Real-time feedback during lengthy operations
- **Error Handling**: Informative error messages with troubleshooting tips
- **Responsive Design**: Dashboard layout adapts to window size
- **Status Tracking**: Visual indicators (✅❌⚠️) for completed/pending steps
- **Value Boxes**: At-a-glance metrics (photo count, status, average altitude, coverage area)

## Workflow Example

1. **Prepare**: Place raw drone images in `raw/` folder
2. **Detect Duplicates**: Run Step 0, review map and images, select best photos
3. **Rename**: Run Step 1 to organize filtered photos
4. **Extract Metadata**: Run Step 2 to get GPS and flight data
5. **Visualize**: Check Map View to verify coverage
6. **Crop Images**: Run Step 3 to create analysis quadrats
7. **Export**: Run Step 4 to generate data collection template
8. **Analyze**: Use Gallery to review each site and fill in data template

## Author

**Lucas Langlois**  
Research Officer  
Centre of Tropical Water & Aquatic Ecosystem Research (TropWATER)  
James Cook University (JCU), Australia

📧 lucas.langlois@jcu.edu.au

## Citation

If you use this application in your work, please cite:

```
Langlois, L. (2025). Drone Spotcheck Shiny App. 
Centre of Tropical Water & Aquatic Ecosystem Research (TropWATER), 
James Cook University, Australia.
https://github.com/lucas-langlois/drone_spotcheck_shiny_app
```

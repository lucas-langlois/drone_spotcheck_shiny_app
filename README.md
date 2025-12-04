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

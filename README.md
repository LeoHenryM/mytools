# 📦 mytools

**Version:** 0.1.4  
**Author:** Leo HENRY  
**Email:** leo.henry@mail.utoronto.ca  

---

## 🔧 Description

`mytools` is a personal collection of useful R functions developed for improving workflow, generating cleaner outputs, and enhancing visualizations. This toolbox includes utilities for plotting, PDF/image conversion, cloud integration, and axis formatting in `ggplot2`.

---

## 🛠️ Key Functions

Here’s a non-exhaustive list of what's inside:

### 📊 Plotting and Output
- `Savegrph(grph, path, width, height)`: Saves a plot to a PDF file.
- `PngfromPdf(path, resolution)`: Converts PDFs in a folder to PNG images.
- `LatexFromDf()`: Formats a data frame into LaTeX for publication-ready output.

### 📈 ggplot2 Enhancements
- `ggbreaks(x, y)`: Applies custom axis breaks and formatting to ggplots.
- `calculate_breaks(limits)`: Internal function to adaptively set axis breaks.
- `calculate_minor_breaks(limits)`: Computes minor breaks for better readability.

### ☁️ OneDrive/Notion Utilities
- `OneDriveLinks(folder_path)`: Generates sharable OneDrive links for files.
- `NotionCrap()`: Some Notion API automation (adds text blocks, previews, etc.).

### 🧪 Misc
- `FastOddsRatio()`: Statistical utility (details not extracted).
- `Update()`: Quickly rebuilds and reinstalls the package using `devtools`.

---

## 🚀 Installation

Install directly from GitHub:

```r
# install.packages("devtools")
devtools::install_github("LeoHenryM/mytools")
    

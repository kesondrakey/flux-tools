# fluxtools v0.2.0

Interactive Shiny application for QA/QC of AmeriFlux‐style flux‐tower data.  
Designed to streamline the post‐processing of eddy covariance datasets (e.g., after EddyPro, physical boundary filtering) to detect and remove outliers, and generate reproducible R code for QA/QC AmeriFlux submission.

If you use fluxtools in your workflow, please cite the use of this product. 

If you use fluxtools in your workflow, please cite:
Key, K. (2025). fluxtools (version 0.2.0) [Computer software]. Zenodo. https://doi.org/10.5281/zenodo.15597159

---

## Table of Contents

- [Key Features](#key-features)  
- [Installation](#installation)  
- [Quickstart](#quickstart)  
- [Data Requirements](#data-requirements)  
- [How It Works](#how-it-works)  
  - [Timestamp Parsing & Time Zones](#timestamp-parsing--time-zones)  
  - [Interactive QC & Selection](#interactive-qc--selection)  
  - [Outlier Detection](#outlier-detection)  
  - [Code Generation & Copy-All](#code-generation--copy-all)  
- [Download & Reset](#download--reset)  
- [Vignette & Docs](#vignette--docs)  
- [Citation](#citation)  
- [License](#license)  

---

## Key Features

- **Interactive Scatter Plot**  
  Choose any numeric variable for X (often `TIMESTAMP_START`) and Y; drag a box or lasso to flag points

- **Multi-Year Support**  
  Upload up to 100 MB of flux‐tower CSV (all years by default, or pick a subset)

- **Dark/Light Mode**  
  Toggle UI theme on the fly (switch is in the top-right of the app)

- **±σ Outlier Highlighting**  
  Use the slider to mark points beyond N standard deviations from a linear fit; click **Select ±σ outliers** to add them

- **Accumulate & Undo**  
  - **Select data** current selection to an accumulated list across variables
  - **Clear Selection** individual points or clear outliers and selections

- **Confirm Removal**  
  Hit **Apply removals** to convert flagged Y-values to `NA` in your data frame and immediately refresh the plot.

- **Export Cleaned Data**  
  Download a CSV with removed points set to `NA` plus an R-script of all your removal code in one go.

---

## Installation

```r
# CRAN (coming soon)
install.packages("fluxtools")

# Install from GitHub
library(devtools) 
devtools::install_github("kesondrakey/fluxtools")


#Load fluxtools and launch the QA/QC application:
library(fluxtools)

# Add the UTC offset for your flux tower site (e.g., UTC-5 for EST)
run_flux_qaqc(-5)

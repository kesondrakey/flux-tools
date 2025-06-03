# flux-tools

Interactive Shiny application for QA/QC of AmeriFlux‐style flux‐tower data.  
Designed to streamline the post‐processing of eddy covariance datasets (e.g., after EddyPro, physical boundary filtering) to detect and remove outliers, and generate reproducible R code for QA/QC AmeriFlux submission.

---

## Table of Contents

- [Key Features](#key-features)  
- [Installation](#installation)  
- [Usage](#usage)  
- [Data Requirements](#data-requirements)  
- [How It Works](#how-it-works)  
  - [Timestamp Parsing & Time Zones](#timestamp-parsing--time-zones)  
  - [Interactive QC & Selection](#interactive-qc--selection)  
  - [Outlier Detection](#outlier-detection)  
  - [Code Generation](#code-generation)  
- [Download & Reset](#download--reset)  
- [Citation](#citation)  
- [License](#license)  

---

## Key Features

- **Interactive Scatter Plot**  
  Drag a lasso or box over points of any numeric variable (Y‐axis) against time or another variable (X‐axis) to inspect and flag questionable measurements.

- **±σ Outlier Highlighting (optional)**  
  Choose a threshold (in standard deviations) to automatically highlight points beyond ±σ from a linear‐model fit. Quickly “Add all ±σ outliers” to your accumulated removal list.

- **Accumulate or Remove Selections**  
  ‣ Add/accumulate current lasso selections across variables → generate R code snippets to “case_when” these raw timestamps to `NA_real_`.  
  ‣ Remove from the accumulated list (undo previous marks).  
  ‣ Reset just the current selection or reset all filters to start over.

- **Confirm Removal**  
  Clicking **Confirm Remove** sets the chosen Y‐variable to `NA` for all flagged rows and records their original timestamp strings. These removals appear in the “Removed data points” code box.

- **Generated R Code**  
  For reproducibility, every selection (current or accumulated) is translated into a self‐contained R snippet.
  This code removes the y-axis variable based on TIMESTAMP_START, and turns them in NA. This code can be copied and 
  pasted directly into the users R script for QA/QC purposes. The accuumulated code will generate multiple variables
  using the "add current selection" button
  
  Example:
  
```r
df2 <- df2 %>%
  mutate(
    FC_1_1_1 = case_when(
      TIMESTAMP_START == '202401062100' ~ NA_real_,
      TIMESTAMP_START == '202401090000' ~ NA_real_,
      TIMESTAMP_START == '202401090030' ~ NA_real_,
      TIMESTAMP_START == '202401100200' ~ NA_real_,
      TIMESTAMP_START == '202401230700' ~ NA_real_,
      TIMESTAMP_START == '202401261830' ~ NA_real_,
      TRUE ~ FC_1_1_1
    )
  )

```

Please cite the use of this product. 

CITATION:
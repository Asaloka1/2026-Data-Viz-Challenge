# Following the Value Chain: Understanding the Carolina Gold Rice Revival Through Farming and Milling Infrastructure

##2026 Local Food Economics Data Visualization Challenge **

*Co-organized by the USDA Agricultural Marketing Service (AMS) & the Agricultural & Applied Economics Association (AAEA)*

**Assigned Sector:** Grain, Oilseed and Other Field Crops (Excluding Fiber)


## Authors
Atta Selorm Aloka  
Alice Mukunzi  
Peace Okafor  

## Mentors
Yoko Kusunose  
Annelise Straw  
Angela Gardner   

---

## Project Overview

This project was developed for the 2026 USDA Agricultural Marketing Service Local Food Economics Data Visualization Challenge under the assigned sector **Grain, Oilseed and Other Field Crops (Excluding Fiber)**.

The analysis examines the relationship between rice production and rice milling infrastructure in Arkansas, North Carolina, and South Carolina. Using publicly available establishment data, the project visualizes where rice-related agricultural activity is reported and how production and processing infrastructure are distributed across regions.

The project consists of two complementary visualizations. Figure 1 maps rice farming and rice milling establishments reported in publicly available data sources. Figure 2 illustrates the approximate transportation distances between rice production areas in the Carolinas and major milling infrastructure in Arkansas, highlighting how production and value-added processing may occur in different locations.

---

## Research Question

How are rice production and rice milling establishments distributed across Arkansas and the Carolinas, and what do publicly available data reveal about the geographic relationship between production and processing infrastructure?

---

## Data Sources

### USDA Agricultural Marketing Service (AMS) Local Food Data Warehouse

The primary data source was the USDA AMS Local Food Data Warehouse.

Establishment data were obtained through the Local Food Data Warehouse and are based on Bureau of Labor Statistics (BLS) Quarterly Census of Employment and Wages (QCEW) data.

### Bureau of Labor Statistics (BLS) QCEW

2024 annual average establishment counts were used for:

- NAICS 111160 (Rice Farming)
- NAICS 311212 (Rice Milling)

### U.S. Census Bureau TIGER/Line Shapefiles

County and state boundary files were obtained through the `tigris` R package using 2024 Census cartographic boundary files.

### Supplemental Context

Discussions with North Carolina Cooperative Extension personnel provided historical and operational context regarding rice production and processing activities in the Carolinas (North Carolina and South Carolina)

These discussions were used only to assist interpretation of patterns observed in the public data and were not used as a primary data source.

---

## Figure Descriptions

### Figure 1: Rice Production and Milling Infrastructure

Figure 1 maps publicly reported rice farming establishments and rice milling establishments across Arkansas, North Carolina, and South Carolina.

County shading represents the number of rice farming establishments reported in publicly available data. Milling establishments are represented by point symbols scaled according to the reported number of establishments.

### Figure 2: Long-Haul Rice Milling Route

Figure 2 illustrates the geographic separation between rice production areas in the Carolinas and major rice milling infrastructure in Arkansas.

The map includes approximate distances of 845 miles and 750 miles between production and processing regions.

---

## Data Processing and Analysis

### Rice Farming Layer

Rice farming establishment data were imported from the Local Food Data Warehouse and standardized using county FIPS codes.

### Rice Milling Layer

Rice milling establishment data were imported from the Local Food Data Warehouse and standardized using county FIPS codes.

### Geographic Processing

Spatial layers were processed using the `sf`, `tigris`, and `maps` packages in R.

### Visualization Design

Visualizations were designed using `ggplot2`, `sf`, `grid`, and `gridExtra`.

---

## Software Requirements

- R
- tidyverse
- sf
- tigris
- maps
- gridExtra
- grid

---

## File Structure

```text
README.md

Figure1_Rice_Production_and_Milling_Infrastructure.R
Figure2_Long_Haul_Rice_Milling_Route.R

Figure1_Rice_Production_and_Milling_Infrastructure.png
Figure2_Long_Haul_Rice_Milling_Route.png

Figure1_Rice_Production_and_Milling_Infrastructure.pdf
Figure2_Long_Haul_Rice_Milling_Route.pdf

final_rice_farming_layer_2024.csv
final_rice_milling_infrastructure_2024.csv

One_Page_Narrative.pdf
```

---

## Replication Instructions

1. Place both CSV files in the project working directory.
2. Install all required R packages. You can quickly verify and install them by running:
   `install.packages(c("tidyverse", "sf", "tigris", "maps", "gridExtra"))`
3. Run Figure 1 R script.
4. Run Figure 2 R script.
5. PNG and PDF outputs will be generated automatically.

---

## Use of Generative AI

Generative AI was used only for limited technical assistance related to code troubleshooting and syntax verification. All research design, analytical decisions, data interpretation, visualizations, and written conclusions were developed by the project team.

---

## Recommended Citation

Atta Selorm Aloka, Alice Mukunzi, and Peace Okafor. (2026). *Following the Value Chain: Understanding Carolina Gold Rice Production, Processing, and Value-Added Opportunities in the Carolinas*. 2026 USDA Agricultural Marketing Service Local Food Economics Data Visualization Challenge.

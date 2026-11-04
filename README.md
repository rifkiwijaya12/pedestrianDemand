# Pedestrian Demand Estimation and Priority Investment
## Research Background
Active travel, particularly walking, is widely recognised as a sustainable transport strategy that can contribute to improved urban air quality.
However, effective pedestrian infrastructure planning requires a systematic understanding of where pedestrian demand occurs and whether existing infrastructure can adequately accommodate it.
In Jakarta, the spatial distribution of pedestrian demand and the availability of pedestrian infrastructure may not always be aligned.
Therefore, understanding this demand–supply gap is therefore essential for identifying locations where pedestrian infrastructure investments can have the greatest potential impact.

## Research Objectives
1. Estimate pedestrian travel demand across Jakarta’s five administrative cities using a four-step transport modelling approach.
2. Assign estimated pedestrian demand to the street network using OpenStreetMap (OSM) data.
3. Identify demand–supply gaps by comparing pedestrian demand with the mapped provision of pedestrian infrastructure.
4. Identify and classify potential intervention priorities using spatial clustering analysis to support preliminary pedestrian infrastructure investment planning.
   
## Research Methodology
The methodology consists of the following stages:
1. Travel demand estimation: Apply the four-step transport modelling framework to estimate pedestrian trips across the study area.
2. Infrastructure provision assessment: Integrate the estimated pedestrian demand with mapped pedestrian infrastructure to identify demand–supply gaps and high-demand road segments.
3. Spatial clustering and prioritisation: Apply DBSCAN clustering to identify potential intervention clusters across three infrastructure categories:
   a. High pedestrian demands in roads without mapped pedestrian infrastructure,
   b. High pedestrian demands in shared streets facilities,
   c. High pedestrian demands in roads with mapped dedicated pedestrian facilities.
   
## Repository Structure
**data/ — Input Datasets**
This directory contains the input datasets used throughout the research workflow

| Dataset | Description |
| :--- | :--- |
| `population/` | Population data used to represent the spatial distribution of population and support travel demand estimation.|
| `land_use/` | Land-use data used to represent the spatial characteristics and activity distribution of the study area.|
| `jakarta_network/` | Jakarta street network data used for pedestrian demand assignment and infrastructure assessment.|
| `transjakarta_gtfs/` | TransJakarta General Transit Feed Specification (GTFS) data used to represent public transport services and support the analysis of multimodal pedestrian trips.|

**script/ — Analytical Workflow**
| Script | Description |
| :--- | :--- |
| `00_setup` | Initialises the research environment, loads required packages, and defines common settings and input paths. |
| `01_Trip_Production` | Estimates the number of trips generated from the study area or transport analysis zones. |
| `01_Trip_Attraction` | Estimates the number of trips attracted to each transport analysis zone. |
| `02_Trip_Distribution` | Distributes estimated trips between origin and destination zones. |
| `03_ModeSplit_Assignment` | Estimates pedestrian mode shares and assigns pedestrian demand to the street network. |
| `05_Visualisation` | Produces maps, charts, and other visual representations of pedestrian demand and infrastructure provision. |
| `06_PriorityInvesment` | Identifies and classifies potential pedestrian infrastructure investment priorities based on the demand–supply assessment and spatial clustering results. |

**Execution Order**
The analytical scripts are designed to be executed sequentially, following the numbering convention in the script/ directory.

## Quick Start
### Prerequisites
Before running this project, ensure that you have installed:
- [R](https://cran.r-project.org/)
- [RStudio](https://posit.co/download/rstudio-desktop/)

### 1. Clone the repository

Open a terminal and run:

```bash
git clone https://github.com/rifkiwijaya12/pedestrianDemand.git
```

Navigate to the project directory:

```bash
cd REPOSITORY
```

### 2. Open the project

Open the `.Rproj` file in RStudio.

### 3. Run the analysis

Follow the analysis workflow described in the repository structure and research methodology sections. Initialize with setup, proceed through the 4 step demand model sequence (Trip Production+Attraction, Trip Distribution, Mode Choice, Trip Assignment) and Finish with visualization and priority investment

## Live demo
Explore the interactive visualization as part of research project output:
https://rifkiwijaya12-pedestriandemandapp.share.connect.posit.cloud/

For users who interest in changing parameter, please follow the instruction the Quick Start section.

## Data Availability

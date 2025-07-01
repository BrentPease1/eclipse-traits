# Avian responses to solar eclipses

### [Neil A. Gilbert](https://www.gilbertecology.com/)†, [Brent S. Pease](https://peaselab.com/)†, [MaryKay Severino](https://eclipsesoundscapes.org), and [Henry 'Trae' Winter III](https://eclipsesoundscapes.org)

† _Shared first authorship_
### Data/code DOI:

#### Please contact Neil Gilbert (neil.gilbert@okstate.edu) or Brent Pease (bpease1@siu.edu) for questions about the code or data.

__________________________________________________________________________________________________________________________________________

## Abstract
Solar eclipses represent natural experiments to evaluate the effect of light fluctuations on wildlife behavior. Recent advances in automated data collection have motivated efforts to document wildlife responses to recent eclipses. We analyzed data for 181 bird species from 873 locations sampled by Eclipse Soundscapes and BirdWeather, two community-science programs in which volunteers deploy acoustic recording units, to quantify avian behavioral responses to the 2023 annular eclipse and 2024 total eclipse in North America. Our foci were to 1) understand drivers of variability in avian behavioral responses to eclipses and 2) quantify the levels of solar obscuration that either promote or suppress vocal activity. We found that, on average, most species were less likely to vocalize during the peak of the total eclipse and that most species showed no changes in vocalization during the peak of the annular eclipse. The diel niche of a species strongly predicted its response to the total eclipse: species that typically vocalize before sunrise and after sunset were especially likely to vocalize during totality. Similarly, eye size predicted eclipse response, with large-eyed species more likely to vocalize during totality than small-eyed species. We found that obscuration levels between 70% and 93% were associated with increased vocalization of most species and that suppressive effects of the eclipse only emerged at locations within 285 kilometers of the total eclipse centerline. Our results provide a quantitative synthesis of avian responses to eclipses and demonstrate the power of the people in helping document biological responses to ephemeral conditions. 

 $~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~$ <img src="https://github.com/BrentPease1/eclipse-traits/blob/main/Figures/figure_01.png" width="600" />
 
## Repository Directory

### Data
 * [2023eclipse_shapefiles](./Data/2023eclipse_shapefiles) Folder with shapefile of 2023 annular eclipse path
 * [2024eclipse_shapefiles](./Data/2024eclipse_shapefiles) Folder with shapefile of 2024 total eclipse path
 * [metadata](./Data/metadata) Folder containing metadata provided by Eclipse Soundscapes Team. All files within this folder generally contain the following information:
   | Column name | Description |
   |-------------|-------------|
   | ESIDNumber | Sensor identifier specific to Eclipse Soundscapes|
   | Latitude | Latitude (EPSG 4326) of sensor |
   | Longitude | Longitude (EPSG 4326) of sensor |
   | coverage | The maximum coverage (percent obscuration of sun's face) that a location experienced during eclipse |
   | FirstContactDate | Date of first contact during eclipse |
   | FirstContactTimeUTC | Time of first contact (UTC time) during eclipse. See [this link](https://en.wikipedia.org/wiki/Solar_eclipse#:~:text=The%20visual%20phases%20observed%20during,the%20entire%20disk%20is%20covered.) for an explanation of eclipse contacts. |
   | SecondContactTimeUTC | Time of second contact (UTC time) during eclipse |
   | ThirdContactTimeUTC | Time of third contact (UTC time) during eclipse |
   | FourthContactTimeUTC | Time of fourth contact (UTC) during eclipse |
   | MaxEclipseTimeUTC | Time (UTC) of eclipse maximum |

These metadata were merged with bird detection information and used in subsequent analysis. 
   * [2023_annular_locations_times.csv](./Data/metadata/2023_annular_locations_times.csv) 2023 Annular Eclipse metadata provided by Eclipse Soundscapes
   * [es2024_locations_times.csv](./Data/metadata/es2024_locations_times.csv) 2024 Total Eclipse metadata provided by Eclipse Soundscapes
   * [es2024_locations_timesv02.csv](./Data/metadata/es2024_locations_timesv02.csv) 2024 Total Eclipse metadata provided by Eclipse Soundscapes. A second round of data arrived from Eclipse Soundscapes, and the metadata for these deployments were stored in this csv. 
 * [avonet.csv](./Data/avonet.csv) AVONET database; the two relevant columns we use are `Habitat.Density` and `Migration`. See [Tobias et al. 2022](https://onlinelibrary.wiley.com/doi/10.1111/ele.13898) for full details
 * [birdweather_elton_botw_name_key.csv](./Data/birdweather_elton_botw_name_key.csv) Crosswalk table for resolving name differences between datasets
 * [cavity.csv](./Data/cavity.csv) Table with nest type. Derived from reviewing species pages on [Birds of the World](https://birdsoftheworld.org/bow/home). Column meanings provided below.
   | Column name | Description |
   |-------------|-------------|
   | com_name | Species common name |
   | sci_name_bw | Species scientific name according to BirdWeather |
   | cavity | Binary classification of whether the species nests in a cavity (1) or not (0) |
 * merged_es_bw_apr2025_viirs.csv Detection data from BirdWeather and Eclipse Soundscapes. NOTE: This file is too large to host on GitHub. It can be downloaded from [this GoogleDrive link](https://drive.google.com/file/d/1K8n42R5rQxtQan30fcZVU59NITi9pD52/view?usp=sharing). This file must be downloaded and placed in the [Data](./Data) folder for many of the scripts to run. Column information for the table provided below.
   | Column name | Description |
   |-------------|-------------|
   | lat_lon_index | Index for grouping of latitude and longitude. Not used in downstream analysis  |
   | ESIDNumber | Sensor identifier specific to Eclipse Soundscapes|
   | common_name | Species common name (BirdNET output) |
   | scientific_name | Species scientific name (BirdNET output) |
   | start_time | Start time of audio clip |
   | confidence | BirdNET confidence score |
   | timestamp | Timestamp of detection |
   | day | Day of month |
   | hour | Hour of detection |
   | date | Date of detection |
   | Latitude | Latitude (EPSG 4326) of sensor |
   | Longitude | Longitude (EPSG 4326) of sensor |
   | coverage | The maximum coverage (percent obscuration of sun's face) that a location experienced during eclipse |
   | FirstContactDate | Date of first contact during eclipse |
   | FirstContactTimeUTC | Time of first contact (UTC time) during eclipse. See [this link](https://en.wikipedia.org/wiki/Solar_eclipse#:~:text=The%20visual%20phases%20observed%20during,the%20entire%20disk%20is%20covered.) for an explanation of eclipse contacts. |
   | SecondContactTimeUTC | Time of second contact (UTC time) during eclipse |
   | ThirdContactTimeUTC | Time of third contact (UTC time) during eclipse |
   | FourthContactTimeUTC | Time of fourth contact (UTC) during eclipse |
   | MaxEclipseTimeUTC | Time (UTC) of eclipse maximum |
   | eclipse_event | Indicates whether row pertains to total eclipse ("total") or annular eclipse ("annular") |
   | dist_to_center | Distance (m) of sensor location from centerline of eclipse path |
   | path_status | Indicates whether sensor was within eclipse path or not |
   | dist_km | Distance (km) of sensor location from centerline of eclipse path |
   | activity | Indicates whether a species is coded as nocturnal or diurnal |
   | data_source | Indicates whether data comes from Birdweather ("BW") or Eclipse Soundscapes ("ES") |
   | Station | Station identifier for BirdWeather |
   | year | Year |
   | month | Month |
   | timestamp_group | Index grouping based on time |
   | avg_rad | Radiance from the [VIIRS Nighttime Light](https://eogdata.mines.edu/products/vnl/) satellite data product; used as a measure of light pollution at a location |
   
 * [elton.txt](./Data/elton.txt) Elton Traits database; using this just to grab family names. See [Wilman et al. 2014](https://esajournals.onlinelibrary.wiley.com/doi/10.1890/13-1917.1) for details.
 * [pnoct_v01.csv](./Data/pnoct_v01.csv) Table with proportion of detections during nighttime (before sunrise or after sunset) under typical conditions. Derived from BirdWeather and Eclipse Soundscapes sensors on non-eclipse days.
   | Column name | Description |
   |-------------|-------------|
   | com | Common name |
   | sci | Scientific name according to BirdWeather |
 * [ritland_clean.csv](./Data/ritland_clean.csv) Ritland's eye size data. See [Ausprey 2021](https://royalsocietypublishing.org/doi/pdf/10.1098/rspb.2021.0853) for full details
 * [unique_locations.csv](./Data/unique_locations.csv) Table with unique sensor locations used in analysis (used for creating Fig. 1 maps)

### Figures 
Contains figure files, including PowerPoint files using to collate and annotate figure panels

### Helpers/Eclipse-Data-Tool-main
The [Eclipse Data Tool](https://github.com/ARISA-Lab-LLC), developed by [ARISA Lab](https://arisalab.org/), is a set of JavaScript files and an HTML page that allows the user to get data for a particular eclipse at different locations. We downloaded the GitHub repository for the software to extract eclipse contact times for all BirdWeather Stations using two contact time scripts: [Annular Contact Times](./Scripts/999_misc__birdweather_oct23_extract_contact_times.R) and [Total Contact Times](./Scripts/999_misc_birdweather_apr24_extract_contact_times.R). We stored the Eclipse Data Tool in our repository for reproducibility of our analysis, but recommend users download directly from [GitHub](https://github.com/ARISA-Lab-LLC). 

### Results
 * [annular_eclipse_results.RData](./Results/annular_eclipse_results.RData) Model objects from annular analysis, as well as formatted data used to fit models. Generated from [204_annular_analysis.R](./Scripts/204_annular_analysis.R).
 * [coverage_analysis_total_and_annular.RData](./Results/coverage_analysis_total_and_annular.RData) Model objects from coverage analysis, as well as formatted data used to fit models. Generated from [203_coverage_analysis.R](./Scripts/203_coverage_analysis.R).
 * [total_eclipse_maximum.RData](./Results/total_eclipse_maximum.RData) Model objects from total eclipse logistic regression, as well as formatted data used to fit models. Generated from [201_total_eclipse_logistic_regression.R](./Scripts/201_total_eclipse_logistic_regression.R).
 * [total_eclipse_vocalization_progression.RData](./Results/total_eclipse_vocalization_progression.RData) Model objects from total eclipse vocalization progression analysis, as well as formatted data used to fit models. Generated from [202_total_eclipse_vocalization_progression.R](./Scripts/202_total_eclipse_vocalization_progression.R).
 
### Scripts
 * [101_data-prep_load_birdnet.R](./Scripts/101_data-prep_load_birdnet.R). Script to load birdnet output from [Eclipse Soundscapes](https://eclipsesoundscapes.org) recordings. [BirdNET](https://birdnet.cornell.edu/) was ran locally and output files were stored in [Data/Birdnet](./Data/Birdnet). Once files are loaded, this script alculates morning onset and evening cessation from raw BirdWeather data downloads. This script writes[Birdnet_cleaned_v03.csv]('./Data/birdnet_cleaned_v03.csv), which is used in subsequent data prep scripts.
 * [101b_data-prep_load_birdnet.R](./Scripts/101_data-prep_load_birdnet.R). Helper script that is sourced within the primary load_birdnet script. This script loads Birdnet output files that have slightly different column names. Output from this script is binded with the data.frame of the primary load_birdnet script.
 * [102_data-prep_merge_es_and_bw.R](./Scripts/102_data-prep_merge_es_and_bw.R). Reads in the output from [101b_data-prep_load_birdnet.R](./Scripts/101_data-prep_load_birdnet.R) (Birdnet_cleaned_v03.csv). Reads in BirdWeather observations from North America during April 2024 and October 2023 and merges with the processed Eclipse Soundscapes Birdnet output. NOTE: raw BirdWeather data files were too large to include in this directory, but are available for download at: [BirdWeather Data Explorer](https://app.birdweather.com/data). This script writes an important output csv that is used in subsequent analysis scripts: [merged_es_bw_apr2025.csv](./Data/merged_es_bw_apr2025.csv).
 * [103_data-prep_extract_viirs.R](./Scripts/103_data-prep_extract_viirs.R). Annotates all birdnet detections with a nighttime_light level for use in subsequent analyses. Monthly cloud-free VIIRS Day Night Band data publicly available for download from the [Earth Observation Group](https://eogdata.mines.edu/products/vnl/). VIIRS data is not included in repository due to size. We downloaded every tile available from March 2023 - March 2024 from the Earth Observation Group. Example path to files: Home > nighttime_light > monthly > v10 > 2023 > 202301 > vcmslcfg. This is the final data prep script and output is ready for analysis in following script.
* [201_total_eclipse_logistic_regression.R](./Scripts/201_total_eclipse_logistic_regression.R). This script fits logistic regression models analyzing whether (1) or not (0) a species was detected during a 4-minute time bin centered on maximum eclipse time during the total solar eclipse and the same period on the preceding day, as well as trait models.
* [202_total_eclipse_vocalization_progression.R](./Scripts/202_total_eclipse_vocalization_progression.R). This script analyzes the progression or change in vocalization activity during the total solar eclipse.
* [203_coverage_analysis.R](./Scripts/203_coverage_analysis.R) This script fits vocalization progression models using data from all sensors & evaluates influence of eclipse coverage on bird responses. This analysis includes both eclipse types (total and annular).
* [204_annular_analysis.R](./Scripts/204_annular_analysis.R) This script fits logistic regression and vocalization progression models for the annular eclipse.
* [301_figure_01.R](./Scripts/301_figure_01.R). Creates and saves panels (b) and (c) for Figure 1 (maps).
* [302_figure_02.R](./Scripts/302_figure_02.R). Creates and saves Figure 2.
* [303_figure_03.R](./Scripts/303_figure_03.R). Creates and saves Figure 3.
* [304_figure_04.R](./Scripts/304_figure_04.R). Creates and saves Figure 4.
* [305_figure_s01.R](./Scripts/305_figure_s01.R). Creates and saves Figure S01.
* [306_table_s01.R](./Scripts/306_table_s01.R). Creates a species list for the supplement. 
* [999_misc__birdweather_oct23_extract_contact_times.R](./Scripts/999_misc__birdweather_oct23_extract_contact_times.R). Script used to run isolate and run birdweather detections during the 2023 annular eclipse through an eclipse contact time tool, based on lat/lon of deployment. Contact Time tool is available in [the Helpers folder](./Helpers/Eclipse-Data-Tool-main).
* [999_misc_birdweather_apr24_extract_contact_times.R](./Scripts/999_misc_birdweather_apr24_extract_contact_times.R). Script used to run isolate and run birdweather detections during the 2024 total eclipse through an eclipse contact time tool, based on lat/lon of deployment. Contact Time tool is available in [the Helpers folder](./Helpers/Eclipse-Data-Tool-main).

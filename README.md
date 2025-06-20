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

### Scripts
 * [101_data-prep_load_birdnet.R](./Scripts/101_data-prep_load_birdnet.R). Script to load birdnet output from [Eclipse Soundscapes](https://eclipsesoundscapes.org) recordings. [BirdNET](https://birdnet.cornell.edu/) was ran locally and output files were stored in [Data/Birdnet](./Data/Birdnet). Once files are loaded, this script alculates morning onset and evening cessation from raw BirdWeather data downloads. This script writes[Birdnet_cleaned_v03.csv]('./Data/birdnet_cleaned_v03.csv), which is used in subsequent data prep scripts.
 * [101b_data-prep_load_birdnet.R](./Scripts/101_data-prep_load_birdnet.R). Helper script that is sourced within the primary load_birdnet script. This script loads Birdnet output files that have slightly different column names. Output from this script is binded with the data.frame of the primary load_birdnet script.
 * [102_data-prep_merge_es_and_bw.R](./Scripts/102_data-prep_merge_es_and_bw.R). Reads in the output from [101b_data-prep_load_birdnet.R](./Scripts/101_data-prep_load_birdnet.R) (Birdnet_cleaned_v03.csv). Reads in BirdWeather observations from North America during April 2024 and October 2023 and merges with the processed Eclipse Soundscapes Birdnet output. NOTE: raw BirdWeather data files were too large to include in this directory, but are available for download at: [BirdWeather Data Explorer](https://app.birdweather.com/data). This script writes an important output csv that is used in subsequent analysis scripts: [merged_es_bw_apr2025.csv](./Data/merged_es_bw_apr2025.csv).
 * [103_data-prep_extract_viirs.R](./Scripts/103_data-prep_extract_viirs.R). Annotates all birdnet detections with a nighttime_light level for use in subsequent analyses. Monthly cloud-free VIIRS Day Night Band data publicly available for download from the [Earth Observation Group](https://eogdata.mines.edu/products/vnl/). VIIRS data is not included in repository due to size. We downloaded every tile available from March 2023 - March 2024 from the Earth Observation Group. Example path to files: Home > nighttime_light > monthly > v10 > 2023 > 202301 > vcmslcfg.

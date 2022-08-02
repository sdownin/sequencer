**SequenceR**  
==============

(*Under development*) User-Friendly Sequence Analysis in **R**

## Accessing the Software

| Platform | Software | Type | Version | Date | Stage |
| ------- | ------- | ------- | ------- | ------- | ------- |
| Web | [**SequenceR Web App**](https://sdowning.shinyapps.io/sequencer-webhost/) | link | 0.5.0 | 2022-08-02 | AOM2022 |
| Windows PC | [**SequenceR Portable App**](https://mailmissouri-my.sharepoint.com/:u:/g/personal/sdr8y_umsystem_edu/EecDeADx3GlNsa99lq2dojkBsYn_6sf0wW3hg5GIFquHFw) | download | 0.5.0 | 2022-08-02 | AOM2022 |
| Mac OS | Unsupported - Use Web App | - | - | - | AOM2022 |

##### Web App Getting Started

- Follow the link above and use the app online without download.

##### Download Getting Started

- Download zipped file
- Unzip, keeping all files inside same directory
- Run app by double-clicking: `sequencer.exe`


## Test Data

| Data Set | Data Table | Stage |
| ------- | ------- | ------- | 
| Verbal utterances | [**Sequence Alphabet**](https://mailmissouri-my.sharepoint.com/:x:/g/personal/sdr8y_umsystem_edu/ESc9Bot0z99LvYFHQGlnuDMBTycy6ZWdAKEa2Sws3gRc0g?e=oRTj9v) | AOM2022 |
| Verbal utterances | [**Substitution Cost Matrix**](https://mailmissouri-my.sharepoint.com/:x:/g/personal/sdr8y_umsystem_edu/ER_E904k2lpNvmmIshCX9JQBrCEZo9Rgiw2W9qK28eBJhA?e=xFej53) | AOM2022 |
| Verbal utterances | [**Sequence Data**](https://mailmissouri-my.sharepoint.com/:x:/g/personal/sdr8y_umsystem_edu/EbhyoKeY7FJPrsXAJtyLCCUB0MvZe6OaQ9HwyOytlr_WjQ?e=PjSD6I) | AOM2022 |


## Sequence Analysis Steps

### 1. Import Data

1. Alphabet
2. Substitution cost matrix
3. Sequence data

Example CSV files for formatting your input data are included in the `TestData/` directory in this repository. 

[![](/_img/readme_analysis_data_import.png "Import data files")](#1-import-data)

### 2. Select Measures

- Measures to compute
- Sequence distance cost settings

[![](/_img/readme_analysis_measures.png "Measures")](#2-select-measures)

### 3. Output Results

- Sequence measures summary
- Save results

[![](/_img/readme_analysis_outputs.png "Outputs")](#3-output-results)

### 4. Plots

- Render plots
- Save plot image

[![](/_img/readme_analysis_plots.png "Plots")](#4-plots)



<hr>

## Changelog

All notable changes to this project will be documented in this section.

#### [0.5.0] - 2022-08-02
```
Added
 (none)

Changed
 - Switched to electron.js framework (Chromium browser; runs on node.js; npm for package management; electron-packager for distributions)
 - Changes simplicity measure from HHI to [-1 * ShannonEntropy] 

Removed
 (none)
```

Development Versions (^0.5.0):
 - based on electron.js framework from the electron-R/Shiny project: https://github.com/COVAIL/electron-quick-start. See also: https://github.com/electron/electron-quick-start

Archived Versions (Deprecated): 
 - see https://github.com/sdownin/sequencer-proto for changelog of previous versions before framework changed to electron.js 


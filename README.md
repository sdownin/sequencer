**SequenceR**  
==============

(*Under development*) User-Friendly Sequence Analysis in **R**

## Accessing the Software

| Platform | Software | Type | Version | Date | Stage |
| ------- | ------- | ------- | ------- | ------- | ------- |
| Web | [**SequenceR Web App**](https://sdowning.shinyapps.io/sequencer-webhost/) | link | 0.5.1 | 2022-08-03 | AOM2023 |
| Windows PC | [**SequenceR Portable App**](https://1drv.ms/u/c/6890386d9c4a0eb1/EQ3TGrnA7xVBqfe0SIPu1kkBKUtsS9AvvS1yqf6XTSZ-TA?e=n4CZop) | download | 0.5.1 | 2022-08-03 | AOM2023 |
| Mac OS | Unsupported - Use Web App | - | - | - | AOM2023 |

##### Web App Getting Started

- Follow the link above and use the app online without download.

##### Download Getting Started

- Download zipped file
- Unzip, keeping all files inside same directory
- Run app by double-clicking: `sequencer.exe`


## Test Data

| # | Data Set | Files | Stage |
| --- | ------- | ------- | ------- | 
| 1 | [**Colored Blocks**](https://mailmissouri-my.sharepoint.com/:u:/g/personal/sdr8y_umsystem_edu/EagV8GzcFpBAkWAZoApGlmkBA2_aheoK0Sjjtk1pYQy33Q?e=xJ1fNd) | alphabet; substitution costs; sequences examples for grouping, motif, complexity, dissimilarity | AOM2023 |
| 2 | [**Verbal Utterances**](https://mailmissouri-my.sharepoint.com/:u:/g/personal/sdr8y_umsystem_edu/EZV8hfckQEVKs5l2XzrDsKcBsIdSF638ioAfZ6AeBgNnrg?e=Bm6qab) | alphabet, substitution costs, sequences | AOM2023 |



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

#### [0.5.1] - 2022-08-03
```
Added
 (none)

Changed
 - Bug fixes for nonuniform data inputs (missing values, actors without actions in a period, etc.). 
     (Note: missing values are not treated as a separate category. This does not yet support 
      different temporal bracketing, empty turns within a period, etc.) 

Removed
 (none)
```

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


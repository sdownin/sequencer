**SequenceR**  
==============

(*Under development*) User-Friendly Sequence Analysis in **R**

## Web App

| Stage | Platform | Version | Date | Download Link |
| --------- | --------- | --------- | --------- | --------- |
| AOM2022 | Web App | 0.5.0 | 2022-07-30 | [SequenceR v0.5.0 Web App](https://sdowning.shinyapps.io/sequencer-webhost/, 'SequenceR v0.5.0 Web App') |

## Download (portable app)

| Stage | Platform | Version | Date | Download Link |
| --------- | --------- | --------- | --------- | --------- |
| AOM2022 | Windows PC | 0.5.0 | 2022-07-30 | [SequenceR v0.5.0 Windows](https://mailmissouri-my.sharepoint.com/:u:/g/personal/sdr8y_umsystem_edu/EecDeADx3GlNsa99lq2dojkBsYn_6sf0wW3hg5GIFquHFw, 'SequenceR v0.5.0 Windows')  |
| AOM2022 | Mac OS | - | - | Currently unsupported. Use web app.  |

### Download Getting Started

- Download zipped file
- Unzip, keeping all files inside same directory
- Run app by double clicking: `sequencer.exe`


Archived (Deprecated) Versions: 
 - see https://github.com/sdownin/sequencer-proto 



### 1. Import Data

1. Alphabet
2. Substitution cost matrix
3. Sequence data

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

#### [0.5.0] - 2022-07-30
```
Added
 (none)

Changed
 - Switched to electron.js framework (Chromium browser; runs on node.js; npm for package management; electron-packager for distributions)
 - Changes simplicity measure from HHI to [-1 * ShannonEntropy] 

Removed
 (none)
```

Refer to previous versions' changelog at https://github.com/sdownin/sequencer-proto

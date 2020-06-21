**SequenceR**  
==============

(*Under Development*) User-Friendly Sequence Analysis in **R**

## Download

| Stage | Version | Date | Download Link |
| ----------- | ----------- | ----------- | ----------- |
| MVP | 0.4.1 | 2020-06-21 | [SequenceR v0.4.1 2020-06-21](https://drive.google.com/file/d/1FUttl4FQU_MxWfBPJFvKLJR4GXDN5ZoQ/view?usp=sharing, 'SequenceR v0.4.1') |

Archived
 - [v0.3.1 2019-12-04](https://drive.google.com/file/d/1xcJQGmt3eQMiv9LAodLvkmEsbiDc-75w/view?usp=sharing, 'SequenceR v0.3.1')
 - [v0.2.0 2019-11-12](https://drive.google.com/file/d/1FHvMK6yUAgqSjJkYDJlBiXKpNrMti1QF/view?usp=sharing, 'SequenceR v0.2.0')


## Getting Started

- Download zipped file
- Unzip, keeping all files inside same directory
- Run app by executing runfile: `run.vbs`

```
SequenceR
    |--GoogleChromePortable/
    |--logs/
    |    └--SequenceRlog.txt
    |--R-Portable/
    |--shiny/
    |    └--www/
    |        └--js/
    |            └--main.js
    |    |--analysisModule.R
    |    |--app.R
    |    |--appServer.R
    |    |--appUi.R
    |--TestData/
    |    |--test_alphabet.csv
    |    |--test_data.csv
    |    └--test_subcostmat.csv
    |--run.vbs
    └--runShinyApp.R
```

[![](/_img/tutorial_screenvid_analysis.gif "SequenceR")](#getting-started)

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

- Plots
- Save plot

[![](/_img/readme_analysis_plots.png "Plots")](#4-plots)



<hr>

## Changelog

All notable changes to this project will be documented in this section.


#### [0.4.1] - 2020-06-21
```
Added
 - Test data

Changed
 - Updated README
 - Moved archived SequenceR packages out of this reposity into Google Drive only

Removed
 (none)
```

#### [0.4.0] - 2020-06-20
```
Added
 - Sequence measures: predictability, grouping, motif
 - Plot image resize in app
 - Plot save
 - Progress spinner icons

Changed
 - Results save; fixed zip bug when saving only a subset of possible measures

Removed
 (none)
```


#### [0.3.0] - 2019-12-02
```
Added
 - Analysis outputs
 - Download link
 - Readme screenshots

Changed
 - Resources directories

Removed
 (none)
```


#### [0.2.0] - 2019-11-12
```
Added
 - Sequence analysis dependencies in R-Portable

Changed
 - App UI tabs

Removed
 (none)
```


#### [0.1.0] - 2019-11-01
```
Added
 - SequenceR main Shiny app
 - GoogleChromePortable
 - R-Portable

Changed
 (none)

Removed
 (none)
```

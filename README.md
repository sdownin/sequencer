**SequenceR**  
==============

User-Friendly Sequence Analysis in **R**

## Download

| Version | Release | Download Link |
| ----------- | ----------- | ----------- |
| 0.3.0 | 2019-12-02 | [SequenceR v0.3.0 2019-12-02](https://drive.google.com/file/d/1PkLfv5xoAXZXRkGILS47NLEKGJ9O6YAQ/view?usp=sharing, 'SequenceR v0.3.0') |


Archived Versions
 - [v0.2.0 2019-11-12](https://drive.google.com/file/d/1w94bUbP7NhzbSSg9DLZNqnQPIxvt7BIX/view?usp=sharing, 'SequenceR v0.2.0')


## Getting Started

- Download zipped file
- Unzip, keeping all files inside same directory
- Run app by executing runfile: `Run.vbs`

```
SequenceR
    |--GoogleChromePortable
    |--R-Portable
    |--shiny
    |    └--www/
    |        └--js/
    |            └--main.js
    |    |--app.R
    |    |--appServer.R
    |    |--appUi.R
    |--.gitignore
    |--README.md
    |--run.vbs
    └--runShinyApp.R
```

[![](/_img/tutorial_screenvid_analysis_v1.gif "SequenceR")](#getting-started)

### 1. Import Data

1. Alphabet
2. Substitution cost matrix
3. Sequence data

[![](/_img/readme_analysis_data_import.png "Import data files")](#1-import-data)

### 2. Select Measures

- Sequence distance cost settings
- Measures to compute

[![](/_img/readme_analysis_measures.png "Measures")](#2-select-measures)

### 3. Output Results and Plots

- Sequence summary outputs
- Plots

[![](/_img/readme_analysis_outputs.png "Outputs")](#3-output-results-and-plots)



<hr>

## Changelog

All notable changes to this project will be documented in this section.

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

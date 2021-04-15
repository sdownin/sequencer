**SequenceR**  
==============

(*Under Development*) User-Friendly Sequence Analysis in **R**

## Download

| Stage | Version | Date | Download Link |
| ----------- | ----------- | ----------- | ----------- |
| MVP | 0.4.2 | 2020-06-24 | [SequenceR v0.4.2 2020-06-24](https://drive.google.com/file/d/1FYmnKIbPVZwyWdWPWWI5TLa1u4dG407n/view?usp=sharing, 'SequenceR v0.4.2') |

Archived Minor Versions
 - [v0.3.1 2019-12-04](https://drive.google.com/file/d/1xcJQGmt3eQMiv9LAodLvkmEsbiDc-75w/view?usp=sharing, 'SequenceR v0.3.1')
 - [v0.2.0 2019-11-12](https://drive.google.com/file/d/1FHvMK6yUAgqSjJkYDJlBiXKpNrMti1QF/view?usp=sharing, 'SequenceR v0.2.0')


## Getting Started

- Download zipped file
- Unzip, keeping all files inside same directory
- Run app by executing runfile: `run.vbs`

```
SequenceR/
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

- Render plots
- Save plot image

[![](/_img/readme_analysis_plots.png "Plots")](#4-plots)



<hr>

## Changelog

All notable changes to this project will be documented in this section.

#### [0.4.3] - 2021-04-14
```
Added
 - File encoding selection for input files (alphabet, substitution cost matrix, sequence data)
 - Category column selection for sequence alphabet input

Changed
 - Substitute abbreviations for category (action) names: A,...,Z,A2,...,Z2,A3,...,Z3,...
 - Alphabet summary (data input step 1) to include sequence category abbreviations 

Removed
 (none)
```

#### [0.4.2] - 2020-06-24
```
Added
 (none)

Changed
 - Predictability measure multiplied by -1.0 (see Rindova, Ferrier, & Wiltbank, 2010)
   (note: grouping measure was already correct, multiplied by -1.0, in v0.4.1)

Removed
 (none)
```

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

#### [0.3.1] - 2019-12-04
```
Added
 - Sequence distance measure via optimal matching
 - Simplicity measure via HHI

Changed
 (none)

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

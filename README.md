# Wastewater modeling for Washington State using simulated data

**General disclaimer** This repository was created for use by CDC programs to collaborate on public health related projects in support of the [CDC mission](https://www.cdc.gov/about/organization/mission.htm).  GitHub is not hosted by the CDC, but is a third party website used by CDC and its partners to share information and collaborate on software. CDC use of GitHub does not imply an endorsement of any one particular service, product, or enterprise. 

## Overview

This repository serves as a walkthrough for using simulated data from Washington state with the CDC's [ww-inference](https://github.com/CDCgov/ww-inference-model) package.

There are worked examples and commented code in order to and format simulated data, provided by the Washington State Department of Health. The aim of this repository is to fit [ww-inference](https://github.com/CDCgov/ww-inference-model) models of wastewater concentration and hospital admissions data, which provides estimates of Rt, and forecast hospital admissions and wastewater concentration.

As this is specifically designed to work with simulated data provided by the Washington State Department of Health, there are numerous custom functions designed to streamline formatting, running, and analysis of model. This is done in order to assess the potential improvements to forecasting accuracy that the inclusion of  wastewater data may provide. We also assess the potential improvements to forecasting accuracy that spatial correlations between wastewater catchment areas may provide.

## Layout of repository

The repository is organised as follows:

![github_diagram](https://github.com/user-attachments/assets/1a490d1b-5f00-457c-8dd0-dce1c21aa43f)

Additional files and folders may be present, but we will focus on these folders, and the three R scripts (ending with .R) that are listed here.

### data and simulated

This folder is where all files that are imported into R should be stored. Within this folder there is a subfolder called *simulated*. The files usually contained within this folder have not been shared, but refer to the simulated data provided by the Washington State Department of Health. No real datasets, or datasets with identifiable information should be stored here, or uploaded to GitHub.

### R, scripts and functions

Here we store all of our R scripts, and custom functions that are required to clean, run and analyze our models. Self contained functions are stored in the *functions* folder and scripts for running through the data and functions are found within *scripts*.

Within the *scripts* folder, there are three .R files, `1_assess_nonspatial_state.R`, `2_assess_nonspatial_substate.R`, and `3_assess_spatial_state.R`.

* `1_assess_nonspatial_state.R`
  - This is the initial script that we run in order to assess the affect of including wastewater data on the accuracy of forecasts of wastewater concentration and hospital admissions.
  - The script makes sure the correct **ww-inference** package version has been installed, loads packages and data, compiles the [stan](https://mc-stan.org/) model which fits the data, and then runs our analysis with the custom function `WA_nonspatial_run()`.
  - There are numerous ways to customise and adapt `WA_nonspatial_run` which are specified in the function code comments. The current set up is that you will run the model on every site included in the data. Then we will randomly select 10 different windows of data, which are termed "runs" to get several different epidemic trajectories. Each window is 118 days, fitting the model to 90, and predicting to 28. The approach will then run through the 10 different windows, and assess the overall model fit across the different windows. Numerous csv files and image files will be output, which are further explained in the output section. **Important:** Please specify a unique and specific "savename" as this is used to create folders and outputs from each model run. 
* `2_assess_nonspatial_substate.R`
  - This runs the same function as above (`WA_nonspatial_run()`) but loops through each site individually.
* `3_assess_spatial_state.R`
  - Here we make sure the spatial-branch of the **ww-inference** package is installed. From here, we specify the model as in previous scripts, but use the function `WA_spatial_run()` to carry out our analysis. This function compares a model not using spatial information for the wastewater data, with two different spatial correlations ([exponential](https://en.wikipedia.org/wiki/Exponential_function) and a [Lewandowski-Kurowicka-Joe distribution (LKJ)](https://en.wikipedia.org/wiki/Lewandowski-Kurowicka-Joe_distribution)).

### output

Within the folder *output* there is two further folders, *full_data* and *summary*. When you run the `WA_nonspatial_run()` and `WA_spatial_run()` functions it will create a folder within each of these with the text you have specified in the `savename = ` argument. 

*full_data*

This folder contains all of the individual model runs that are created as you loop through each run. For every run you will create a series of files, these will include the name "hosp" if they related to hospitalization forecasts, or "ww" if they relate to wastewater forecasts. These folders are not included by default, as there is a lot of information here, and the summary information is likely more important to disseminate.

* _diagnostics.csv
  - This file contains a number of diagnostic tests to ensure the model ran succesfully. If any of these diagnostics are `TRUE`, it may indicate the model poorly fit. Please see [here](https://github.com/CDCgov/ww-inference-model/blob/main/R/model_diagnostics.R) for more detail.
* _modelscore.csv
  - This file contains a number of scoring variables to assess model fit. See [here](https://epiforecasts.io/scoringutils/index.html) for further detail.
* _rawpredictions.csv
  - These are the raw predictions of the model, this data is aggregated after all runs have been completed to assess model performance across multiple epidemic trajectories. **Note** you cannot open these files directly into excel, they have been encoded to reduce the filesize, however they will read into R normally.
* _site_correlation.csv
  - This will only be present in runs that use the `WA_spatial_run()` function but refer to the correlation in predictions found when using a spatial relationship between wastewater catchment sites.
* _timetaken.csv
  - This is the time taken to run the model.
* _correlations.jpg
  - A visual representation of the correlations found.
* _forecast.jpg
  - A graph comparing forecasts with the data. For the `WA_nonspatial_run()` outputs, this will compare the model using wastewater data with the model without wastewater data. For the outputs of `WA_spatial_run()` this will compare models without spatial information with those using an exponential relationship and those with a LKJ relationship.
  
*summary*

This folder contains the summary information for completed instances of `WA_nonspatial_run()` and `WA_spatial_run()`. Here the overall assessments of the different model types are made, and the results displayed in aggregate. The files are as follows

* key.csv
  - This file contains the specific sites investigated, the forecast and calibration times, the number of repeats, the savename and the start date of each temporal window.
* _forecasts.jpg
  - Visualizations of the wastewater and hospitalization forecasts. An individual plot for each run is present, and for wastewater an individual plot for each site as well.
* scoring_.csv
  - Scores from [scoringutils](https://epiforecasts.io/scoringutils/index.html) on the aggregate data.
* diagnostics.csv
  - Model diagnostics for each run.

## Notices and disclaimers

### Public Domain Standard Notice
This repository constitutes a work of the United States Government and is not
subject to domestic copyright protection under 17 USC § 105. This repository is in
the public domain within the United States, and copyright and related rights in
the work worldwide are waived through the [CC0 1.0 Universal public domain dedication](https://creativecommons.org/publicdomain/zero/1.0/).
All contributions to this repository will be released under the CC0 dedication. By
submitting a pull request you are agreeing to comply with this waiver of
copyright interest.

### License Standard Notice
The repository utilizes code licensed under the terms of the Apache Software
License and therefore is licensed under ASL v2 or later.

This source code in this repository is free: you can redistribute it and/or modify it under
the terms of the Apache Software License version 2, or (at your option) any
later version.

This source code in this repository is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
PARTICULAR PURPOSE. See the Apache Software License for more details.

You should have received a copy of the Apache Software License along with this
program. If not, see http://www.apache.org/licenses/LICENSE-2.0.html

The source code forked from other open source projects will inherit its license.

### Privacy Standard Notice
This repository contains only non-sensitive, publicly available data and
information. All material and community participation is covered by the
[Disclaimer](DISCLAIMER.md)
and [Code of Conduct](code-of-conduct.md).
For more information about CDC's privacy policy, please visit [http://www.cdc.gov/other/privacy.html](https://www.cdc.gov/other/privacy.html).

### Contributing Standard Notice
Anyone is encouraged to contribute to the repository by [forking](https://help.github.com/articles/fork-a-repo)
and submitting a pull request. (If you are new to GitHub, you might start with a
[basic tutorial](https://help.github.com/articles/set-up-git).) By contributing
to this project, you grant a world-wide, royalty-free, perpetual, irrevocable,
non-exclusive, transferable license to all users under the terms of the
[Apache Software License v2](http://www.apache.org/licenses/LICENSE-2.0.html) or
later.

All comments, messages, pull requests, and other submissions received through
CDC including this GitHub page may be subject to applicable federal law, including but not limited to the Federal Records Act, and may be archived. Learn more at [http://www.cdc.gov/other/privacy.html](http://www.cdc.gov/other/privacy.html).

### Records Management Standard Notice
This repository is not a source of government records, but is a copy to increase
collaboration and collaborative potential. All government records will be
published through the [CDC web site](http://www.cdc.gov).

## Additional Standard Notices
Please refer to [CDC's Template Repository](https://github.com/CDCgov/template) for more information about [contributing to this repository](https://github.com/CDCgov/template/blob/main/CONTRIBUTING.md), [public domain notices and disclaimers](https://github.com/CDCgov/template/blob/main/DISCLAIMER.md), and [code of conduct](https://github.com/CDCgov/template/blob/main/code-of-conduct.md).

<!-- badges: start -->
[![R build status](https://github.com/resplab/accept/workflows/R-CMD-check/badge.svg)](https://github.com/resplab/accept/actions)
<!-- badges: end -->
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/accept)](https://cran.r-project.org/package=accept)
[![metacran downloads](https://cranlogs.r-pkg.org/badges/accept)](https://cran.r-project.org/package=accept)
[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)

# accept
R package for the ACute COPD Exacerbation Prediction Tool (ACCEPT)

The function `accept()` provides predictions from the latest version of the `accept` prediction model. `accept1()` provides predictions of exacerbations for COPD patients per original published manuscript. `accept2()` is an updated version of ACCEPT that is fine tuned for improved predictions in patients who do not have a prior history of exacerbations. Please refer to the published papers for more information: 

Adibi A, Sin DD, Safari A, Jonhson KM, Aaron SD, FitzGerald JM, Sadatsafavi M. The Acute COPD Exacerbation Prediction Tool (ACCEPT): a modelling study. The Lancet Respiratory Medicine, 8(10), pp.1013-1021; [doi:10.1016/S2213-2600(19)30397-2](https://www.thelancet.com/journals/lanres/article/PIIS2213-2600%2819%2930397-2/fulltext)

Safari, A., Adibi, A., Sin, D.D., Lee, T.Y., Ho, J.K., Sadatsafavi, M. and IMPACT study team, 2022. ACCEPT 2· 0: Recalibrating and externally validating the Acute COPD exacerbation prediction tool (ACCEPT). EClinicalMedicine, 51, p.101574. [doi:10.1016/j.eclinm.2022.101574](http://doi/org/10.1016/j.eclinm.2022.101574)

The following animation explains the `accept` model in 90 seconds:

[![IMAGE ALT TEXT HERE](https://img.youtube.com/vi/UuGLN128Z3Y/0.jpg)](https://www.youtube.com/watch?v=UuGLN128Z3Y)

## Installation

The latest stable version can be downloaded from CRAN:  
`install.packages ('accept')`

Alternatively, you can download the latest development version from GitHub:

```
install.packages("remotes")
remotes::install_github("resplab/accept")
```

# Web App for ACCEPT 

ACCEPT is also available as web app, accessible at [http://resp.core.ubc.ca/ipress/accept](http://resp.core.ubc.ca/ipress/accept)

# ACCEPT in R

### Sample Data

To get started, there is an R data frame with the package of sample patient data. I have printed columns 1-13 and 14-19 separately because there isn't enough space:

```
library(accept)
samplePatients <- accept::samplePatients

```

### Exacerbation Prediction

To get a prediction for exacerbation rate, you will need to pass in a patient vector:

```
results <- accept(samplePatients[1,]) #accept uses the latest updated prediction model
print(t(results))
```

The **accept()** function returns a data frame with the patient data used for prediction, along with the predictions for different treatment options. 

To visualize the data, there is a graphing function called **plotExacerbations()**, which creates a Plotly bar graph. You have the option of selecting **probability** or **rate** for which prediction you want to see, and either **CI** or **PI** to select the confidence interval or prediction interval respectively.

```
plotExacerbations(results, type="probability", interval = "CI")
```

```
plotExacerbations(results, type="probability", interval = "PI")
```

```
plotExacerbations(results, type="rate", interval = "CI")
```

### Probability of N Exacerbations (Poisson)

We can also calculate the predicted number of exacerbations in a year:

```
patientResults = accept1(samplePatients[1,]) #accept uses the original prediction model
exacerbationsMatrix = predictCountProb(patientResults, n = 10, shortened = TRUE)
print(exacerbationsMatrix)
```

The shortened parameter groups the probabilities from 3-10 exacerbations into one category, "3 or more exacerbations." To see all n exacerbation probabilities:

```
exacerbationsMatrix = predictCountProb(patientResults, n = 10, shortened = FALSE)
print(exacerbationsMatrix)
```

To visualize the matrix as a heatmap, we can use the function **plotHeatMap**:

```
plotHeatMap(patientResults, shortened = FALSE)
```

## Cloud-based API Access 

The [Peer Models Network](https://www.peermodelsnetwork.com) allows users to access ACCEPT through the cloud. A MACRO-enabled Excel-file can be used to interact with the model and see the results. To download the PRISM Excel template file for ACCEPT, please refer to the [Peer Models Network model repository](https://models.peermodelsnetwork.com).

#### Python
```
import json
import requests
url = 'https://prism.peermodelsnetwork.com/route/accept/run'
headers = {'x-prism-auth-user': YOUR_API_KEY}
model_run = requests.post(url, headers=headers,
json = {"func":["prism_model_run"],"model_input":[{"ID": "10001","male": 1,"age": 57,"smoker": 0,"oxygen": 0,"statin": 0,"LAMA": 1,"LABA": 1,"ICS": 1,"FEV1": 51,"BMI": 18,"SGRQ": 63,"LastYrExacCount": 2,"LastYrSevExacCount": 1,"randomized_azithromycin": 0,"randomized_statin": 0,"randomized_LAMA": 0,"randomized_LABA": 0,"randomized_ICS": 0, "random_sampling_N" : 100,  "calculate_CIs" : "TRUE"}]})
print(model_run)
results = json.loads(model_run.text)
print(results)
```

#### Linux Bash

In Ubuntu, you can call the API with `curl`:

```
curl \
-X POST \
-H "x-prism-auth-user: REPLACE_WITH_API_KEY" \
-H "Content-Type: application/json" \
-d '{"func":["prism_model_run"],"model_input":[{"ID": "10001","male": 1,"age": 57,"smoker": 0,"oxygen": 0,"statin": 0,"LAMA": 1,"LABA": 1,"ICS": 1,"FEV1": 51,"BMI": 18,"SGRQ": 63,"LastYrExacCount": 2,"LastYrSevExacCount": 1,"randomized_azithromycin": 0,"randomized_statin": 0,"randomized_LAMA": 0,"randomized_LABA": 0,"randomized_ICS": 0, "random_sampling_N" : 100, 
"calculate_CIs" : "TRUE"}]}' \
https://prism.peermodelsnetwork.com/route/accept/run
```


## User Manual

An interactive user manual that describes the study, the web app, the API, and the R package is available [here](https://resplab.github.io/acceptManual/section-introduction.html).

## Citation

Please cite:

Adibi A, Sin DD, Safari A, Jonhson KM, Aaron SD, FitzGerald JM, Sadatsafavi M. The Acute COPD Exacerbation Prediction Tool (ACCEPT): a modelling study. The Lancet Respiratory Medicine. Published Online First 2020 March 13th; [doi:10.1016/S2213-2600(19)30397-2](https://www.thelancet.com/journals/lanres/article/PIIS2213-2600%2819%2930397-2/fulltext)

Safari, A., Adibi, A., Sin, D.D., Lee, T.Y., Ho, J.K., Sadatsafavi, M. and IMPACT study team, 2022. ACCEPT 2· 0: Recalibrating and externally validating the Acute COPD exacerbation prediction tool (ACCEPT). EClinicalMedicine, 51, p.101574. [doi:10.1016/j.eclinm.2022.101574](http://doi.org/10.1016/j.eclinm.2022.101574)

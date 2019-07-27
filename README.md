[![Build Status](https://travis-ci.org/resplab/accept.svg?branch=master)](https://travis-ci.org/resplab/accept)
[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)

# accept
R package for the ACute COPD Exacerbation Prediction Tool (ACCEPT)

Please refer to the published paper for more information: 

Amin Adibi, Don D Sin, Abdollah Safari, Kate M Johnson, Shawn Aaron, J Mark FitzGerald, Mohsen Sadatsafavi (2019). Development and External Validation of the Acute COPD Exacerbation Prediction Tool (ACCEPT). bioRxiv 651901; doi: [https://doi.org/10.1101/651901](https://doi.org/10.1101/651901)

## Installation

The latest stable version can be downloaded from CRAN:  
`install.packages ('accept')`

Alternatively, you can download the latest development version from GitHub:

```{r chunk-label, eval = FALSE, echo = TRUE}
install.packages("devtools")
devtools::install_github("resplab/accept")
```

# Web App for ACCEPT 

ACCEPT is also available as web app, accessible at [http://resp.core.ubc.ca/ipress/accept](http://resp.core.ubc.ca/ipress/accept)

# PRISM: ACCEPT on the Cloud

The [PRISM platform](https://prism.resp.core.ubc.ca) allows users to access ACCEPT through the cloud. A MACRO-enabled Excel-file can be used to interact with the model and see the results. To download the PRISM Excel template file for ACCEPT, please refer to the [PRISM model repository](http://resp.core.ubc.ca/ipress/prism)

# ACCEPT in R


## ACCEPT Prediction Functions

### Sample Data {-}

To get started, there is an R data frame with the package of sample patient data. I have printed columns 1-13 and 14-19 separately because there isn't enough space:

```{r sample-patients, eval = TRUE, echo = TRUE}
library(accept)
samplePatients = accept::samplePatients
print(samplePatients[,1:13])
print(samplePatients[,14:19])
```

```{r, eval=TRUE, echo=FALSE}
library(htmltools)
rawHTML <- paste(readLines("predictACCEPT-inputs.html"), collapse="\n")
HTML(rawHTML)
```

### Exacerbation Rate {-}

To get a prediction for exacerbation rate, you will need to pass in a patient vector:

```{r exacerbation-rate, eval = TRUE, echo = TRUE}
results <- predictACCEPT(samplePatients[1,])
print(t(results))
```

The **predictACCEPT()** function returns a data frame with the original patient data, along with the predictions for different treatment options. Here is a summary of the results: 

```{r, eval=TRUE, echo=FALSE}
library(htmltools)
rawHTML <- paste(readLines("predictACCEPT-outputs.html"), collapse="\n")
HTML(rawHTML)
```

To visualize the data, there is a graphing function called **plotExacerbations()**, which creates a Plotly bar graph. You have the option of selecting **probability** or **rate** for which prediction you want to see, and either **CI** or **PI** to select the confidence interval or prediction interval respectively.

```{r exacerbation-rate-2, eval = TRUE, echo = TRUE}
plotExacerbations(results, type="probability", interval = "CI")
```

```{r exacerbation-rate-3, eval = TRUE, echo = TRUE}
plotExacerbations(results, type="probability", interval = "PI")
```

```{r exacerbation-rate-4, eval = TRUE, echo = TRUE}
plotExacerbations(results, type="rate", interval = "CI")
```

### Probability of N Exacerbations (Poisson) {-}

We can also calculate the predicted number of exacerbations in a year:

```{r n-exacerbations-1, eval = TRUE, echo = TRUE}
patientResults = predictACCEPT(samplePatients[1,])
exacerbationsMatrix = predictCountProb(patientResults, n = 10, shortened = TRUE)
print(exacerbationsMatrix)
```

The shortened parameter groups the probabilities from 3-10 exacerbations into one category, "3 or more exacerbations." To see all n exacerbation probabilities:

```{r n-exacerbations-2, eval = TRUE, echo = TRUE}
exacerbationsMatrix = predictCountProb(patientResults, n = 10, shortened = FALSE)
print(exacerbationsMatrix)
```

To visualize the matrix as a heatmap, we can use the function **plotHeatMap**:

```{r n-exacerbations-3, eval = TRUE, echo = TRUE}
plotHeatMap(patientResults, shortened = FALSE)
```

## Citation

Please cite:

The manuscript is currently under peer-review. A preprint is available on bioRxiv
```Amin Adibi, Don D Sin, Abdollah Safari, Kate M Johnson, Shawn Aaron, J Mark FitzGerald, Mohsen Sadatsafavi (2019). Development and External Validation of the Acute COPD Exacerbation Prediction Tool (ACCEPT). bioRxiv 651901; doi:10.1101/651901```

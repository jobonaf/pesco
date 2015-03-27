pesco
====

Data fusion for air quality data

###### Authors
Lucia Paci, [Giovanni Bonafè](mailto:gbonafe@arpa.emr.it)

###### Description
PESCO stands for "Post-processing and Evaluation with Statistical methods of a Chemistry-transport-model Output". The package provides functions to perform data fusion for air quality data, correcting the output of a deterministic CTM with observed data, with a Trans-Gaussian Kriging approach.

###### Installation
You can install the package by downloading the tarball and (in R)
```
install.packages("/path/to/tarball/pesco_0.2.2.tar.gz", repos = NULL, type = "source")
```
Otherwise, you can install the latest version directly from GitHub
```
require(devtools)
install_github("jobonaf/pesco")
```
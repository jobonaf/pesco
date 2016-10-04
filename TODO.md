TODO
====

#### Datasets

- [x] observed PM10 (estra_qaria)
- [x] observed NO2 (estra_qaria)
- [x] CTM concentrations (NetCDF, one day)
- [x] emissions (ASCII table)
- [x] elevation (ASCII table)
- [x] population (ASCII table)

#### Essentials

- [x] read data
 - [ ] minor bug: obs.data in ArpaER format refers to final time of sampling, so daily average should be 01-24, not 00-23
- [x] prepare data
- [x] kriging
- [x] test the kriging
- [x] all-inclusive function to prepare all you need to process a day
- [x] build source package

#### Extras

- [ ] write the output of kriging to NetCDF
- [ ] cross-validation
- [ ] functions to manage annual runs
- [ ] functions to calculate annual indicators
- [ ] function to invalidate daily output

#### Documentation

* doc of functions:
 - [x] aqstat.functions.Rd
 - [x] date.functions.Rd
 - [x] time.functions.Rd
 - [x] char.functions.Rd
 - [x] geo.functions.Rd
 - [x] read.functions.Rd
 - [x] prepare.functions.Rd
 - [x] daily_synthesis.Rd
 - [x] boxcox.Rd
 - [x] kriging.Rd
 - [x] interpolate.Rd
 - [ ] crossval.Rd
- [x] doc of datasets
- [x] demo for reading data
- [x] demo for preparing data
- [x] demo for kriging
- [ ] demo for cross-validation
- [ ] references
- [ ] description of the theory
- [ ] complete Value sections
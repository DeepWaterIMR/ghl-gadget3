
# Gadget3 model for Northeast Atlantic Greenland halibut

**Institute of Marine Research, Norway** and **Marine and Freshwater
Research Institute, Iceland**

**Authors (alphabetic order): Butler Will, Elvarsson Bjarki,
Hallfredsson Elvar H., Howell Daniel, Vihtakari Mikko, Windsland
Kristin**

**Maintainer: <mikko.vihtakari@hi.no>**

------------------------------------------------------------------------

This is a GitHub repository for a [Gadget assessment
model](https://gadget-framework.github.io/gadget3/) benchmarked for
[Northeast Arctic Greenland
halibut](https://www.ices.dk/community/groups/Pages/AFWG.aspx) in 2023,
and to be further developed for JNR-AFWG. The
[model](https://github.com/deepWaterIMR/ghl-gadget3) and
[data](https://github.com/deepWaterIMR/ghl-gadget-data) are available on
GitHub.

The repository is private, which means that if you see it, you have been
given permission to edit the repository. Please contact the maintainer
if you have any questions.

# Model overview

Our model has been developed with transparency, reproducibility and
automation of data acquisition in mind. We use the new [gadget3
framework](https://gadget-framework.github.io/gadget3/) for R that is
currently under an active development. The model has four sub-stocks
(immature/mature, female/male) from 1980 using a single time-step per
year. There are five fishing fleets: TrawlNor, TrawlRus, OtherNor,
OtherRus and Internat. Internat shares selectivity parameters with
TrawlNor and OtherRus with OtherNor due to lack of sufficient length and
sex distribution data for these fleets. In addition, there are 4 survey
fleets (\`EggaN, EcoS, WinterS, and RussianS) all with separate
suitability parameters.

The model obtains data from the IMR database, Russian surveys and
landings as well as landings from other countries. All of these data are
compiled to a
[duckdb](https://cran.r-project.org/web/packages/duckdb/index.html)
database using the
[*mfdb*](https://cran.r-project.org/web/packages/mfdb/index.html)
package for R. The data are aggregated for the model and stored in the
`file.path(base_dir, "data")` folder. This means that a working model
can be run without an access to the mfdb database. The data are used to
define initial values for sub-stocks (`3 stocks.R`), survey indices and
fleets (`4 fleets.R`), which Gadget uses to iterate better fitting
alternatives using the provided data. The data are collected in the
`2-n.R` files. General model and species settings are defined in the
`1 settings.R` file. The `5 likelihood.R` file sets up the likelihood
components and `6 parameters.R` generates the model and defines model
parameters.

The most fundamental differences to the previous assessment model setup
are starting the model from 1980 instead of 1992, using only one time
step per year instead of four, and using exact catches in tonnes instead
of estimating catches through effort in likelihood. Further, data are no
longer allocated to females and males externally, but the model
considers sexual dimorphism internally through sub-stocks and likelihood
components. These chances make data flow more transparent and possible
to automise. The following sections explain model settings, fit, and
results.

# Organization of the repository

Use
[`_RUN_THE_MODEL.R`](https://github.com/DeepWaterIMR/ghl-gadget3/blob/master/_RUN_THE_MODEL.R)
to run the model. The scripts should automatically install required
packages for you. In order to run the model for first time, you’ll need
an [MFDB](https://cran.r-project.org/web/packages/mfdb/index.html)
database stored in the
[ghl-gadget-data](https://github.com/DeepWaterIMR/ghl-gadget-data)
repository. The easiest way to set up the model is to clone both
repositories (this and ghl-gadget-data) in the same root directory.

-   Custom functions required by the model are located in the **R**
    folder.
-   The **data** folder contains datasets used to start the model or as
    shortcuts for other options
-   The Gadget model runs are located in the folder specified by the
    `base_dir` object.
-   The **figures** sub-folder contains figures illustrating the data
    based into the model, as well as the model output diagnostics.
-   The **data** sub-folder contains all data required by the model. In
    order to acquire the data for the first time, you’ll need the
    [ghl-gadget-data](https://github.com/DeepWaterIMR/ghl-gadget-data)
    repository.
-   The **session** sub-folder contains copy of work space and
    .Rhistory.
-   The **scripts** sub-folder contains copy of R scripts used to
    generate the model.
-   The **optim** sub-folder is created when parameter optimization is
    run and contains optimised model parameters together with the
    optimised fit object (`optim_fit`), which is used to create the
    model diagnostic figures.
-   The **iterative_reweighting**, **jitter**, and **retro** sub-folders
    are created when iterative reweighting, jitter or retrospective
    analyses is run, respectively, and contains results of those
    processes (`run_iterative`, `run_jitter`, `run_retor`).
-   The projections folder is created when `_RUN_PROJECTIONS.R` is run.

# Model projections

Model projections can be run using a parameter fit object (`optim_fit`).
Projections are run using the `_RUN_PROJECTIONS.R` file and figures made
using `8 projection figures.R`. These scripts are still unfinished and
do not work as intended.

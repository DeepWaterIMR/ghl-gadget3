
# A standardized Gadget 3 model for Northeast Atlantic Greenland halibut

**Institute of Marine Research, Norway**

**Authors (alphabetic order): Elvarsson Bjarki, Hallfredsson Elvar H.,
Howell Daniel, Vihtakari Mikko, Vollen Tone, Windsland Kristin**

**Maintainer: <mikko.vihtakari@hi.no>**

------------------------------------------------------------------------

This is a GitHub repository for a standardized [Gadget assessment
model](https://gadget-framework.github.io/gadget3/) created for
Greenland halibut. The model structure can be used as a base for other
species. The aim of the project is to create a transparent model that is
compatible with the Icelandic Greenland halibut model for the ICES AFWG
benchmark for the species in 2023. The end goal of the process is to
define harvest control rules which may consider the ecosystem
interactions. The model is still under construction and does not work as
intended.

The repository is private, which means that if you see it, you have been
given permission to edit the repository. Please contact the maintainer
if you have any questions.

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
-   The Gadget model runs are located in the **model\_files** folder.
-   The **figures** folder contains figures illustrating the data based
    into the model
-   The **data** folder contains all data required by the model. In
    order to acquire the data for the first time, you’ll need the
    [ghl-gadget-data](https://github.com/DeepWaterIMR/ghl-gadget-data)
    repository.

The model works until the TMB step (line 97 in `_RUN_THE_MODEL.R`),
which does not compile due to unidentified bugs in the model.

# Model overview

We use the new [Gadget3
framework](https://gadget-framework.github.io/gadget3/) for R that is
currently under an active development. This means that gadget3 is not
feature complete and contains bugs in addition to the bugs in this
model.

The model obtains data from the IMR database, Russian surveys and
landings as well as landings from other countries. All of these data are
compiled to a
[duckdb](https://cran.r-project.org/web/packages/duckdb/index.html)
database using the
[*mfdb*](https://cran.r-project.org/web/packages/mfdb/index.html)
package for R. The data are aggregated for the model and stored in the
`model_files/data` folder. This means that a working model can be run
without an access to the mfdb database. The data are used to define
initial values for sub-stocks (`3 stocks.R`), survey indices and fleets
(`4 fleets.R`), which Gadget uses to iterate better fitting alternatives
using the provided data. The data are collected in the `2-n.R` files.

# Model structure

There are four sub-stocks, N fleets, and N survey time-series in the
model. In contrast to the current Gadget model for NEA Greenland
halibut, the data are not any longer allocated to females and males
externally but the model considers sexual dimorphism internally through
sub-stocks.

# Things to do before the benchmark

*Last edited: MV, 29.09.2021.* Comments in italics.

1.  Move to Gadget3.

    -   Done. The next step is to make the model to compile.

2.  Include age data.

    -   Done. The next step is to make the model to compile. After that
        the data need to be setup optimally.

3.  Move to a one year time-step. Structure the model so that different
    time-steps can be examined.

    -   Implemented but not done. In theory
        `model_params$timestep_fun <- mfdb::mfdb_timestep_yearly` in
        `1 settings.R` should do the trick but the model undoubedly
        contain bugs that need to be ironed out.

4.  Move to Bjarki’s data weighting scheme.

    -   Done. Compare the impacts on the model output once the model
        compiles

5.  Survey indices

    -   Redo all of them. See
        [strata-and-survey-indices](https://github.com/DeepWaterIMR/ghl-gadget-data/blob/main/docs/strata-and-survey-indices.Rmd)

6.  Estimate or exact catch in tonnes?

    -   Try first with exact

7.  Think again about the Russian survey do this one with lengthdist +
    biomass levels (but need to keep the option of also including
    biomass levels possible)

    -   Remove because the survey has been suspended?

8.  Fleet structure.

    -   New structure proposed in
        [landings-data](https://github.com/DeepWaterIMR/ghl-gadget-data/blob/main/docs/landings-data.Rmd).
        Implemented into the model although historical fleets probably
        contain bugs that need to be ironed out.

9.  M

    -   Splitted by sex. Need to be estimated externally

10. Let the model split to sex instead of us doing it

    -   Done.

11. Transparency, reproducibility and documentation

    -   Done. Improve the documentation.

12. Method compatibility with the WN Greenland halibut model

    -   Implemented.

13. Make the model extend further back in time.

    -   The model can be extended back to 1935 if needed. Define
        `model_params$year_range` in `1 settings.R` as an integer
        sequence and iron out the bugs.

14. Move to mature female SSB as the reporting measure instead of 45cm+

    -   Can be done with the new model structure

15. HCR

    -   Some time after the benchmark

16. Fix length-weight relationship

    -   Make a separate document which explains the decisions behing all
        life history parameters and initial condiitons. Split initial
        sigma by sex.

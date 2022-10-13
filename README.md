
# A standardized Gadget 3 model for Northeast Atlantic Greenland halibut

**Institute of Marine Research, Norway** and **Marine and Freshwater
Research Institute, Iceland**

**Authors (alphabetic order): Butler Will, Elvarsson Bjarki,
Hallfredsson Elvar H., Howell Daniel, Vihtakari Mikko, Windsland
Kristin**

**Maintainer: <mikko.vihtakari@hi.no>**

------------------------------------------------------------------------

This is a GitHub repository for a standardized [Gadget assessment
model](https://gadget-framework.github.io/gadget3/) created for
Northeast Arctic Greenland halibut. The model structure can be used as a
base for other species. The aim of the project is to create a
transparent model that is compatible with the Icelandic Greenland
halibut model for the ICES AFWG benchmark for the species in 2023. The
end goal of the process is to define harvest control rules which may
consider the ecosystem interactions. The model is still under
construction and does not work as intended.

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
-   The Gadget model runs are located in the **model_files** folder.
-   The **figures** folder contains figures illustrating the data based
    into the model, as well as the model output diagnostics.
-   The **data** folder contains all data required by the model. In
    order to acquire the data for the first time, you’ll need the
    [ghl-gadget-data](https://github.com/DeepWaterIMR/ghl-gadget-data)
    repository.

# Model overview

We use the new [gadget3
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
externally, when such information is not available, but the model
considers sexual dimorphism internally through sub-stocks.

# Things to do before the benchmark

*Last edited: MV, 13.10.2022.* Comments in italics.

1.  Move to Gadget3.

    -   Done.

2.  Include age data.

    -   Done.

3.  Move to a one year time-step. Structure the model so that different
    time-steps can be examined.

    -   Done. Use the `model_params$timestep_fun` element to change the
        time step.

4.  Move to Bjarki’s data weighting scheme.

    -   Done. Compare the impacts on the model output once the model
        compiles.

5.  Survey indices

    -   EggaN and BESS based juvenile index done. Potentially add:
        EcoSouth (\>35 cm, ldist, sex?).

6.  Estimate or exact catch in tonnes?

    -   Try first with exact

7.  Add Russian survey index

    -   Done

8.  Fleet structure.

    -   Done. New structure proposed in
        [landings-data](https://github.com/DeepWaterIMR/ghl-gadget-data/blob/main/docs/landings-data.Rmd)
        and implemented.

9.  Split M by sex (and maturity?)

    -   Done. Gadget now estimates M for mature males

10. Let the model split to sex instead of us doing it

    -   Done. Sex-split comes currently from `EggaN_mat`. Needs to be
        improved.

11. Transparency, reproducibility and documentation

    -   Done. Improve the documentation once the model is complete and
        running.

12. Method compatibility with the WN Greenland halibut model

    -   Done.

13. Make the model extend further back in time.

    -   Done. The model starts from 1980.

14. Move to mature female SSB as the reporting measure instead of 45cm+

    -   Done. female_mat is the SSB.

15. Fix length-weight relationship

    -   Make a separate document which explains the decisions behind all
        life history parameters and initial conditions. Split initial
        sigma by sex.

16. Add Russian catch and survey ldist with sex

    -   Done.

17. Add CPUE index

    -   So far not added because Russian SI reaches back to 1984.

18. Make the model realistic

    -   Not done. Need help here.

19. Reference points

20. Jitter

    -   Make the model realistic first.

21. Uncertainties

    -   Make the model stable and realistic first.

22. HCR

    -   Some time after the benchmark

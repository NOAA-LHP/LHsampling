<!-- README.md is generated from README.Rmd. Please edit that file -->

# LHsampling

An individual-based model (IBM) incorporating within-population
variability in von Bertalanffy growth, size-dependent natural mortality,
and a size-selective fishery to simulate an exploited fish population
and catch (harvest). A bootstrap algorithm allows the user to
investigate various sampling approaches including sampling strategy
(proportional or fixed otolith sampling, POS or FOS, respectively),
sample size, supplementation with fishery-independent sampling, and
assumptions regarding von Bertalanffy t0 and the relationship between
variance of length at age and age. A function to produce plots of the
bootstrap sampling results is also provided.

## Overview

Code to inform fishery dependent biological sampling from simulated fish
populations and resulting catch using an individual based model. For
additional details see Schemmel et al 2022
(<https://academic.oup.com/icesjms/article/79/5/1497/6581603>).

## Version Control Platform

-   Git

## License

See the [LICENSE.md](./LICENSE.md) for details

## Installation

Install the devtools package (if needed) and then use:

library(devtools)

devtools::install_github(“NOAA-LHP/LHsampling”)

library(LHsampling)

<!-- Do not edit below. This adds the Disclaimer and NMFS footer. -->

------------------------------------------------------------------------

## Disclaimer

The United States Department of Commerce (DOC) GitHub project code is
provided on an ‘as is’ basis and the user assumes responsibility for its
use. DOC has relinquished control of the information and no longer has
responsibility to protect the integrity, confidentiality, or
availability of the information. Any claims against the Department of
Commerce stemming from the use of its GitHub project will be governed by
all applicable Federal law. Any reference to specific commercial
products, processes, or services by service mark, trademark,
manufacturer, or otherwise, does not constitute or imply their
endorsement, recommendation or favoring by the Department of Commerce.
The Department of Commerce seal and logo, or the seal and logo of a DOC
bureau, shall not be used in any manner to imply endorsement of any
commercial product or activity by DOC or the United States Government.”

------------------------------------------------------------------------

<img src="https://raw.githubusercontent.com/nmfs-general-modeling-tools/nmfspalette/main/man/figures/noaa-fisheries-rgb-2line-horizontal-small.png" width="200" style="height: 75px !important;"  alt="NOAA Fisheries">

[U.S. Department of Commerce](https://www.commerce.gov/) \| [National
Oceanographic and Atmospheric Administration](https://www.noaa.gov) \|
[NOAA Fisheries](https://www.fisheries.noaa.gov/)

![](draft/assets/images/repo-banner.png)

# Kerswell et al. (2021; G3)

This repository provides all materials for the manuscript *Backarc Lithospheric Thickness and Serpentine Stability Control Slab-Mantle Coupling Depths in Subduction Zones* (Kerswell et al., 2021; G3). You can find the paper [here](https://buchanankerswell.com/assets/pdf/kerswell-et-al-coupling-g3-2021.pdf).

This repository includes:

- An R script to download all data required to compile the study
- R scripts to run the analyses and reproduce all results and figures
- A Makefile and run.sh script to easily compile the study
- The complete manuscript written in Rmarkdown

This repository is self-contained but requires the following software (all open-source).

## Prerequisite software

### R

This study is written in R. Follow the instructions at [R's homepage](https://www.r-project.org) to download and install the latest release of R on your machine.

## Running the study

Clone (or download) this repository:

```
# Clone this repository
git clone https://github.com/buchanankerswell/kerswell_et_al_coupling.git

# Change into the directory
cd kerswell_et_al_coupling

# Use Makefile to compile
make
```

This will check for required R packages and try to install missing packages automatically.

If all packages are found and available it will proceed to run the study. The study takes about 20 minutes to run on my MacBook Air (M1 8GB, 2020).

# Open Science Framework

This repository can also be found at the official [OSF repository](https://osf.io/zjac3/).

# Funding

This project was supported by the NSF grant OIA1545903 to M. Kohn, S. Penniston-Dorland, and M. Feineman

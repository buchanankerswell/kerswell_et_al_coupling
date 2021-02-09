# Supplementary material for Kerswell et al. (2020, G3)

This repository stores supplementary material for the manuscript *Backarc lithospheric thickness and serpentine stability control slab-mantle coupling depths in subduction zones* and includes:

- The complete dataset

- R scripts to reproduce all results

- R scripts to reproduce all figures

- A [shiny](https://shiny.rstudio.com) application for calculating lithospheric thickness and predicting coupling depth

I recommend cloning this repository. This will ensure that the scripts run without issue.

*Raw binary numerical model output is not required to reproduce results, but can be found [here](https://osf.io/zjac3/)*

## Reproducing results

### Install R

Head to the [R-Project](https://www.r-project.org) to download and install R

> R is a free software environment for statistical computing and graphics. It compiles and runs on a wide variety of UNIX platforms, Windows and MacOS.

### Quick way

Clone (or download) this repository:

```
git clone https://github.com/buchanankerswell/kerswell_et_al_coupling.git
```

In terminal (macOS) run:

```
cd path/to/kerswell_et_al_coupling/
Rscript compile.R
Rscript process.R
Rscript visualize.R
```

In windows command prompt run:

```
cd path\to\kerswell_et_al_coupling
"C:\Program Files\R\R-x.y.z\bin\Rscript.exe" C:\path\to\kerswell_et_al_coupling\compile.R
"C:\Program Files\R\R-x.y.z\bin\Rscript.exe" C:\path\to\kerswell_et_al_coupling\process.R
"C:\Program Files\R\R-x.y.z\bin\Rscript.exe" C:\path\to\kerswell_et_al_coupling\visualize.R
```

Replacing `\R-x.y.z\` with your version of R.

### Long way

If you wish to reproduce the results all the way from raw binary model output, clone this repository as above. Replace the data folder with the one found at [osf](https://osf.io/zjac3/), which also contains the raw I2VIS binary output. Then:

In terminal (macOS) run:

```
cd path/to/kerswell_et_al_coupling/
Rscript compile.R
Rscript process_binary.R
Rscript visualize.R
```

In windows command prompt run:

```
cd path\to\kerswell_et_al_coupling
"C:\Program Files\R\R-x.y.z\bin\Rscript.exe" C:\path\to\kerswell_et_al_coupling\compile.R
"C:\Program Files\R\R-x.y.z\bin\Rscript.exe" C:\path\to\kerswell_et_al_coupling\process_binary.R
"C:\Program Files\R\R-x.y.z\bin\Rscript.exe" C:\path\to\kerswell_et_al_coupling\visualize.R
```

Replacing `\R-x.y.z\` with your version of R.

### Tables

Pretty tables can be made if you have a [TeX Live](https://tug.org/texlive/) distribution installed on your machine. Uncomment the lines in the `process.R` script and run `Rscript process.R`. Otherwise, you can just view the tables in the `tables` directory, which are already there if you cloned this repository.

## Web App

There are a two options for running the application for predicting coupling depths:

1. From your command line run:

```
cd path/to/kerswell_et_al_coupling/coupling_app/
Rscript app.R

Listening on http://127.0.0.1:6247
```

Open a web browser and navigate to `http://127.0.0.1:6247`

2. [Run](https://kerswell.shinyapps.io/coupling_app/) the version hosted online

*There is a cap of 25hr of app runtime per month for the online version. If you are blocked this may be the case and you can run the app locally. This app is not intended for research purposes, but as a tool for exploring the concepts presented in the paper.*

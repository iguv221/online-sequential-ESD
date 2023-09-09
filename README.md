
# Automated Online Sequential ESD (R)

This package includes R codes for online sequential ESD(osESD), a variation of GESD tests.  
It is a statistical testing method for anomaly detection in univariate time series datasets.  
We provide osESD and an automated grid search method auto-osESD.  
Auto-osESD can be used to find the best parameters for a specific dataset,  
using parameters either provided explicitly or basic parameters if not provided.  
Original paper can be found in [LINK].  

## Installation
### 1. Clone repository.
Clone or download zip. file of our repository into local device.

### 2. Download dependencies
Download dependencies written in main.R . 
This can be easily done by running 'install.packages' of each library
```
install.packages('optparse')
install.packages('tcltk')
install.packages('caret')
```

Also, import sources needed in running code. 
This can be done with 'source'.
```
source("models//osESD_detectors.R")
source("models//osESD_Test.R")
source("models//osESD_Transform.R")
source("utils//functions.R")
```


## Versions
R = 4.3.1
Rstudio = 2022.12.0+353






## Example Usage

After cloning this repository and installing all dependencies, one can run our osESD method with the below code in R,  
with data_name being directory to dataset and result_directory being directory to where indices of anomalies be exported.

```
ex_osESD_anoms<-run_osESD(data=ex_data,size=size)
```

To run auto-osESD, the below code should be run.  

```
auto_osESD_anoms<-run_auto_osESD(data=ex_data,labeled=labeled,parameters=ex_opt,weights=ex_weights,min_max_switch = min_max_switch)
```

To change parameters and provide new ones, the below code should be modified and run.  

```
ex_opt_list <- list(
  make_option(c("--WindowSizes"), type="double", default=c(50,200)),
  make_option(c("--AndOr"), type="double", default=c(1,0)),
  make_option(c("--MaxRs"), type="double", default=c(3,20)),
  make_option(c("--Dwins"), type="double", default=c(2,30)),
  make_option(c("--Rwins"), type="double", default=c(4,30)),
  make_option(c("--Alphas"), type="double", default=c(0.001,0.05)))

ex_opt <- parse_args(OptionParser(option_list = ex_opt_list))

labeled<-TRUE
min_max_switch<-TRUE
auto_osESD_anoms<-run_auto_osESD(data=ex_data,labeled=labeled,parameters=ex_opt,weights=ex_weights,min_max_switch = min_max_switch)
```

Finally, if the dataset is unlabeled, then one should set '--labeled' to False.  
```
labeled<-FALSE
ex_auto_osESD_anoms_3<-run_auto_osESD(data=ex_data,labeled=labeled)
```




<!---
## Options

| Options Id | Description | Type | Default Value |
|-----|-----|-----|-----|
| vscodeRSupport | Install R packages to make vscode-R work. lsp means the `languageserver` package, full means lsp plus the `httpgd` package. | string | minimal |
| installDevTools | Install the `devtools` R package. | boolean | false |
| installREnv | Install the `renv` R package. | boolean | false |
| installRMarkdown | Install the `rmarkdown` R package. It is required for R Markdown or Quarto documentation. | boolean | false |
| installJupyterlab | Install and setup JupyterLab (via `python3 -m pip`). JupyterLab is a web-based interactive development environment for notebooks. | boolean | false |
| installRadian | Install radian (via `python3 -m pip`). radian is an R console with multiline editing and rich syntax highlight. | boolean | false |
| installVscDebugger | Install the `vscDebugger` R package from the GitHub repo. It is required for the VSCode-R-Debugger. | boolean | false |
| useTesting | For Debian, install packages from Debian testing. If false, the R package installed by apt may be out of date. | boolean | true |
| installBspm | Install and enable BSPM (Bridge to System Package Manager) to install R packages. This option is only working on Ubuntu now. | boolean | false |




## Customizations

### VS Code Extensions

- `REditorSupport.r`




## Supported platforms

`linux/amd64` platform `debian`, `ubuntu:focal` and `ubuntu:jammy`.

If the `useTesting` is `true`, `linux/arm64` platform `debian` also supported.











### Binary installation via apt

This feature will configure apt to install R and R packages.

Packages that can be installed via apt can be displayed with the following command.

```sh
apt-cache search "^r-.*" | sort
```

For example, the following command installs the `dplyr` package.

```sh
apt-get -y install --no-install-recommends r-cran-dplyr
```

Thanks to [r2u](https://eddelbuettel.github.io/r2u/), on Ubuntu,
all packages on CRAN and BioConductor can be installed via apt.

If you want to install R packages via apt during the container build phase,
you can use [the `ghcr.io/rocker-org/devcontainer-features/apt-packages` Feature](https://github.com/rocker-org/devcontainer-features/blob/main/src/apt-packages)
to do so.

```json
"features": {
    "ghcr.io/rocker-org/devcontainer-features/r-apt:latest": {},
    "ghcr.io/rocker-org/devcontainer-features/apt-packages:1": {
        "packages": "r-cran-curl"
    }
},
"overrideFeatureInstallOrder": [
    "ghcr.io/rocker-org/devcontainer-features/r-apt"
]
```

`ghcr.io/rocker-org/devcontainer-features/apt-packages` is not guaranteed to install after this Feature,
so be sure to set up [the `overrideFeatureInstallOrder` property](https://containers.dev/implementors/features/#overrideFeatureInstallOrder).

### Source installation via R

Packages that cannot be installed via apt must be installed using the R functions.

For more information, please check [the Rocker Project website](https://rocker-project.org/use/extending.html).


### Binary installation via R with bspm

If set the `installBspm` option to `true`, this Feature will install and set up
the [`bspm` R package](https://github.com/Enchufa2/bspm).

`bspm` provides functions to manage packages via the distribution's package manager.

Known limitation: `bspm` does not seem to work correctly on Debian.
(<https://github.com/rocker-org/devcontainer-features/pull/169#issuecomment-1665839740>)

## Python package installation

This feature has some options to install Python packages such as `jupyterlab`.
When installing Python packages, if `python3 -m pip` is not available, it will install `python3-pip` via apt.

This feature set `PIP_BREAK_SYSTEM_PACKAGES=1` when installing Python packages.

## References

- [Rocker Project](https://rocker-project.org)
---!>

---

_Note: This file was auto-generated from the [devcontainer-feature.json](https://github.com/rocker-org/devcontainer-features/blob/main/src/r-apt/devcontainer-feature.json).  Add additional notes to a `NOTES.md`._

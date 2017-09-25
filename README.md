# outbreakteachR

[![Build Status](https://travis-ci.org/OJWatson/outbreakteachR.png?branch=master)](https://travis-ci.org/OJWatson/outbreakteachR)

### What is this?

outbreakteachR is a package designed to assist in demonstration of analysis associated to "paper outbreak" teaching practical. It is a collection of functions designed to ease analaysis of the paper outbreak practical within R. This includes importing and cleaning data from an excel sheet, viewing the infection network, visualising the outbreak as an animation, and calculating the epidemiological parameters associated with the practical.

***
> To view the tutorial please click [here](https://cdn.rawgit.com/mrc-ide/outbreakteachR/b2d7e04/tutorials/outbreakteachR-package-tutorial.html)

***

In addition, there is an updated google form version of the infection form that went with the practical, which can be located [here](https://drive.google.com/open?id=0B0-wM-jL1G-Sb2NiRU1DbDJWZGs).

### Installing *outbreakteachR*

To install the development version from github the package [*devtools*](https://github.com/hadley/devtools) is required.

```r
install.packages("devtools")
library(devtools)
```
Once devtools is installed it is best to restart our R session. To do this either close RStudio or restart R (ctrl + shift + F10). Once your R session
has been restarted the package can be installed and loaded using:

```r
devtools::install_github("mrc-ide/outbreakteachR", ref="mrc-branch")
library(outbreakteachR)
```

***

#### Asking a question

For bug reports, feature requests, contributions, use github's [issue system.](https://github.com/mrc-ide/outbreakteachR/issues)

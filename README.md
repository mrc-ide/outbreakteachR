# outbreakteachR

[![Travis-CI Build Status](https://travis-ci.org/mrc-ide/outbreakteachR.png?branch=mrc-branch)](https://travis-ci.org/mrc-ide/outbreakteachR)


![]()<img src="img/logo.svg" width="50%">

***
> Now published in Epidemics - "Íde Cremin, Oliver Watson, Alastair Heffernan, Natsuko Imai, Norin Ahmed, Sandra Bivegete, Teresia Kimani, Demetris Kyriacou, Preveina Mahadevan, Rima Mustafa, Panagiota Pagoni, Marisa Sophiea, Charlie Whittaker, Leo Beacroft, Steven Riley, Matthew C. Fisher, An infectious way to teach students about outbreaks, Epidemics, Available online 9 December 2017, ISSN 1755-4365, https://doi.org/10.1016/j.epidem.2017.12.002" 
***

### What is this?

outbreakteachR is a package designed to assist in demonstration of analysis associated to "paper outbreak" teaching practical. It is a collection of functions designed to ease analaysis of the paper outbreak practical within R. This includes importing and cleaning data from an excel sheet, viewing the infection network, visualising the outbreak as an animation, and calculating the epidemiological parameters associated with the practical.

***
> To view the tutorial please click [here](https://htmlpreview.github.io/?https://github.com/mrc-ide/outbreakteachR/blob/mrc-branch/tutorials/package_tutorial/outbreakteachR-package-tutorial.html)

***

In addition, there is an updated google form version of the infection form that went with the practical, which can be located [here](https://drive.google.com/drive/folders/1-WcKZzIBRXn2vdwoaW-weJaQ48rIaQ3b?usp=sharing).

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

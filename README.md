
<!-- README.md is generated from README.Rmd. Please edit that file -->

# adodown

<!-- badges: start -->
<!-- badges: end -->

The goal of `adodown` is to provide Stata users with documentation
websites akin those offered to R users by `pkgdown`.

## Installation

You can install the development version of adodown like so:

``` r
if (!require("devtools")) install.packages("devtools")
devtools::install_gihub("arthur-shaw/adodown")
```

## Usage

With great power comes great responsibility.

Before adodown can perform its feats of heroics, the target Stata
package must use the following folder system:

    src
    \_ ado          # <- ado files
    \_ sthlp        # <- SMCL help files
    \_ mdhlp        # <- Markdown help files
    \_ vignettes    # <- Markdown vignettes
    README.md       # <- at the root of the package
    package.pkg     # <- can be anywhere; just needs to exist

With this structure in place, `{adodown}` can

- Convert Markdown files to Quarto
- Extract relevant information from the `.pkg` file

Once the folder system above is in place, `{adodown}` can build and
preview a site with a single command:

``` r
# provide paths
# - pkg_dir: where your Stata package is located; path must exist
# - site_dir: where you want your Quarto site; path need not exist yet
adodown::build_site(
  pkg_dir = "path/to/root/of/stata/package",
  site_dir = "desired/path/of/documentation/site"
)
```

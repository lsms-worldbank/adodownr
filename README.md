
<!-- README.md is generated from README.Rmd. Please edit that file -->

# adodownr

<!-- badges: start -->
<!-- badges: end -->

The goal of `adodownr` is to provide Stata users with documentation
websites akin those offered to R users by `pkgdown`.

## Installation

You can install the development version of adodownr like so:

``` r
if (!require("pak")) install.packages("pak")
pak::pak("arthur-shaw/adodownr")
```

## Usage

With great power comes great responsibility.

Before adodownr can perform its feats of heroics, the target Stata
package must use the following folder system. 
This folder system is set up automatically if using the Stata package
[adodown](https://github.com/lsms-worldbank/adodown).

```
    src/
      \_ ado/           # <- Folder with ado files
      \_ mdhlp/         # <- Folder with markdown help files
      \_ vignettes/     # <- Folder with markdown vignettes
      <packagename>.pkg # <- .pkg file in src/ folder
    README.md           # <- at the root of the package
```

With this structure in place, `{adodownr}` can

- Convert Markdown files to Quarto
- Extract relevant information from the `.pkg` file

Once the folder system above is in place, `{adodownr}` can build and
preview a site with a single command:

``` r
# provide paths
# - pkg_dir: where your Stata package is located; path must exist
# - site_dir: where you want your Quarto site; path need not exist yet
adodownr::build_site(
  pkg_dir = "path/to/root/of/stata/package",
  site_dir = "desired/path/of/documentation/site"
)
```

## Inspiration

This package draws heavy inspiration from two packages:

- [pkgdown](https://github.com/r-lib/pkgdown/), the best known R package
  for documentation websites
- [ecodown](https://github.com/edgararuiz/ecodown), an experimental
  effort to use Quarto for the same purpose

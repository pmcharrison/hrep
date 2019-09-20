
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Representing harmony with the hrep package

[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![Travis build
status](https://travis-ci.org/pmcharrison/hrep.svg?branch=master)](https://travis-ci.org/pmcharrison/hrep)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/pmcharrison/hrep?branch=master&svg=true)](https://ci.appveyor.com/project/pmcharrison/hrep)
[![Coverage
status](https://coveralls.io/repos/github/pmcharrison/hrep/badge.svg)](https://coveralls.io/r/pmcharrison/hrep?branch=master)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.2545770.svg)](https://doi.org/10.5281/zenodo.2545770)

The *hrep* R package provides utilities for representing and
manipulating chord sequences for perceptually informed harmony
modelling. The available representations include symbolic
representations, acoustic representations, and sensory representations.
Integer encodings are defined for several symbolic representations,
allowing the user to efficiently express chord sequences as sequences of
integers.

## Resources

  - [Function-level
    documentation](https://pmcharrison.github.io/hrep/reference/index.html)
  - [Changelog](https://pmcharrison.github.io/hrep/news/index.html)
  - [File an issue](https://github.com/pmcharrison/hrep/issues)
  - [Music corpora](https://github.com/pmcharrison/hcorp)
  - [Models of simultaneous
    consonance](https://github.com/pmcharrison/incon)
  - [Automatic voicing of chord
    sequences](https://github.com/pmcharrison/voicer)

## Installation

The *hrep* package may be installed from GitHub as follows:

``` r
if (!requireNamespace("devtools")) install.packages("devtools")
devtools::install_github("pmcharrison/hrep")
```

## Example usage

Chords may be defined as sequences of integers, with each integer
corresponding to a pitch or a pitch class. The following chord defines a
C major triad in first inversion:

``` r
library(hrep)
x <- pi_chord(c(52, 60, 67))
print(x)
#> Pitch chord: 52 60 67
```

From this symbolic representation, it is possible to derive various
acoustic and sensory representations, such as:

1)  A wave:

<!-- end list -->

``` r
plot(wave(x))
```

<img src="man/figures/README-unnamed-chunk-2-1.png" width="70%" />

2)  A sparse pitch spectrum:

<!-- end list -->

``` r
plot(sparse_pi_spectrum(x))
```

<img src="man/figures/README-unnamed-chunk-3-1.png" width="70%" />

3)  A sparse pitch-class spectrum:

<!-- end list -->

``` r
plot(sparse_pc_spectrum(x))
```

<img src="man/figures/README-unnamed-chunk-4-1.png" width="70%" />

4)  A smooth pitch-class spectrum:

<!-- end list -->

``` r
plot(smooth_pc_spectrum(x))
```

<img src="man/figures/README-unnamed-chunk-5-1.png" width="70%" />

Chords can be translated to various symbolic representations, which can
be encoded to integer formats. For example, here we convert the chord to
the pitch-class chord representation, and then encode it to an integer.

``` r
pc_chord((x))
#> Pitch-class chord: [4] 0 7
as.integer(encode(pc_chord(x)))
#> [1] 8210
```

Similarly, the following code expresses the chord as a pitch-class set,
and then encodes the pitch-class set as an integer.

``` r
pc_set(x)
#> Pitch-class set: 0 4 7
as.integer(encode(pc_set(x)))
#> [1] 145
```

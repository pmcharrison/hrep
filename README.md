
<!-- README.md is generated from README.Rmd. Please edit that file -->

# hrep - Harmony Representations

[![lifecycle](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![Travis build
status](https://travis-ci.org/pmcharrison/hrep.svg?branch=master)](https://travis-ci.org/pmcharrison/hrep)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/pmcharrison/hrep?branch=master&svg=true)](https://ci.appveyor.com/project/pmcharrison/hrep)
[![Coverage
status](https://coveralls.io/repos/github/pmcharrison/hrep/badge.svg)](https://coveralls.io/r/pmcharrison/hrep?branch=master)

`hrep` is an R package that provides methods for representing and
manipulating chord sequences. It is currently still under active
development, but we plan an official release in 2019.

## Development notes

  - The integer representation of a given representation must begin at 1
    and count upwards with no missing values. 0 can optionally be used
    to code missing chords (but this is not implemented here).

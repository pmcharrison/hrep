
<!-- README.md is generated from README.Rmd. Please edit that file -->

# hrep - Harmony Representations

[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)

`hrep` is an R package that provides methods for representing and
manipulating chord sequences. It is currently still under active
development, but we plan an official release in 2019.

## Development notes

  - The integer representation of a given representation must begin at 1
    and count upwards with no missing values. 0 can optionally be used
    to code missing chords (but this is not implemented here).

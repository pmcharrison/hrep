.onLoad <- function(libname, pkgname) {
  initialise_chord_qualities()

  packageStartupMessage(
    "Version 0.10.0.9001 of the hrep package redefined the ",
    "integer encodings for pc_chord, pc_chord_type, pc_set, and pc_set_type. ",
    "The packages hcorp, incon/corpdiss, and voicer have been updated ",
    "correspondingly. Please make sure that you have installed the latest ",
    "versions of these packages."
  )
}

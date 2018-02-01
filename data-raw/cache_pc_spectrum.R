dir <- "inst/extdata"
file <- "cache_convert_pc_set_to_pc_spectrum.rds"

R.utils::mkdirs(dir)

res <- cache_convert_pc_set_to_pc_spectrum()
saveRDS(res, file.path(dir, file), compress = "bzip2")

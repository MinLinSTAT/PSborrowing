## code to prepare `dat1` dataset goes here

path <- system.file(
    "extdata", "dat2.rda", package = "PSborrowing"
)

load(path)

usethis::use_data(dat2, overwrite = TRUE)

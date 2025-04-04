## code to prepare `dat1` dataset goes here

path <- system.file(
    "extdata", "dat1.rda", package = "PSborrowing"
)

load(path)

usethis::use_data(dat1, overwrite = TRUE)

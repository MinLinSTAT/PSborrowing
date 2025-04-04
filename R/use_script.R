#' Create or Open a Script in the `inst/scripts` Directory
#'
#' This function checks if the `/inst/scripts/` directory exists in the package.
#' If it doesn't, the function creates the directory. Then, it checks if a script
#' file with the specified name already exists in that directory. If the file exists,
#' it opens the file in RStudio. Otherwise, it creates a new file and opens it.
#'
#' @param filename A character string specifying the name of the script file (without
#'   the `.R` extension) to create or open within `inst/scripts/`.
#'
#' @return This function is called for its side effects and returns `NULL` invisibly.
#'   It creates or opens a file in the `inst/scripts/` directory, and if running in
#'   RStudio, opens it in the editor.
#'
#' @details
#' This function is intended to streamline script creation within a package structure,
#' making it easy to create or edit scripts in the `inst/scripts/` directory.
#' If `rstudioapi` is available and the function is called interactively in RStudio,
#' the script file will open automatically in the editor.
#'
#' @examples
#' \dontrun{
#' # Create or open a script named "my_script.R" in the inst/scripts directory
#' use_script("my_script")
#' }
#'
#' @importFrom rstudioapi navigateToFile isAvailable
#' @export
use_script <- function(filename) {
    # Define the path to the scripts directory and file
    scripts_dir <- file.path("inst", "scripts")
    file_path <- file.path(scripts_dir, paste0(filename, ".R"))

    # Check if the directory exists, create it if it doesn't
    if (!dir.exists(scripts_dir)) {
        dir.create(scripts_dir, recursive = TRUE)
        message("Created directory: ", scripts_dir)
    }

    # Check if the file exists, open it if it does, otherwise create it
    if (file.exists(file_path)) {
        message("Opening existing file: ", file_path)
    } else {
        file.create(file_path)
        message("Created new file: ", file_path)
    }

    # Open the file in RStudio if available
    if (interactive() && requireNamespace("rstudioapi", quietly = TRUE) &&
        rstudioapi::isAvailable()) {
        rstudioapi::navigateToFile(file_path)
    } else {
        message("File path: ", file_path)
    }
}

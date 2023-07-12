find_file_in_pkg <- function(
    pkg_dir,
    file_pattern
) {

    pkg_file <- fs::dir_ls(
        path = pkg_dir, 
        regexp = file_pattern,
        recurse = TRUE
    )

    return(pkg_file)

}

get_pkg_metadata <- function(pkg_file) {

    pkg_file <- readLines(
        pkg_file,
        warn = FALSE # suppress warning to avoid confusion
    )

    version <- stringr::str_subset(pkg_file, "^v")[1] |>
        stringr::str_replace("v (.+)", "\\1")
    name    <- stringr::str_subset(pkg_file, "^d")[1] |>
        stringr::str_replace("d (.+)", "\\1")
    title   <- stringr::str_subset(pkg_file, "^d")[2] |>
        stringr::str_replace("d (.+)", "\\1")
    desc    <- stringr::str_subset(pkg_file, "^d")[3] |>
        stringr::str_replace("d (.+)", "\\1")
    authors <- stringr::str_subset(pkg_file, "^d")[4] |>
        stringr::str_replace("d (.+)", "\\1")
    url     <- stringr::str_subset(pkg_file, "^d")[5] |>
        stringr::str_replace("d (.+)", "\\1")

    pkg <- list(
        version = version,
        name = name,
        desc = desc,
        authors = authors,
        url = url
    )

    return(pkg)

}

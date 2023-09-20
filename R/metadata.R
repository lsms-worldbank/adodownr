find_file_in_pkg <- function(
    dir,
    file_pattern,
    recurse = TRUE
) {

    pkg_file <- fs::dir_ls(
        path = dir,
        regexp = file_pattern,
        recurse = recurse
    )

    return(pkg_file)

}

get_pkg_metadata <- function(pkg_file) {

    pkg_lines <- readLines(
        pkg_file,
        warn = FALSE # suppress warning to avoid confusion
    )

    for (i in 1:length(pkg_lines)) {
        if ('*** version' %in% pkg_lines[i])     pkg_v   <- stringr::str_remove(pkg_lines[i+1],"v ")
        if ('*** stata' %in% pkg_lines[i])       stata_v <- stringr::str_remove(pkg_lines[i+1],"d Version: ")
        if ('*** name' %in% pkg_lines[i])        name    <- stringr::str_remove(pkg_lines[i+1],"d ")
        if ('*** date' %in% pkg_lines[i])        date    <- stringr::str_remove(pkg_lines[i+1],"d Distribution-Date: ")
        if ('*** description' %in% pkg_lines[i]) desc    <- stringr::str_remove(pkg_lines[i+1],"d ")
        if ('*** author' %in% pkg_lines[i])      author  <- stringr::str_remove(pkg_lines[i+1],"d Author:")
        if ('*** contact' %in% pkg_lines[i])     contact <- stringr::str_remove(pkg_lines[i+1],"d Contact: ")
        if ('*** url' %in% pkg_lines[i])         url     <- stringr::str_remove(pkg_lines[i+1],"d URL: ")
    }

    pkg <- list(
        pkg_v = pkg_v,
        stata_v = stata_v,
        name = name,
        date = date,
        desc = desc,
        author = author,
        contact = contact,
        url = url
    )

    return(pkg)
}

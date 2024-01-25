#' Find file in package file system.
#' 
#' @description 
#' Find file(s) in file system thaat match regex pattern.
#' 
#' @param dir Character. Directory that is root of search.
#' @param file_pattern Character. Regex pattern.
#' @param recursee Boolean. Whether or not to recurse into child dirs of `dir`.
#' 
#' @return Character. Vector of path(s)
#' 
#' @noRd 
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

#' Extract Stata package metadata relevant for site
#' 
#' @description 
#' Stata's `.pkg` file contains package metadata relevant for the site
#' (e.g. package version, author(s), repo URL, etc.). 
#' `adodown` provides predictable structure to that file that can facilitate
#' the process of extracting data.
#' 
#' @param pkg_file_path Character. Path to Stata's package file.
#' 
#' @return List. Compilation of package metadata:
#' 
#' - `pkg_version`. Version of the Stata package
#' - `name`. Name of the Stata package.
#' - `desc`. Description of the package.
#' - `stata_version`. Version of Stata required for package use.
#' - `author`.  Author.
#' - `contact`. Contact.
#' - `url`. URL of repository where package source code stored.
#' - `date`.  Date current Stata package version released.
#' 
#' @noRd 
get_pkg_metadata <- function(pkg_file_path) {

    pkg_lines <- readLines(
        con = pkg_file_path,
        warn = FALSE # suppress warning to avoid confusion
    )

    # TODO: use `metadata_exists()` to check that all metadata are present,
    # and to fail gracefully if not.
    # TO DISCUSS: whether to require all fields that the code below tries to
    # extract. Note: code returns "" for missing metadata entries.

    # extract metadata, moving from top to bottom of `.pkg` file
    pkg_version <- extract_metadata(
        pkg_lines = pkg_lines,
        header_text = "*** version", 
        entry_prefix = "v "
    )
    name <- extract_metadata(
        pkg_lines = pkg_lines,
        header_text = "*** name",
        entry_prefix = "d "
    )
    desc <- extract_metadata(
        pkg_lines = pkg_lines,
        header_text = "*** description", 
        entry_prefix = "d "
    )
    stata_version <- extract_metadata(
        pkg_lines = pkg_lines,
        header_text = "*** stata", 
        entry_prefix = "d Version: "
    )
    author <- extract_metadata(
        pkg_lines = pkg_lines,
        header_text = "*** author", 
        entry_prefix = "d Author: "
    )
    contact <- extract_metadata(
        pkg_lines = pkg_lines,
        header_text = "*** contact", 
        entry_prefix = "d Contact: "
    )
    url <- extract_metadata(
        pkg_lines = pkg_lines,
        header_text = "*** url", 
        entry_prefix = "d URL: "
    )
    date <- extract_metadata(
        pkg_lines = pkg_lines,
        header_text = "*** date", 
        entry_prefix = "d Distribution-Date: "
    )

    # compile metadata in list
    pkg <- list(
        pkg_version = pkg_version,
        name = name,
        desc = desc,
        stata_version = stata_version,
        author = author,
        contact = contact,
        url = url,
        date = date
    )

    return(pkg)
}

#' Check that a particular metadata entry exists
#' 
#' @param pkg_file_path Character. Path to Stata's `.pkg` file
#' @param entry Character. Name of the entry to check
#' 
#' @return Boolean. Whether the entry exists or not in the `.pkg` file
#' 
#' @noRd
metadata_exists <- function(
    pkg_file_path,
    entry
) {

    pkg_lines <- readLines(
        con = pkg_file_path,
        warn = FALSE # suppress warning to avoid confusion
    )

    check_entry_exists <- function(
        pkg_lines,
        header_text,
        entry_prefix
    ) {

        # find location of header
        header_loc <- grep(x = pkg_lines, pattern = header_text, fixed = TRUE)

        # say header exists if location is not an empty integer vector
        header_exists <- !identical(header_loc, integer(0))

        # say entry exists if header exists and if pattern found at next line
        entry_exists <- ifelse(
            test = header_exists == TRUE,
            yes = grepl(
                x = pkg_lines[header_loc + 1], 
                pattern = entry_prefix, 
                fixed = TRUE
            ),
            no = FALSE
        )

        return(entry_exists)

    }

    if (entry == "pkg_version") {

        entry_exists <- check_entry_exists(
            pkg_lines = pkg_lines,
            header_text = "*** version",
            entry_prefix = "v .+"
        )

    } else if (entry == "name") {

        entry_exists <- check_entry_exists(
            pkg_lines = pkg_lines,
            header_text = "*** name",
            entry_prefix = "d .+"
        )

    } else if (entry == "stata_version") {

        entry_exists <- check_entry_exists(
            pkg_lines = pkg_lines,
            header_text = "*** stata",
            entry_prefix = "d Version: Stata .+"
        )

    } else if (entry == "author") {

        entry_exists <- check_entry_exists(
            pkg_lines = pkg_lines,
            header_text = "*** author",
            entry_prefix = "d Author: .+"
        )

    } else if (entry == "contact") {

        entry_exists <- check_entry_exists(
            pkg_lines = pkg_lines,
            header_text = "*** contact",
            entry_prefix = "d Contact: .+"
        )

    } else if (entry == "url") {

        entry_exists <- check_entry_exists(
            pkg_lines = pkg_lines,
            header_text = "*** url",
            entry_prefix = "d URL: .+"
        )

    }

    return(entry_exists)

}

#' Extract metadata entries in `.pkg` file
#' 
#' @param header_text Character. Complete header for a metadata entry.
#' @param entry_prefix Character. String that comes immediately before the entry.
#' 
#' @return Character. Metadata entry represented as a character vector.
#' 
#' @importFrom stringr str_remove fixed
#' 
#' @noRd
extract_metadata <- function(
    pkg_lines,
    header_text,
    entry_prefix
) {

    # find location of header
    header_loc <- grep(x = pkg_lines, pattern = header_text, fixed = TRUE)

    # say header exists if location is not an empty integer vector
    header_exists <- !identical(header_loc, integer(0))

    # say entry exists if header exists and if pattern found at next line
    entry_exists <- ifelse(
        test = header_exists == TRUE,
        yes = grep(x = pkg_lines[header_loc + 1], pattern = entry_prefix),
        no = FALSE
    )

    metadata_entry <- ifelse(
        test = header_exists == TRUE & entry_exists == TRUE,
        yes = stringr::str_remove(
            string = pkg_lines[header_loc + 1],
            pattern = stringr::fixed(entry_prefix)
        )
    )

    return(metadata_entry)

}

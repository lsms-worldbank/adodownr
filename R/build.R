#' Build Quarto files from Markdown
#'
#' @description
#' Build content files such reference or vignette, converting from
#' Markdown to Quarto
#'
#' @param dir_in Character. Source package directory where files are located.
#' @param exclude Character. Files to exclude from building process. 
#' Specified as a regular expression.
#' @param dir_out Character. Target site directory where files are written.
#'
#' @importFrom fs dir_ls
#' @importFrom purrr walk
#'
#' @return None. Side-effect of writing several files to disk.
#'
#' @export
build_files <- function(
    dir_in,
    exclude = NULL,
    dir_out
) {

    # collect list of all Markdown files
    files <- fs::dir_ls(path = dir_in, regexp = "\\.md$")
    
    # exclude files, if specified
    if (!is.null(exclude)) {
        files <- grep(
            x = files,
            pattern = exclude,
            invert = TRUE,
            value = TRUE
        )
    }

    purrr::walk(
        .x = files,
        .f = ~ convert_md_to_qmd(
            path_in = .x,
            dir_out = dir_out
        )
    )

}

#' Get short description of command from help file
#'
#' @description
#' The command short description has the following format:
#' `commandname - This command is used for short description.`
#'
#' @param file Character. Path to source help file
#'
#' @importFrom dplyr filter
#' @importFrom stringr str_detect
#'
#' @return Character. Short description of command from help file title
get_cmd_short_desc <- function(file) {

    doc <- readLines(
        file,
        warn = FALSE # suppress warnings to avoid confusion
    )

    title_heading <- which(
        doc == "Title" | doc == "# Title" | doc == "## Title"
    )
    syntax_heading <- which(
        doc == "Syntax" | doc == "# Syntax" | doc == "## Syntax"
    )

    title_area <- data.frame(chr = doc[title_heading[1]:syntax_heading[1]])
    cmd_short_desc <- title_area |>
        dplyr::filter(
            !stringr::str_detect(.data$chr, "^Title|^#{1,2} Title") &
            !stringr::str_detect(.data$chr, "^Syntax|^#{1,2} Syntax") &
            !stringr::str_detect(.data$chr, "^==+") &
            .data$chr != ""
        ) |>
        dplyr::pull() |>
        stringr::str_replace(".+- (.+)", "\\1")

    return(cmd_short_desc)

}

#' Build Quarto site from Stata package files
#'
#' @description
#' This single function performs several actions:
#'
#' - Creates folders for the Quarto site
#' - Converts Markdown files to Quarto format, using some opinionated methods
#' - Builds a command reference index, creating a table of functions, links,
#' and descriptions
#' - Copies the package logo over
#' - Composes and writes a Quarto YAML file, implementing an opinionated
#' layout and feeding forward some package and assets into relevant
#' places
#' - Opens a preview of the Quarto documentation website
#'
#' @param pkg_dir Charcter. Directory of the source package.
#' @param site_dir Charcter. Directory of the target site.
#' @param rm_old_site_dir Boolean. If `TRUE`, delete old site. Otherwise, keep
#' but overwrite same-named files.
#'
#' @importFrom fs file_move path file_exists
#' @importFrom quarto quarto_path quarto_preview
#'
#' @export
build_site <- function(
    pkg_dir,
    site_dir,
    rm_old_site_dir = FALSE
) {

    # TODO: check that pkg has expected folder structure

    # create folders
    create_folders(site_dir, rm_old_site_dir = rm_old_site_dir)

    # create index from README
    convert_readme_to_index(
        pkg_dir = pkg_dir,
        site_dir = site_dir
    )

    # build news
    news_path <- fs::path(pkg_dir, "news.md")
    news_exists <- fs::file_exists(news_path)
    if (news_exists) {
        convert_md_to_qmd(
            path_in = news_path,
            dir_out = site_dir
        )
    }

    # build reference files
    pkg_ref_dir <- fs::path(pkg_dir, "src", "mdhlp")
    site_ref_dir <- fs::path(site_dir, "reference")
    build_files(
        dir_in = pkg_ref_dir,
        exclude = "README.md",
        dir_out = site_ref_dir
    )

    # determine whether YAML schema for index page is provided
    has_yaml_schema <- fs::file_exists(
        path = fs::path(
            pkg_dir, "src", "dev", "assets", "reference_index.yml"
        )
    )

    # build reference index
    # if YAML schema available, build a custom index page using the schema
    if (has_yaml_schema == TRUE) {

        build_custom_reference_index(
            dir_in = pkg_dir,
            dir_out = site_ref_dir
        )

    # otherwise, build the default index page
    } else {

        build_reference_index(
            dir_in = pkg_ref_dir,
            exclude = "README.md",
            dir_out = site_ref_dir
        )

    }

    # build vignettes
    pkg_vig_dir <- fs::path(pkg_dir, "src", "vignettes")
    site_vig_dir <- fs::path(site_dir, "articles")
    if (fs::dir_exists(pkg_vig_dir)) {
        build_files(
            dir_in = pkg_vig_dir,
            exclude = "README.md",
            dir_out = site_vig_dir
        )
    }

    # copy logo over
    pkg_logo <- find_file_in_pkg(
        dir = fs::path(pkg_dir, "src"),
        file_pattern = "logo.png"
    )
    pkg_logo_exists <- length(pkg_logo) >= 1
    if (pkg_logo_exists == TRUE) {

        fs::file_copy(
            path = pkg_logo,
            new_path = fs::path(site_dir, "images", "logo.png"),
            overwrite = TRUE
        )

    }

    # create YAML
    create_quarto_yaml(
        pkg_dir = pkg_dir,
        site_dir = site_dir,
        pkg_logo_exists = pkg_logo_exists
    )

    # TODO: find problem; fix it
    # Remove strange / in the first line of quarto.yml
    quarto_yaml_path <- fs::path(site_dir, "_quarto.yml")
    quarto_yaml <- readLines(
        quarto_yaml_path,
        warn = FALSE
    )
    quarto_yaml <- quarto_yaml[-1]
    writeLines(
        text = quarto_yaml,
        con = quarto_yaml_path
    )

    # check that Quarto installed
    quarto_installed <- !is.null(quarto::quarto_path())

    # preview site
    if (quarto_installed) {
        quarto::quarto_preview(file = fs::path(site_dir, "index.qmd"))
    } else {
        warning("Quarto not installed.")
    }

}

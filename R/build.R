#' Build Quarto files from Markdown
#'
#' @description
#' Build content files such reference or vignette, converting from
#' Markdown to Quarto
#'
#' @param dir_in Character. Source package directory where files are located.
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
    dir_out
) {

    files <- fs::dir_ls(path = dir_in, regexp = "\\.md$")

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
            !stringr::str_detect(chr, "^Title|^#{1,2} Title") &
            !stringr::str_detect(chr, "^Syntax|^#{1,2} Syntax") &
            !stringr::str_detect(chr, "^==+") &
            chr != ""
        ) |>
        dplyr::pull() |>
        stringr::str_replace(".+- (.+)", "\\1")

    return(cmd_short_desc)

}

#' Build reference index from files in source package help folder
#'
#' @param dir_in Character. Help/reference file folder of source package.
#' @param dir_out Character. Reference foler of target documentation site.
#'
#' @importFrom fs dir_ls path_file path_ext_remove path
#' @importFrom purrr map_chr pmap_chr
#' @importFrom glue glue
#'
#' @return None. Side-effect of writing a file to disk
#'
#' @export
build_reference_index <- function(dir_in, dir_out) {

    help_pkg_paths <- fs::dir_ls(path = dir_in, regexp = "\\.md$")
    help_names <- help_pkg_paths |>
        fs::path_file() |>
        fs::path_ext_remove()
    help_description <- purrr::map_chr(
        .x = help_pkg_paths,
        .f = ~ get_cmd_short_desc(file = .x)
    )
    help_site_paths <- purrr::map_chr(
        .x = help_names,
        .f = ~ fs::path("/reference", paste0(.x, ".qmd"))
    )

    tbl_header <- c(
        "Functions | Description",
        "|---|---|"
    )

    tbl_body <- purrr::pmap_chr(
        .l = list(
            func = as.list(help_names),
            path = as.list(help_site_paths),
            desc = as.list(help_description)
        ),
        .f = ~ glue::glue("|[{..1}]({..2})|{..3}|")
    )

    tbl <- c(tbl_header, tbl_body)

    index_content <- c(
        "---",
        "title: Function reference",
        "---",
        "",
        tbl
    )

    writeLines(
        text = index_content,
        con = fs::path(dir_out, "index.qmd")
    )

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
    create_folders(site_dir,rm_old_site_dir)

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
        dir_out = site_ref_dir
    )

    # build reference index
    build_reference_index(
        dir_in = pkg_ref_dir,
        dir_out = site_ref_dir
    )

    # build vignettes
    pkg_vig_dir <- fs::path(pkg_dir, "src", "vignettes")
    site_vig_dir <- fs::path(site_dir, "articles")
    if (fs::dir_exists(pkg_vig_dir)) {
        build_files(
            dir_in = pkg_vig_dir,
            dir_out = site_vig_dir
        )
    }

    # copy logo over
    pkg_logo <- find_file_in_pkg(
        dir = pkg_dir,
        file_pattern = "logo.png"
    )
    fs::file_copy(
        path = pkg_logo,
        new_path = fs::path(site_dir, "images", "logo.png"),
        overwrite = TRUE
    )

    # create YAML
    create_quarto_yaml(
        pkg_dir = pkg_dir,
        site_dir = site_dir
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

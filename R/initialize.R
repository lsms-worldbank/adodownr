#' Create Quarto site folders
#'
#' Create folders for site, reference, articles, and images (e.g., logos)
#'
#' @param site_dir Charcter. Directory of the target site.
#' @param rm_old_site_dir Boolean. If `TRUE`, delete old site. Otherwise, keep.
#'
#' @importFrom fs dir_create dir_exists dir_ls dir_delete
create_folders <- function(
    site_dir, 
    rm_old_site_dir = FALSE
) {

    # delete site directory from prior run
    # if it already exist and
    # if instructed to do so
    site_dir_exists <- fs::dir_exists(site_dir)
    if (site_dir_exists == TRUE & rm_old_site_dir == TRUE) {
        fs::dir_delete(site_dir)
    }

    # Make sure the top folder is created
    fs::dir_create(site_dir)

    # reference
    fs::dir_create(fs::path(site_dir, "reference"))

    # articles
    fs::dir_create(fs::path(site_dir, "articles"))

    # images
    fs::dir_create(fs::path(site_dir, "images"))

}

#' Compose YAML for Quarto package website
#'
#' @param pkg_dir Character. Path to root of target path
#' @param pkg_logo_exists Boolean. Whether package hex image exists
#' (or can be found).
#' 
#' @return Character. Quarto YAML
#'
#' @importFrom fs path dir_ls file_exists
#' @importFrom yaml as.yaml
#' @importFrom stringr str_subset
compose_quarto_yaml <- function(
    pkg_dir,
    pkg_logo_exists
) {

    pkg_file <- find_file_in_pkg(
        dir = fs::path(pkg_dir, "src"),
        file_pattern = "\\.pkg$",
        recurse = FALSE
    )

    pkg <- get_pkg_metadata(pkg_file_path = pkg_file)

    # pkg_logo <- find_file_in_pkg(
    #     dir = pkg_dir,
    #     file_pattern = "logo.png"
    # )
    # TODO:
    # - make URI programmatically determined
    # - have image parts of specification programmatically included
    pkg_logo <- "images/logo.png"

    spec <- list(
        project = list(
            type = "website",
            `output-dir` = "docs"
        ),
        website = list(
            title = pkg$name,
            favicon = pkg_logo,
            `page-navigation` = TRUE,
            navbar = list(
                background = "primary",
                search = TRUE,
                logo = pkg_logo,
                left = list(
                    # specify either as a list of lists or as a data frame
                    # see here: https://github.com/vubiostat/r-yaml/tree/master#columnmajor
                    list(
                        text = "Reference",
                        href = "reference/index.qmd"
                    ),
                    # placeholder for articles
                    list(
                    ),
                    list(
                        text = "News",
                        href = "news.qmd"
                    )
                ),
                right = list(
                    list(
                        icon = "github",
                        href = pkg$url
                    )
                )
            )
        ),
        format = list(
            html = list(
                theme = "cosmo",
                toc = TRUE
            )
        )
    )

    # add/remove elements from spec
    # NOTE: move from right to left to avoid problems for operations that rely
    # on thé index of the entry

    # remove image URI if no package logo provided (or exists where expected)
    if (pkg_logo_exists == FALSE) {
        spec$website$favicon <- NULL
    }

    # remove news entry in navbar if no news file found
    news_path <- fs::path(pkg_dir, "news.md")
    has_news <- fs::file_exists(path = news_path)
    if (has_news == FALSE) {
        spec$website$navbar$left[[3]] <- NULL
    }

    # populate/delete articles navbar entry as a function of articles found
    # determine whether package has articles
    path_articles <- fs::path(pkg_dir, "src", "vignettes")
    n_articles <- fs::dir_ls(path_articles, regexp = "\\.md") |>
        stringr::str_subset(pattern = "README", negate = TRUE) |>
        length()
    has_articles <- n_articles > 0

    # if there are articles, create articles entry in navbar
    if (has_articles) {
        # create YAML-friendly list of articles in target package
        article_list <- make_article_yaml(path_articles)
        # insert articles navbar entry right after reference entry
        spec$website$navbar$left[[2]] <- list(
            text = "Articles",
            menu = article_list
        )
    # otherwise, remove placeholder entry for articles
    } else {
        spec$website$navbar$left[[2]] <- NULL
    }

    # convert specification list to YAML
    # translating logical values into verbatim text
    quarto_yaml <- yaml::as.yaml(
        x = spec,
        handlers = list(
            # write logicals as verbatim `true` and `false`
            logical = function(x) {
                if (is.logical(x)) {
                    result <- ifelse(x, "true", "false")
                    class(result) <- "verbatim"
                } else {
                    result <- x
                }
                return(result)
            }
        )
    )

    return(quarto_yaml)

}

#' Write Quarto YAML to disk
#'
#' @param yaml Character. YAML specification string
#' @param site_dir Character. Directory where `_quarto.yml` should be written
#'
#' @importFrom yaml write_yaml
write_quarto_yaml <- function(
    yaml,
    site_dir
) {

    yaml::write_yaml(
        x = yaml,
        file = fs::path(site_dir, "_quarto.yml")
    )

}

#' Create Quarto site YAML from source package details
#'
#' @description
#' First, composes the YAML. Then writes it to disk
#'
#' To compose
#'
#' @param pkg_dir Character. Source package directory.
#' @param site_dir Character. Target site directory.
#' @param pkg_logo_exists Boolean. Whether package hex image exists
#' (or can be found).
#' 
#' @export 
create_quarto_yaml <- function(
    pkg_dir,
    site_dir,
    pkg_logo_exists = FALSE
) {

    # compose Quarto site YAML from package details
    # and presence or not of a package logo
    site_yaml <- compose_quarto_yaml(
        pkg_dir = pkg_dir, 
        pkg_logo_exists = pkg_logo_exists
    )

    # write _quarto.yml to root of site directory
    write_quarto_yaml(
        yaml = site_yaml,
        site_dir = site_dir
    )

}

# quarto_specs <- list(
#     project = list(
#         type = "website"
#     ),
#     website = list(
#         title = "Default title", # TODO extract package name somehow
#         `page-navigation` = TRUE,
#         navbar = list(
#             search = TRUE,
#             left = list(
#                 # specify either as a list of lists or as a data frame
#                 # see here: https://github.com/vubiostat/r-yaml/tree/master#columnmajor
#                 list(
#                     text = "Reference",
#                     href = "reference/index.qmd"
#                 ),
#                 list(
#                     text = "Articles",
#                     menu = article_list_for_yaml
#                 )
#             ),
#             right = list(
#                 list(
#                     icon = "github",
#                     href = "" # TODO: extract repo address somehow
#                 )
#             )
#         )
#     ),
#     format = list(
#         html = list(
#             theme = "cosmo",
#             toc = TRUE
#         )
#     )
# )

#' Make article component of Quarto YAML
#'
#' @description
#' Performs the following steps:
#'
#' - Compiles articles in the source package
#' - Composes the nested list structure expected by `{yaml::as.yaml}`
#'
#' @param articles_dir Character. Directory where package articles are stored
#'
#' @importFrom fs dir_ls path_file fs_path
#' @importFrom stringr str_replace
make_article_yaml <- function(
    articles_dir
) {

    # obtain file names of articles
    article_file_names <- articles_dir |>
        fs::dir_ls(regexp = "\\.md$") |>
        stringr::str_subset(pattern = "README", negate = TRUE) |>
        fs::path_file()

    # construct a relative path in Quarto site project
    article_new_rel_paths <- fs::path("articles/", article_file_names) |>
        stringr::str_replace(
            pattern = "\\.md",
            replacement = "\\.qmd"
        )
    n_articles <- length(article_new_rel_paths)

    # construct a list of the following form
    # list(
    #     list(href = "articles/file1.qmd"),
    #     list(href = "articles/file2.qmd")
    # )

    # create empty list
    article_list_for_yaml <- list()
    # name each vector element as href
    # article_paths_named <- stats::setNames(
    #     object = article_new_rel_paths,
    #     nm = rep("href", n_articles)
    # )
    # set i-th vector in i-th element of list
    for (i in seq_along(1:length(article_new_rel_paths))) {
        article_list_for_yaml[[i]] <- list(href = article_new_rel_paths[i])
    }

    # return nested list
    return(article_list_for_yaml)
}

# fs::dir_ls(path)

# nav_elements <- data.frame(
#     text = c()
# )

# quarto_yaml <- yaml::as.yaml(
#     x = quarto_specs,
#     handlers = list(
#         # write logicals as verbatim `true` and `false`
#         logical = function(x) {
#             if (is.logical(x)) {
#                 result <- ifelse(x, "true", "false")
#                 class(result) <- "verbatim"
#             } else {
#                 result <- x
#             }
#             return(result)
#         }
#     )
# )

# cat(quarto_yaml, "\n")

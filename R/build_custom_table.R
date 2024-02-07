#' Build custom reference index
#' 
#' @inheritParams build_reference_index
#' @param dir_in Character. Path to root of Stata package directory.
build_custom_reference_index <- function(
  dir_in,
  dir_out
) {

  # ingest YAML
  yaml <- yaml::yaml.load_file(
    input = fs::path(dir_in, "src/dev/assets/refence_index.yml")
  )

  # find section titles in YAML
  section_indices <- find_section(yaml = yaml)

  # write lines as character vector
  reference_index_content <- purrr::map_chr(
    .x = section_indices,
    .f = ~ write_section(
      yaml = yaml,
      index = .x
    )
  )

  # write to disk
  writeLines(
      text = reference_index_content,
      con = fs::path(dir_out, "index.qmd")
  )

}


# find `title`
# following is either `subtitle` or `contents`
# if `subtitle`, next in level is contents

#' Find the index of the "title" key in YAML
#' 
#' @param yaml YAML file loaded by `yaml::yaml.load_file()`
#' 
#' @importFrom purrr map
find_titles <- function(
  yaml
) {

  has_title <- purrr::map(
    .x = yaml$reference, 
    .f = ~ rlang::has_name(x = .x, name = "title")
  )

  # has_subtitle <- purrr::map(
  #   .x = my_yaml$reference, 
  #   .f = ~ rlang::has_name(x = .x, name = "subtitle")
  # )
  # has_subtitle_index <- which(has_subtitle == TRUE)

  has_title_index <- which(has_title == TRUE)

  return(has_title_index)

}


#' Write a section of documentation
#' 
#' @description
#' Sections contain one or more of the following
#' 
#' - Title (required)
#' - Description (optional)
#' - Sub-section (optional). See `write_sub_section` for more details.
#' - Contents (required, if not part of sub-section)
#' 
#' @inheritParams build_custom_reference_index
#' @inheritParams find_titles
#' @param index Index in the YAML of the object to write.
#' 
#' @importFrom glue glue
#' @importFrom rlang has_name
#' @importFrom purrr map as_vector
write_section <- function(
  dir_in,
  yaml,
  index
) {

  title <- glue::glue("## {yaml$reference[[index]]$title}")

  # TODO: hand if does (not) exist
  description <- yaml$reference[[index]]$desc

  # if next index is a sub-section, write write that sub-section
  # from the next index until the next title
  next_index <- index + 1
  next_is_sub_section <- rlang::has_name(
    x = yaml$reference[[next_index]], 
    name = "subtitle"
  )

  if (next_is_sub_section == TRUE) {
    
    # index of next title
    title_indices <- find_titles(yaml = yaml)
    next_title_index <- title_indices[title_indices > index][1]
    before_next_title_index <- ifelse(
      test = length(next_title_index) > 0,
      yes = next_title_index - 1,
      no = NA_integer_
    )

    # indices of next section(s) before next title
    # if no title after current index
      next_section_indices <- c(next_index:length(yaml$reference))
    # if there is another title after current index
    } else {
      next_section_indices <- c(next_index:before_next_title_index)
    }

    # write next section(s)
    section_contents <- purrr::map(
      .x = next_section_indices,
      .f = ~ write_sub_section(
        dir_in = dir_in,
        yaml = yaml, 
        index = .x
      )
    )

    # convert from list to character vector
    section_contents <- purrr::as_vector(section_contents)

  } else if (next_is_sub_section == FALSE) {

    section_contents <- write_contents(
      dir_in = dir_in,
      yaml = yaml, 
      index = next_index
    )

  }

  # compose section
  section <- c(title, "", description, "", section_contents)

  return(section)

}

#' Write a sub-section of documentation
#' 
#' @description
#' Sub-sections contain one or more of the following:
#' 
#' - Subtitle (required)
#' - Description (optional)
#' - Contents (required)
#' 
#' @inheritParams build_custom_reference_index
#' @inheritParams find_titles
#' @inheritParams write_section
write_sub_section <- function(
  dir_in,
  yaml,
  index
) {

  subtitle <- glue::glue("### {yaml$reference[[index]]$subtitle}")

  # TODO: hand if does (not) exist
  description <- yaml$reference[[index]]$desc

  contents <- write_contents(
    dir_in = dir_in,
    yaml = yaml,
    index = index
  )

  sub_section <- c(subtitle, "", description, "", contents)

  return(sub_section)

}

#' Write the table contents of a section/sub-section of documentation
#' 
#' @description
#' Composes the table of commands, links to their docs, and description 
#' of their use
#' 
#' @inheritParams build_custom_reference_index
#' @inheritParams find_titles
#' @inheritParams write_section
#' 
#' @importFrom fs dir_ls path
#' @importFrom glue glue glue_collapse
#' @importFrom purrr map_chr pmap_chr
#' 
#' @seealso build_reference_index
write_contents <- function(
  dir_in,
  yaml,
  index
) {

  # compile a list of the command names provided in the YAML's `contents` field
  help_names <- yaml$reference[[index]]$contents %>%
    as.list()

  # get the file paths for the command names compiled above
  help_pkg_paths <- fs::dir_ls(
    path = dir_in, 
    regexp = glue::glue("({glue::glue_collapse(x = unlist(help_names), sep = '|')})\\.md"),
    recurse = TRUE
  )

  # get the description of each command
  help_description <- purrr::map_chr(
      .x = help_pkg_paths,
      .f = ~ get_cmd_short_desc(file = .x)
  )

  # construct a path for the qmd for each help file
  help_site_paths <- purrr::map_chr(
      .x = help_names,
      .f = ~ fs::path("/reference", paste0(.x, ".qmd"))
  )

  # compose the table header
  tbl_header <- c(
      "| Functions | Description |",
      "|---|---|"
  )

  # compose the table body as character vector
  tbl_body <- purrr::pmap_chr(
      .l = list(
          # command name
          func = as.list(help_names),
          # path to the qmd file
          path = as.list(help_site_paths),
          # desription of the command
          desc = as.list(help_description)
      ),
      .f = ~ glue::glue("|[{..1}]({..2})|{..3}|")
  )

  # assemble the table header and body, adding an empty line at the end
  tbl <- c(tbl_header, tbl_body, "")

  return(tbl)

}

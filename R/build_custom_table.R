# # read YAML file
# yaml_path <- "C:/Users/wb393438/Survey Solutions/susoapi/_pkgdown.yml"
# my_yaml <- yaml::yaml.load_file(input = yaml_path)

# # -------------------

# ## {title}

# {desc}

# "| Command | Description |",
# "|---|---|"
# "|[{..1}]({..2})|{..3}|"

# ## {title}

# ### {subtitle}

# {desc}

# "| Command | Description |",
# "|---|---|"
# "|[{..1}]({..2})|{..3}|"

# -----------------

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
    if (length(next_title_index) == 0) {
      next_section_indices <- c(next_index:length(yaml$reference))
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

write_contents <- function(
  dir_in,
  yaml,
  index
) {

  help_names <- yaml$reference[[index]]$contents %>%
    as.list()

  help_pkg_paths <- fs::dir_ls(
    path = dir_in, 
    regexp = glue::glue("({glue::glue_collapse(x = unlist(help_names), sep = '|')})\\.md"),
    recurse = TRUE
  )

  help_description <- purrr::map_chr(
      .x = help_pkg_paths,
      .f = ~ get_cmd_short_desc(file = .x)
  )

  help_site_paths <- purrr::map_chr(
      .x = help_names,
      .f = ~ fs::path("/reference", paste0(.x, ".qmd"))
  )

  tbl_header <- c(
      "| Functions | Description |",
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

  tbl <- c(tbl_header, tbl_body, "")

  return(tbl)

}

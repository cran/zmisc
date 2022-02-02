
#### .Rd file parsing ####

# Utility function to extract title from Rd file
#
# The function could theoretically return most fields from the Rd file, but any
# formatting will be included, making other fields than "title" of little use,
# so this is currently the only supported field.
#
# In most cases, it is better to use the roxy_* functions which use roxygen to
# extract fields (tags) directly from the source files.
rd_get <- function(function_name, section) {
  stopifnot(section %in% c("title", "description", "examples"))
  rd <- find_package_root_file("man", paste0(function_name,".Rd")) |>
    tools::parse_Rd()
  rd_tags  <- tools:::RdTags(rd)
  rd_section <- rd[[which(rd_tags == paste0("\\", section))]] |> unlist()
  rd_section
}

#### Roxygen parsing ####

# Parse source code to get roxygen block objects
#
# This function is based on roxygenise, but opits the parts that have side
# effects - that is the parts that affect the enviroment and that output
# documentation files.
roxy_get_blocks <- function (package.dir = ".") {
  load_code <- NULL
  clean <- FALSE
  base_path <- normalizePath(package.dir, mustWork = TRUE)
  encoding <- desc::desc_get("Encoding", file = base_path)[[1]]
  if (!identical(encoding, "UTF-8")) {
      warning("roxygen2 requires Encoding: UTF-8", call. = FALSE)
  }
  roxygen2:::roxy_meta_load(base_path)
  packages <- roxygen2:::roxy_meta_get("packages")
  lapply(packages, loadNamespace)
  load_code <- roxygen2:::find_load_strategy(load_code)
  env <- load_code(base_path)
  blocks <- roxygen2:::parse_package(base_path, env = NULL)
  blocks <- lapply(blocks, roxygen2:::block_set_env, env = env)
  blocks
}


# Extract a documentation section from roxy blocks object
#
# This function takes a roxygen blocks structure, which typically is created by
# calling roxy_get_blocks(), and extracts the content for a particular topic
# (usually a function name) and tag (such as @title/@description/@examples).
#
# The main use case is to include the result in custom documentation files. The
# result is returned in the form of a character vector, which contains any
# formating as it is written in the source file. Thus, if the intention is to
# use the result in an .Rmd file, the roxydoc documentation should be formatted
# using markdown (and the @md tag should therefore be included)
#
# @param blocks A roxy blocks structure (list)
# @param name_topic The name of the topic (usually a function name)
# @param name_tag The name of the tag/section (@title/@description/@examples)
roxy_get_section <- function(blocks, name_topic, name_tag) {

  # Better to work with unclassed structure
  b <- blocks |> rapply(unclass, how="list")

  # Get the list of all topics (function names) that are
  # documented in the package, and assign as names to b
  topics <- b |>
    lapply('[[', 'object') |>
    lapply('[[', 'topic') |>
    unlist()
  names(b) <- topics

  # Get the substructure for the current topic, extract
  # all its tags, and assign as names to the structure
  b.cur_topic <- b[[name_topic]]$tags
  cur_topic_tags <- b.cur_topic |>
    lapply('[[', 'tag') |>
    unlist()
  names(b.cur_topic) <- cur_topic_tags

  # What we are interested in is the 'raw' field in
  # of the focal tag of the topic
  result <- b.cur_topic[[name_tag]]$raw

  # Return the result, trimming whitespace at start/end
  stringr::str_trim(result)
}


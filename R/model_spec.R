#' Load a PLS-SEM model specification
#'
#' Reads a construct/item/path specification from a YAML file, JSON file, or
#' an R list, and validates it. This is what makes the pipeline reusable
#' across datasets: instead of hardcoding construct names and item names in
#' the analysis script, you describe your model once in a small config file.
#'
#' @param spec Either a path to a \code{.yml}/\code{.yaml}/\code{.json} file,
#'   or an R list already in the expected shape (see Details).
#'
#' @details
#' The specification must have this shape (YAML shown, JSON/list identical
#' in structure):
#'
#' \preformatted{
#' constructs:
#'   Psychological Factors:
#'     type: reflective
#'     items: [PS_1, PS_2, PS_3]
#'   Social Factors:
#'     type: reflective
#'     items: [SO_1, SO_2, SO_3, SO_4]
#'   Good Shopping Vibes:
#'     type: reflective
#'     items: [GV_1, GV_2, GV_3, GV_4, GV_5]
#' paths:
#'   - from: [Psychological Factors, Social Factors]
#'     to: Good Shopping Vibes
#' }
#'
#' \code{type} may be \code{reflective} or \code{composite} (formative,
#' mode B). Any number of constructs and any number of path statements are
#' supported.
#'
#' @return An object of class \code{pls_model_spec}, a list with elements
#'   \code{constructs} (named list of item vectors), \code{construct_types}
#'   (named character vector), and \code{paths} (list of from/to pairs), plus
#'   convenience elements \code{seminr_measurement} and \code{seminr_structural}
#'   already built with \code{seminr::constructs()} / \code{seminr::relationships()}.
#'
#' @examples
#' \dontrun{
#' spec <- load_model_spec("model_spec.yml")
#' spec <- load_model_spec(list(
#'   constructs = list(
#'     "PsyFac" = list(type = "reflective", items = c("PS_1", "PS_2")),
#'     "Outcome" = list(type = "reflective", items = c("GV_1", "GV_2"))
#'   ),
#'   paths = list(list(from = "PsyFac", to = "Outcome"))
#' ))
#' }
#' @export
load_model_spec <- function(spec) {
  raw <- if (is.character(spec)) {
    if (!file.exists(spec)) stop("Model spec file not found: ", spec, call. = FALSE)
    ext <- tolower(tools::file_ext(spec))
    if (ext %in% c("yml", "yaml")) {
      yaml::read_yaml(spec)
    } else if (ext == "json") {
      jsonlite::fromJSON(spec, simplifyVector = FALSE)
    } else {
      stop("Unsupported spec file extension: '", ext, "'. Use .yml, .yaml, or .json.",
           call. = FALSE)
    }
  } else if (is.list(spec)) {
    spec
  } else {
    stop("`spec` must be a file path or a list.", call. = FALSE)
  }

  validate_model_spec(raw)

  construct_names <- names(raw$constructs)
  construct_items <- lapply(raw$constructs, function(x) unlist(x$items, use.names = FALSE))
  names(construct_items) <- construct_names
  construct_types <- vapply(raw$constructs, function(x) {
    t <- if (is.null(x$type)) "reflective" else tolower(x$type)
    if (!t %in% c("reflective", "composite", "formative")) {
      stop("Construct type must be 'reflective' or 'composite' (or 'formative'), got: '", t, "'",
           call. = FALSE)
    }
    if (t == "formative") t <- "composite"
    t
  }, character(1))
  names(construct_types) <- construct_names

  seminr_constructs <- lapply(construct_names, function(nm) {
    items <- construct_items[[nm]]
    type <- construct_types[[nm]]
    if (type == "reflective") {
      seminr::reflective(nm, item_names = items)
    } else {
      seminr::composite(nm, item_names = items)
    }
  })
  seminr_measurement <- do.call(seminr::constructs, seminr_constructs)

  path_list <- lapply(raw$paths, function(p) {
    seminr::paths(from = unlist(p$from, use.names = FALSE), to = unlist(p$to, use.names = FALSE))
  })
  seminr_structural <- do.call(seminr::relationships, path_list)

  structure(
    list(
      constructs = construct_items,
      construct_types = construct_types,
      paths = raw$paths,
      seminr_measurement = seminr_measurement,
      seminr_structural = seminr_structural
    ),
    class = "pls_model_spec"
  )
}

validate_model_spec <- function(raw) {
  if (is.null(raw$constructs) || length(raw$constructs) == 0) {
    stop("Model spec must define at least one construct under `constructs`.", call. = FALSE)
  }
  if (is.null(raw$paths) || length(raw$paths) == 0) {
    stop("Model spec must define at least one path under `paths`.", call. = FALSE)
  }
  for (nm in names(raw$constructs)) {
    items <- raw$constructs[[nm]]$items
    if (is.null(items) || length(items) == 0) {
      stop("Construct '", nm, "' has no items defined.", call. = FALSE)
    }
  }
  construct_names <- names(raw$constructs)
  for (p in raw$paths) {
    referenced <- c(unlist(p$from, use.names = FALSE), unlist(p$to, use.names = FALSE))
    missing <- setdiff(referenced, construct_names)
    if (length(missing) > 0) {
      stop("Path references undefined construct(s): ", paste(missing, collapse = ", "),
           call. = FALSE)
    }
  }
  invisible(TRUE)
}

#' @export
print.pls_model_spec <- function(x, ...) {
  cat("<pls_model_spec>\n")
  cat(length(x$constructs), "construct(s):\n")
  for (nm in names(x$constructs)) {
    cat(sprintf("  - %s [%s]: %s\n", nm, x$construct_types[[nm]],
                paste(x$constructs[[nm]], collapse = ", ")))
  }
  cat(length(x$paths), "structural path statement(s):\n")
  for (p in x$paths) {
    cat(sprintf("  - %s -> %s\n",
                paste(unlist(p$from), collapse = " + "),
                paste(unlist(p$to), collapse = " + ")))
  }
  invisible(x)
}

#' All item names referenced by a model spec, in construct order
#' @param spec A `pls_model_spec`
#' @return Character vector of item names
#' @export
spec_all_items <- function(spec) {
  unlist(spec$constructs, use.names = FALSE)
}

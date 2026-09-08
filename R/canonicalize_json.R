#' Canonicalize a JSON String
#'
#' Rewrites a JSON string into a deterministic, human-readable form: the
#' keys of every object are sorted, arrays whose elements are themselves
#' objects or arrays are sorted by their canonical text, the result is
#' pretty-printed with two-space indentation, and it ends with a newline.
#'
#' This exists because Lmod's `spider -o jsonSoftwarePage` emits object
#' keys - and the set-like arrays (the package list, the per-version
#' `parent` list, and same-version build variants) - in hash order, so
#' re-scanning an unchanged module tree produces a different file every
#' time.  [spider()] passes its output through this function so that the
#' cache is a pure function of the module tree.
#'
#' Element order carries no meaning in the spider output, so downstream
#' parsing is unaffected: [parse_module()] reduces `parent` with
#' `unique(unlist(...))` and keys the default off `defaultVersionName`,
#' and [module_avail()] re-sorts `versions` with [gtools::mixedorder()].
#' A single `parent` dependency chain is an array of plain strings and is
#' left in its given order.
#'
#' @param json A JSON-formatted character string.
#'
#' @return A canonical JSON-formatted character string, ending with a
#' newline.
#'
#' @examples
#' cat(canonicalize_json('[{"b":2,"a":1},{"b":0,"a":9}]'))
#'
#' @importFrom jsonlite fromJSON toJSON
#' @export
canonicalize_json <- function(json) {
  stopifnot(length(json) == 1L, is.character(json), !is.na(json))

  ## Always sort under the 'C' locale
  old_collate <- Sys.getlocale("LC_COLLATE")
  on.exit(Sys.setlocale("LC_COLLATE", old_collate))
  Sys.setlocale("LC_COLLATE", "C")

  enc <- function(v) {
    as.character(toJSON(v, auto_unbox = TRUE, null = "null", na = "null"))
  }

  order_recursively <- function(v) {
    if (!is.list(v)) return(v)
    v <- lapply(v, order_recursively)
    nms <- names(v)
    if (is.null(nms)) {
      ## A JSON array. Sort it only when its elements are themselves
      ## containers - those are the sets 'spider' shuffles.  Order by
      ## "package" first, where present (the top-level module list), then
      ## by canonical text; arrays of scalars, e.g. a single 'parent'
      ## dependency chain, are ordered data and left alone.
      if (length(v) > 1L && any(vapply(v, is.list, NA))) {
        pkg <- vapply(v, function(el) {
          p <- if (is.list(el)) el[["package"]] else NULL
          if (length(p) == 1L) as.character(p) else NA_character_
        }, NA_character_)
        v <- v[order(pkg, vapply(v, enc, ""))]
      }
    } else if (all(nzchar(nms))) {
      ## A JSON object. Sort its keys.
      v <- v[order(nms)]
    }
    v
  }

  x <- order_recursively(fromJSON(json, simplifyVector = FALSE))
  out <- toJSON(x, auto_unbox = TRUE, null = "null", na = "null", pretty = 2)
  paste0(out, "\n")
}

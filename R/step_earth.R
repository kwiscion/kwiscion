#' Step earth
#'
#' @param recipe recipe
#' @param ... selector
#' @param role role
#' @param trained trained
#' @param drop drop
#' @param outcome outcome
#' @param options options
#' @param res res
#' @param prefix prefix
#' @param skip skip
#' @param id id
#'
#' @export
#'
#' @importFrom recipes bake
#' @importFrom recipes prep
#'
#' @examples
step_earth <- function(recipe,
                       ...,
                       role = "predictor",
                       trained = FALSE,
                       # num_terms = 10,
                       # prune_method = 'backward',
                       drop = TRUE,
                       outcome = NULL,
                       options = list(),
                       res = NULL,
                       prefix = "EARTH",
                       skip = FALSE,
                       id = recipes::rand_id("earth")) {

  # ADD RANGE HANDLING TO num_terms AND prune_terms

  # if (!is_tune(threshold) & !is_varying(threshold)) {
  #   if (!is.na(threshold) && (threshold > 1 | threshold <= 0)) {
  #     rlang::abort("`threshold` should be on (0, 1].")
  #   }
  # }
  if (is.null(outcome)) {
    rlang::abort("`outcome` should select at least one column.")
  }

  recipes::add_step(
    recipe,
    step_earth_new(
      terms = recipes::ellipse_check(...),
      role = role,
      trained = trained,
      # num_terms = num_terms,
      # prune_method = prune_method,
      drop = drop,
      outcome = outcome,
      options = options,
      res = res,
      prefix = prefix,
      skip = skip,
      id = id
    )
  )
}

step_earth_new <-
  function(terms, role, trained,
           # num_terms, prune_method,
           drop, outcome,
           options, res,
           prefix, skip, id) {
    recipes::step(
      subclass = "earth",
      terms = terms,
      role = role,
      trained = trained,
      # num_terms = num_terms,
      # prune_method = prune_method,'
      drop = drop,
      outcome = outcome,
      options = options,
      res = res,
      prefix = prefix,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_earth <- function(x, training, info = NULL, ...) {
  x_names <- recipes::terms_select(x$terms,   info = info)
  y_names <- recipes::terms_select(x$outcome, info = info)
  recipes::check_type(training[, x_names])

  earth_call <- rlang::expr(earth::earth())

  if (length(x$options) > 0)
    earth_call <- recipes:::mod_call_args(earth_call, args = x$options)

  earth_call$x <- expr(training[, x_names, drop = FALSE])
  earth_call$y <- expr(training[, y_names, drop = FALSE])
  earth_obj <- eval(earth_call)

  step_earth_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    # num_tune = x$num_tune,
    # prune_method = x$prune_method,
    drop = x$drop,
    outcome = x$outcome,
    options = x$options,
    res = earth_obj,
    prefix = x$prefix,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_earth <- function(object, new_data, ...) {
  if (!is.null(object$res$cuts)) {

    which.terms <- c(1, grep('^h\\(', rownames(object$res$cuts)))

    if(length(which.terms) > 1){
      x <- earth:::get.earth.x(object$res, data = new_data, parent.frame())
      x <- earth:::get.bx(x, which.terms, object$res$dirs, object$res$cuts)

      x_new <- data.frame(row.names = 1:nrow(x))
      for(i in object$res$namesx) {
        x_tmp <- x[,grep(i, colnames(x))]
        colnames(x_tmp) <- sprintf('%s_%s', object$prefix, colnames(x_tmp))
        # colnames(x_tmp) <- sprintf('%s_%s%d', i, object$prefix, seq_len(ncol(x_tmp)))

        x_new <- cbind(x_new, x_tmp)
      }
      new_data <- dplyr::bind_cols(new_data, tibble::as_tibble(x_new))
      if(object$drop) {
        new_data <-
          new_data[, !(colnames(new_data) %in% object$res$namesx), drop = FALSE]
      }
    }
  }
  tibble::as_tibble(new_data)
}

print.step_earth <-
  function(x, width = max(20, options()$width - 29), ...) {
    if (is.null(x$res$cuts)) {
      cat("No EARTH splines were extracted.\n")
    } else {

      cat("EARTH extraction with ")
      recipes::printer(rownames(x$res$namesx), x$terms, x$trained, width = width)
    }
    invisible(x)
  }

#' pca_coefs <- function(x) {
#'   rot <- as.data.frame(x$res$rotation)
#'   vars <- rownames(rot)
#'   if (x$num_comp > 0) {
#'     npc <- ncol(rot)
#'     res <- utils::stack(rot)
#'     colnames(res) <- c("value", "component")
#'     res$component <- as.character(res$component)
#'     res$terms <- rep(vars, npc)
#'     res <- as_tibble(res)[, c("terms", "value", "component")]
#'   } else {
#'     res <- tibble::tibble(terms = vars, value = rlang::na_dbl,
#'                           component = rlang::na_chr)
#'   }
#'   res
#' }
#'
#' pca_variances <- function(x) {
#'   rot <- as.data.frame(x$res$rotation)
#'   vars <- rownames(rot)
#'   if (x$num_comp > 0) {
#'     variances <- x$res$sdev ^ 2
#'     p <- length(variances)
#'     tot <- sum(variances)
#'     y <- c(variances,
#'            cumsum(variances),
#'            variances / tot * 100,
#'            cumsum(variances) / tot * 100)
#'     x <-
#'       rep(
#'         c(
#'           "variance",
#'           "cumulative variance",
#'           "percent variance",
#'           "cumulative percent variance"
#'         ),
#'         each = p
#'       )
#'
#'     res <- tibble::tibble(terms = x,
#'                           value = y,
#'                           component = rep(1:p, 4))
#'   } else {
#'     res <- tibble::tibble(
#'       terms = vars,
#'       value = rep(rlang::na_dbl, length(vars)),
#'       component = rep(rlang::na_chr, length(vars))
#'     )
#'   }
#'   res
#' }
#'
#'
#'
#' #' @rdname step_pca
#' #' @param x A `step_pca` object.
#' #' @export
#' tidy.step_pca <- function(x, type = "coef", ...) {
#'   if (!is_trained(x)) {
#'     term_names <- sel2char(x$terms)
#'     res <- tibble(terms = term_names,
#'                   value = na_dbl,
#'                   component  = na_chr)
#'   } else {
#'     type <- match.arg(type, c("coef", "variance"))
#'     if (type == "coef") {
#'       res <- pca_coefs(x)
#'     } else {
#'       res <- pca_variances(x)
#'     }
#'   }
#'   res$id <- x$id
#'   res
#' }
#'
#'
#'
#' #' @rdname tunable.step
#' #' @export
#' tunable.step_pca <- function(x, ...) {
#'   tibble::tibble(
#'     name = "num_comp",
#'     call_info = list(list(pkg = "dials", fun = "num_comp", range = c(1L, 4L))),
#'     source = "recipe",
#'     component = "step_pca",
#'     component_id = x$id
#'   )
#' }

match_pal = function(pal, pals) {
  fx = function(x) tolower(gsub("[-, _, \\,, (, ), \\ , \\.]", "", x))
  charmatch(fx(pal), fx(pals))
}

get_pal_lens = function(pal) {
  pal_lens = c(
    R3 = 8L, R4 = 8L, ggplot2 = 8L, `Okabe-Ito` = 9L, Accent = 8L,
    `Dark 2` = 8L, Paired = 12L, `Pastel 1` = 9L, `Pastel 2` = 8L, 
    `Set 1` = 9L, `Set 2` = 8L, `Set 3` = 12L, `Tableau 10` = 10L, 
    `Classic Tableau` = 10L, `Polychrome 36` = 36L, Alphabet = 26L
  )
  pal_lens[pal]
}

# take a character string, match to either palette.pals() pr hcl.pals(), and
# generate the corresponding function factor with alpha transparency
gen_pal_fun = function(pal, gradient = FALSE, alpha = NULL, n = NULL) {
  pal_match = match_pal(pal, palette.pals())
  if (!is.na(pal_match)) {
    if (pal_match < 1L) stop("'palette' is ambiguous")
    pal_fun = palette.colors
    if (!is.null(n) && n >= get_pal_lens(pal_match)) {
      warning(
        "\nFewer colours ", get_pal_lens(pal_match), " provided than than there are groups ",
        n, ". Recycling to make up the shortfall."
      )
      pal_fun = function(n, palette, alpha) palette.colors(n = n, palette = pal, alpha = alpha, recycle = TRUE)
    }
    if (gradient) {
      pal_fun = function(n, palette, alpha) colorRampPalette(palette.colors(palette = pal, alpha = alpha))(n)
    }
  } else {
    pal_match = match_pal(pal, hcl.pals())
    if (!is.na(pal_match)) {
      if (pal_match < 1L) stop("'palette' is ambiguous")
      pal_fun = hcl.colors
    } else {
      stop(
        "\nPalette string not recogized. Must be a value produced by either",
        "`palette.pals()` or `hcl.pals()`.\n",
        call. = FALSE
      )
    }
  }
  return(pal_fun)
}

palette_factory = function(ngrps = 1L, palette = NULL, gradient = FALSE, ordered = FALSE, alpha = 1) {
  if (gradient) ngrps = 100L

  pal_theme = get_tpar("palette.qualitative", default = NULL)
  theme_flag = !is.null(pal_theme)

  recycle_or_interpolate = function(cols, ngrps, gradient, alpha) {
    if (!is.null(alpha)) cols = adjustcolor(cols, alpha.f = alpha)
    if (length(cols) < ngrps && length(cols) != 1) {
      if (gradient) {
        return(list(fun = function(...) cols, args = list(colorRampPalette(cols, alpha = TRUE)(ngrps))))
      } else {
        warning(
          "\nFewer colours (", length(cols), ") provided than than there are groups (",
          ngrps, "). Recycling to make up the shortfall."
        )
        return(list(fun = "c", args = as.list(rep_len(cols, ngrps))))
      }
    }
    list(fun = "c", args = as.list(cols))
  }

  gradient_slice_fun = function(n, palette, from = 0.1, to = 0.9, alpha = 1) {
    colorRampPalette(
      hcl.colors(n = 100, palette = palette, alpha = alpha)[(100 * from + 1):(100 * to)],
      alpha = TRUE
    )(n)
  }

  # Theme fallback path
  if (is.null(palette) && theme_flag) {
    if (length(pal_theme) == 1) {
      qual_match = match_pal(pal_theme, palette.pals())
      if (!is.na(qual_match)) {
        if (ngrps >= get_pal_lens(pal_theme) || ordered) {
          pal_theme = get_tpar("palette.sequential", default = NULL)
        }
      } else if (gradient && !is.na(match_pal(pal_theme, hcl.pals()))) {
        pal_theme = get_tpar("palette.sequential", default = NULL)
      }
    }
    if (length(pal_theme) == 1) {
      palette_fun = gen_pal_fun(pal = pal_theme, gradient = gradient, alpha = alpha)
      args = list(n = ngrps, palette = pal_theme, alpha = alpha)
    }
    palette = pal_theme
  }

  # No palette specified
  if (is.null(palette)) {
    if (ngrps <= length(palette()) && !ordered && !gradient) {
      # must be a function to avoid arg ambiguity
      palette_fun = function(alpha) adjustcolor(palette(), alpha)
      args = list(alpha = alpha)
    } else {
      if (ngrps <= 8 && !ordered) {
        palette = "R4"
        palette_fun = palette.colors
      } else {
        palette = "Viridis"
        if (!gradient && !ordered) {
          palette_fun = hcl.colors
        } else {
          palette_fun = gradient_slice_fun
        }
      }
      args = list(n = ngrps, palette = palette, alpha = alpha)
    }
    return(list(palette_fun = palette_fun, args = args, gradient = gradient, ordered = ordered, ngrps = ngrps))
  }

  # Character palette input
  if (is.character(palette)) {
    if (length(palette) > 1) {
      ri = recycle_or_interpolate(palette, ngrps, gradient, alpha)
      palette_fun = ri$fun
      args = ri$args
    } else {
      palette_fun = gen_pal_fun(palette, gradient = gradient, alpha = alpha, n = ngrps)
      args = list(n = ngrps, palette = palette, alpha = alpha)
    }
    return(list(palette_fun = palette_fun, args = args, gradient = gradient, ordered = ordered, ngrps = ngrps))
  }

  # Language objects: call/name
  if (inherits(palette, c("call", "name"))) {
    if (inherits(palette, "name") && is.character(eval(palette))) {
      cols = as.vector(eval(palette))
      ri = recycle_or_interpolate(cols, ngrps, gradient, alpha)
      palette_fun = ri$fun
      args = ri$args
      return(list(palette_fun = palette_fun, args = args, gradient = gradient, ordered = ordered, ngrps = ngrps))
    }

    args = as.list(palette)
    palette_fun = paste(args[[1]])
    args[[1]] = NULL

    if (palette_fun %in% c("c", "list")) {
      if (palette_fun == "list") palette_fun = "c"
      cols = unlist(args, recursive = FALSE, use.names = FALSE)
      ri = recycle_or_interpolate(cols, ngrps, gradient, alpha)
      palette_fun = ri$fun
      args = ri$args
    } else {
      args[["n"]] = ngrps
      # remove unnamed arguments to prevent unintentional argument sliding
      if (any(names(args) == "")) args[[which(names(args) == "")]] = NULL
    }
    return(list(palette_fun = palette_fun, args = args, gradient = gradient, ordered = ordered, ngrps = ngrps))
  }

  # Function palette input
  if (inherits(palette, "function")) {
    palette_fun = palette
    args = list()
    return(list(palette_fun = palette_fun, args = args, gradient = gradient, ordered = ordered, ngrps = ngrps))
  }

  stop(
    "\nInvalid palette argument. Must be a recognized keyword, or a ",
    "palette-generating function with named arguments.\n"
  )
}


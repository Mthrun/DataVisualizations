Multiplot <- function(
    ...,
    Plotlist = NULL,
    ColNo = 1,
    LayoutMat,
    Plotter = "auto",
    main,
    main_fontsize = 16,
    PlotlyMargin = 0.02,
    ShareX = FALSE,
    ShareY = FALSE,
    TitleX = ShareX,
    TitleY = ShareY,
    Widths = NULL,
    Heights = NULL,
    WhichLayout = "merge"
) {
  is_plotly_object <- function(x) {
    inherits(x, "plotly")
  }

  is_ggplot_object <- function(x) {
    inherits(x, "ggplot") ||
      inherits(x, "gg") ||
      inherits(x, "ggplot2::ggplot")
  }

  validate_col_number <- function(x) {
    if (
      length(x) != 1L ||
        !is.numeric(x) ||
        is.na(x) ||
        !is.finite(x) ||
        x < 1 ||
        x != floor(x) ||
        x > .Machine$integer.max
    ) {
      stop("Multiplot: ColNo must be one positive integer.", call. = FALSE)
    }
    as.integer(x)
  }

  # A Plotly or ggplot object can itself be list-like. Wrap a single object so
  # that combining it with ... does not flatten its internal structure.
  if (is.null(Plotlist)) {
    plotlist_items <- list()
  } else if (
    is_plotly_object(Plotlist) ||
      is_ggplot_object(Plotlist) ||
      inherits(Plotlist, c("grob", "gTree", "trellis", "recordedplot"))
  ) {
    plotlist_items <- list(Plotlist)
  } else if (is.list(Plotlist)) {
    plotlist_items <- Plotlist
  } else {
    plotlist_items <- list(Plotlist)
  }

  plots <- Filter(Negate(is.null), c(list(...), plotlist_items))
  numPlots <- length(plots)

  if (numPlots == 0L) {
    stop("Multiplot: no plots were supplied.", call. = FALSE)
  }

  has_main <- !missing(main) && !is.null(main)

  if (length(Plotter) != 1L || is.na(Plotter)) {
    stop(
      "Multiplot: Plotter must be 'auto', 'native', 'ggplot2', or 'plotly'.",
      call. = FALSE
    )
  }

  Plotter <- tolower(as.character(Plotter))
  if (Plotter == "ggplot") Plotter <- "ggplot2"
  if (Plotter == "base") Plotter <- "native"

  Plotter <- match.arg(
    Plotter,
    choices = c("auto", "native", "ggplot2", "plotly")
  )

  is_plotly <- vapply(plots, is_plotly_object, logical(1))
  is_ggplot <- vapply(plots, is_ggplot_object, logical(1))

  # Auto-dispatch rules:
  #   1. Any Plotly panel selects Plotly, provided every other panel is Plotly
  #      or ggplot (subplot() supports both).
  #   2. All-ggplot input selects gridExtra.
  #   3. Everything else uses the original native/grid path.
  if (Plotter == "auto") {
    if (any(is_plotly)) {
      if (!all(is_plotly | is_ggplot)) {
        bad <- which(!(is_plotly | is_ggplot))
        stop(
          paste0(
            "Multiplot: Plotly panels cannot be combined with native/grid ",
            "panels. Incompatible plot position(s): ",
            paste(bad, collapse = ", "), "."
          ),
          call. = FALSE
        )
      }
      Plotter <- "plotly"
    } else if (all(is_ggplot)) {
      Plotter <- "ggplot2"
    } else {
      Plotter <- "native"
    }
  }

  if (Plotter == "plotly" && !all(is_plotly | is_ggplot)) {
    bad <- which(!(is_plotly | is_ggplot))
    stop(
      paste0(
        "Multiplot: Plotter = 'plotly' accepts only Plotly or ggplot panels. ",
        "Incompatible plot position(s): ", paste(bad, collapse = ", "), "."
      ),
      call. = FALSE
    )
  }

  if (Plotter != "plotly" && any(is_plotly)) {
    stop(
      paste0(
        "Multiplot: Plotly object(s) were detected, but Plotter = '",
        Plotter,
        "'. Use Plotter = 'auto' or Plotter = 'plotly'."
      ),
      call. = FALSE
    )
  }

  # ---------------------------------------------------------------------------
  # Plotly branch
  # ---------------------------------------------------------------------------
  if (Plotter == "plotly") {
    if (!requireNamespace("plotly", quietly = TRUE)) {
      stop(
        "Multiplot: package 'plotly' is required for Plotter = 'plotly'.",
        call. = FALSE
      )
    }

    if (
      !is.numeric(PlotlyMargin) ||
        !length(PlotlyMargin) %in% c(1L, 4L) ||
        anyNA(PlotlyMargin) ||
        any(!is.finite(PlotlyMargin)) ||
        any(PlotlyMargin < 0 | PlotlyMargin > 1)
    ) {
      stop(
        "Multiplot: PlotlyMargin must be one or four numbers between 0 and 1.",
        call. = FALSE
      )
    }

    # Preserve the old one-panel behavior while still allowing a Plotly title.
    if (numPlots == 1L) {
      p <- plots[[1L]]
      if (is_ggplot_object(p)) {
        p <- plotly::ggplotly(p)
      }

      if (has_main) {
        p <- plotly::layout(
          p,
          title = list(
            text = paste(as.character(main), collapse = " "),
            x = 0.5,
            xanchor = "center",
            font = list(size = main_fontsize)
          )
        )
      }
      return(p)
    }

    if (missing(LayoutMat)) {
      ColNo <- validate_col_number(ColNo)
      nrows <- as.integer(ceiling(numPlots / ColNo))
      nslots <- nrows * ColNo
      subplot_plots <- plots

      # Padding keeps ColNo exact. plotly_empty() is intended for empty cells in
      # subplot layouts and hides the placeholder axes.
      if (nslots > numPlots) {
        subplot_plots <- c(
          subplot_plots,
          replicate(
            nslots - numPlots,
            plotly::plotly_empty(),
            simplify = FALSE
          )
        )
      }
    } else {
      if (!is.matrix(LayoutMat) || !is.numeric(LayoutMat) ||
          nrow(LayoutMat) < 1L || ncol(LayoutMat) < 1L) {
        stop(
          "Multiplot: Plotly LayoutMat must be a non-empty numeric matrix.",
          call. = FALSE
        )
      }

      # subplot() lays panels out row by row, whereas R matrices are stored
      # column by column. Transposing before vectorization preserves the visual
      # positions represented by LayoutMat.
      cell_ids <- as.vector(t(LayoutMat))
      blank_cells <- is.na(cell_ids) | cell_ids == 0
      plot_ids <- cell_ids[!blank_cells]

      if (
        any(!is.finite(plot_ids)) ||
          any(plot_ids != floor(plot_ids)) ||
          any(plot_ids < 1L) ||
          any(plot_ids > numPlots)
      ) {
        stop(
          paste0(
            "Multiplot: every non-empty Plotly LayoutMat cell must contain an ",
            "integer plot ID between 1 and ", numPlots, "."
          ),
          call. = FALSE
        )
      }

      plot_ids <- as.integer(plot_ids)

      if (anyDuplicated(plot_ids)) {
        duplicated_ids <- unique(plot_ids[duplicated(plot_ids)])
        stop(
          paste0(
            "Multiplot: repeated LayoutMat ID(s) ",
            paste(duplicated_ids, collapse = ", "),
            " imply spanning panels. The Plotly branch supports ordinary cells ",
            "and blanks, but not gridExtra-style spanning IDs."
          ),
          call. = FALSE
        )
      }

      if (!setequal(plot_ids, seq_len(numPlots))) {
        missing_ids <- setdiff(seq_len(numPlots), plot_ids)
        stop(
          paste0(
            "Multiplot: LayoutMat must contain every supplied plot exactly once. ",
            "Missing plot ID(s): ", paste(missing_ids, collapse = ", "),
            ". Use 0 or NA for blank cells."
          ),
          call. = FALSE
        )
      }

      subplot_plots <- lapply(cell_ids, function(id) {
        if (is.na(id) || id == 0) {
          plotly::plotly_empty()
        } else {
          plots[[as.integer(id)]]
        }
      })
      nrows <- nrow(LayoutMat)
    }

    subplot_args <- list(
      nrows = nrows,
      margin = PlotlyMargin,
      shareX = ShareX,
      shareY = ShareY,
      titleX = TitleX,
      titleY = TitleY,
      which_layout = WhichLayout
    )

    if (!is.null(Widths)) subplot_args$widths <- Widths
    if (!is.null(Heights)) subplot_args$heights <- Heights

    # subplot() accepts a list of Plotly/ggplot objects as its first argument.
    p <- do.call(
      plotly::subplot,
      c(list(unname(subplot_plots)), subplot_args)
    )

    if (has_main) {
      p <- plotly::layout(
        p,
        title = list(
          text = paste(as.character(main), collapse = " "),
          x = 0.5,
          xanchor = "center",
          font = list(size = main_fontsize)
        )
      )
    }

    # A visible return is important for htmlwidgets in interactive sessions.
    return(p)
  }

  # Preserve the original one-panel behavior for non-Plotly objects.
  if (numPlots == 1L) {
    print(plots[[1L]])
    return(invisible(plots[[1L]]))
  }

  # ---------------------------------------------------------------------------
  # ggplot2/gridExtra branch
  # ---------------------------------------------------------------------------
  if (Plotter == "ggplot2") {
    if (!requireNamespace("grid", quietly = TRUE)) {
      stop(
        "Multiplot: package 'grid' is required for Plotter = 'ggplot2'.",
        call. = FALSE
      )
    }
    if (!requireNamespace("gridExtra", quietly = TRUE)) {
      stop(
        "Multiplot: package 'gridExtra' is required for Plotter = 'ggplot2'.",
        call. = FALSE
      )
    }

    arrange_args <- unname(plots)
    if (missing(LayoutMat)) {
      arrange_args <- c(arrange_args, list(ncol = validate_col_number(ColNo)))
    } else {
      arrange_args <- c(arrange_args, list(layout_matrix = LayoutMat))
    }

    if (has_main) {
      arrange_args$top <- grid::textGrob(
        paste(as.character(main), collapse = " "),
        x = 0.5,
        hjust = 0.5,
        gp = grid::gpar(
          fontsize = main_fontsize,
          fontface = "bold"
        )
      )
    }

    p <- do.call(gridExtra::grid.arrange, arrange_args)
    return(invisible(p))
  }

  # ---------------------------------------------------------------------------
  # Native/grid branch
  # ---------------------------------------------------------------------------
  if (!requireNamespace("grid", quietly = TRUE)) {
    stop(
      "Multiplot: package 'grid' is required for Plotter = 'native'.",
      call. = FALSE
    )
  }

  if (missing(LayoutMat)) {
    ColNo <- validate_col_number(ColNo)
    LayoutMat <- matrix(
      seq_len(ColNo * ceiling(numPlots / ColNo)),
      ncol = ColNo,
      nrow = ceiling(numPlots / ColNo)
    )
  }

  if (!is.matrix(LayoutMat) || nrow(LayoutMat) < 1L || ncol(LayoutMat) < 1L) {
    stop("Multiplot: LayoutMat must be a non-empty matrix.", call. = FALSE)
  }

  nrows <- nrow(LayoutMat)
  ncols <- ncol(LayoutMat)
  grid::grid.newpage()

  if (!has_main) {
    grid::pushViewport(
      grid::viewport(layout = grid::grid.layout(nrows, ncols))
    )

    for (i in seq_len(numPlots)) {
      matchidx <- as.data.frame(which(LayoutMat == i, arr.ind = TRUE))
      if (nrow(matchidx) == 0L) next

      print(
        plots[[i]],
        vp = grid::viewport(
          layout.pos.row = matchidx$row,
          layout.pos.col = matchidx$col
        )
      )
    }
  } else {
    grid::pushViewport(
      grid::viewport(
        layout = grid::grid.layout(
          nrow = nrows + 1L,
          ncol = ncols,
          heights = grid::unit.c(
            grid::unit(1.8, "lines"),
            grid::unit(rep(1, nrows), "null")
          )
        )
      )
    )

    grid::grid.text(
      paste(as.character(main), collapse = " "),
      y = grid::unit(0.98, "npc"),
      gp = grid::gpar(
        fontsize = main_fontsize,
        fontface = "bold"
      )
    )

    for (i in seq_len(numPlots)) {
      matchidx <- as.data.frame(which(LayoutMat == i, arr.ind = TRUE))
      if (nrow(matchidx) == 0L) next

      print(
        plots[[i]],
        vp = grid::viewport(
          layout.pos.row = matchidx$row + 1L,
          layout.pos.col = matchidx$col
        )
      )
    }
  }

  invisible(list(Plotlist = plots))
}

# Existing Plotly call now auto-detects:
# Multiplot(obj1, obj2, ColNo = 2, main = "rs")
#
# Explicit Plotly options:
# Multiplot(
#   obj1, obj2,
#   Plotter = "plotly",
#   ColNo = 2,
#   PlotlyMargin = 0.05,
#   ShareX = TRUE,
#   ShareY = TRUE,
#   main = "rs"
# )

\name{Multiplot}
\alias{Multiplot}
\title{Arrange Multiple Native, ggplot2, or Plotly Plots}
\usage{
Multiplot(
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
)
}
\arguments{
  \item{...}{
    Plot objects to arrange. These may be native/grid-compatible plots,
    \code{ggplot} objects, or Plotly objects. Objects supplied here precede
    objects supplied through \code{Plotlist}.
  }
  \item{Plotlist}{
    Optional plot object or list of plot objects. A single Plotly, ggplot,
    grob, gTree, trellis, or recorded plot object is retained as one panel
    rather than being flattened as a list. \code{NULL} elements are discarded.
  }
  \item{ColNo}{
    Positive integer specifying the number of columns when \code{LayoutMat} is
    omitted. The number of rows is calculated automatically. Ignored when
    \code{LayoutMat} is supplied.
  }
  \item{LayoutMat}{
    Optional matrix specifying the panel layout. Plot numbers refer to the
    positions of the supplied plots after \code{NULL} entries have been
    removed. See \bold{Layout matrices} under \bold{Details} for
    backend-specific behavior.
  }
  \item{Plotter}{
    Character scalar selecting the plotting backend. The default,
    \code{"auto"}, detects the backend from the supplied objects. Other
    accepted values are \code{"native"}, \code{"ggplot2"}, and
    \code{"plotly"}. The aliases \code{"base"} and \code{"ggplot"} are
    accepted for \code{"native"} and \code{"ggplot2"}, respectively.
    Matching is case-insensitive.
  }
  \item{main}{
    Optional overall figure title. Multiple character values are collapsed to
    one title separated by spaces.
  }
  \item{main_fontsize}{
    Numeric font size used for \code{main}. In the native and ggplot2 branches,
    the title is bold. In the Plotly branch, the value is passed to the Plotly
    title font.
  }
  \item{PlotlyMargin}{
    Plotly only. One number, or four numbers, between \code{0} and \code{1},
    passed as the \code{margin} argument to \code{plotly::subplot()}.
  }
  \item{ShareX}{
    Plotly only. Logical value passed as \code{shareX} to
    \code{plotly::subplot()} to control shared x axes.
  }
  \item{ShareY}{
    Plotly only. Logical value passed as \code{shareY} to
    \code{plotly::subplot()} to control shared y axes.
  }
  \item{TitleX}{
    Plotly only. Logical value passed as \code{titleX} to
    \code{plotly::subplot()}. By default it follows \code{ShareX}.
  }
  \item{TitleY}{
    Plotly only. Logical value passed as \code{titleY} to
    \code{plotly::subplot()}. By default it follows \code{ShareY}.
  }
  \item{Widths}{
    Plotly only. Optional vector of relative subplot-column widths, passed as
    \code{widths} to \code{plotly::subplot()}.
  }
  \item{Heights}{
    Plotly only. Optional vector of relative subplot-row heights, passed as
    \code{heights} to \code{plotly::subplot()}.
  }
  \item{WhichLayout}{
    Plotly only. Value passed as \code{which_layout} to
    \code{plotly::subplot()}. The default is \code{"merge"}.
  }
}
\description{
  Arranges multiple plot objects in one figure. The function supports the
  original native/grid and ggplot2/gridExtra procedures and an interactive
  Plotly procedure based on \code{plotly::subplot()}.
}
\details{
  Plot objects may be supplied through \code{...}, through \code{Plotlist}, or
  through both. Objects from \code{...} are placed first, and all \code{NULL}
  entries are removed before backend detection and layout construction. An
  error is raised when no plots are supplied.

  \subsection{Automatic backend selection}{
    With \code{Plotter = "auto"}, the backend is selected as follows:
    \itemize{
      \item If at least one object inherits from \code{"plotly"}, the Plotly
      branch is selected, provided every other object is either a Plotly or
      ggplot object.
      \item If every object is a ggplot object, the ggplot2/gridExtra branch is
      selected.
      \item Otherwise, the native/grid branch is selected.
    }

    Plotly objects cannot be combined with native/grid objects. Such input is
    rejected with an error rather than allowing the final widget to overwrite
    the preceding plots. Plotly and ggplot objects may be combined in the
    Plotly branch. The Plotly branch may also be selected explicitly for one or
    more ggplot objects.

    Selecting \code{"native"} or \code{"ggplot2"} while supplying a Plotly
    object also raises an error; use \code{Plotter = "auto"} or
    \code{Plotter = "plotly"} instead.
  }

  \subsection{Layout matrices}{
    When \code{LayoutMat} is omitted, \code{ColNo} determines the number of
    columns. In the Plotly branch, incomplete final rows are padded with
    \code{plotly::plotly_empty()} so the requested number of columns is
    retained exactly.

    For the Plotly branch, \code{LayoutMat} must be a non-empty numeric matrix.
    Its cells are interpreted in visual row order. Each supplied plot must
    occur exactly once using an integer ID from \code{1} through the number of
    plots. Use \code{0} or \code{NA} for a blank cell. Repeated IDs, which
    represent spanning panels in gridExtra-style layouts, are not supported by
    the Plotly branch.

    For the ggplot2 and native/grid branches, the existing grid-style layout is
    retained. For example,
    \code{matrix(c(1, 2, 3, 3), nrow = 2, byrow = TRUE)} places plots 1 and 2
    in the first row and allows plot 3 to span the second row where supported by
    the selected backend.
  }

  \subsection{Single panels and titles}{
    A single Plotly panel is returned directly. A single ggplot panel selected
    for the Plotly branch is first converted with \code{plotly::ggplotly()}.
    When \code{main} is supplied, it is applied with \code{plotly::layout()}.

    For one non-Plotly panel, the original behavior is retained: the object is
    printed and returned invisibly.
  }

  The Plotly branch requires the \pkg{plotly} package. The ggplot2 branch
  requires \pkg{gridExtra} and \pkg{grid}; the native branch requires
  \pkg{grid}.
}
\value{
  The return value depends on the selected backend:
  \itemize{
    \item For \code{Plotter = "plotly"}, a Plotly object is returned visibly.
    \item For \code{Plotter = "ggplot2"}, the object produced by
    \code{gridExtra::grid.arrange()} is returned invisibly.
    \item For \code{Plotter = "native"}, an invisible list containing the
    arranged input objects in component \code{Plotlist} is returned.
    \item For one non-Plotly input, that object is returned invisibly after
    being printed.
  }
}
\examples{
data(Lsun3D)
Data <- Lsun3D$Data
Cls <- Lsun3D$Cls

obj1 <- Classplot(
  Data[, 1], Data[, 2], Cls = Cls,
  Plotter = "ggplot", Size = 3, main = "First plot"
)
obj2 <- Classplot(
  Data[, 2], Data[, 3], Cls = Cls,
  Plotter = "ggplot", Size = 3, main = "Second plot"
)
obj3 <- Classplot(
  Data[, 1], Data[, 3], Cls = Cls,
  Plotter = "ggplot", Size = 3, main = "Third plot"
)

## All objects are ggplots, so "auto" selects the ggplot2/gridExtra branch.
Multiplot(obj1, obj2, obj3, ColNo = 2, main = "Three Classplots")

## Repeated IDs may define a spanning panel in a grid-style layout.
grid_layout <- matrix(c(1, 2, 3, 3), nrow = 2, byrow = TRUE)
Multiplot(
  obj1, obj2, obj3,
  LayoutMat = grid_layout,
  Plotter = "ggplot2",
  main = "Spanning layout"
)

## Plotly objects are detected automatically.
if (requireNamespace("plotly", quietly = TRUE)) {
  p1 <- plotly::plot_ly(
    x = 1:10, y = (1:10)^2,
    type = "scatter", mode = "lines"
  )
  p2 <- plotly::plot_ly(
    x = 1:10, y = sqrt(1:10),
    type = "scatter", mode = "markers"
  )

  interactive_panel <- Multiplot(
    p1, p2,
    ColNo = 2,
    PlotlyMargin = 0.05,
    ShareX = TRUE,
    main = "Two Plotly Panels"
  )
  interactive_panel

  p3 <- plotly::plot_ly(
    x = 1:10, y = log(1:10),
    type = "scatter", mode = "lines+markers"
  )

  ## Plotly LayoutMat supports ordinary cells and 0/NA blank cells.
  plotly_layout <- matrix(c(1, 2, 3, 0), nrow = 2, byrow = TRUE)
  Multiplot(p1, p2, p3, LayoutMat = plotly_layout)
}
}
\seealso{
  \code{\link{Classplot}}, \code{gridExtra::grid.arrange()},
  \code{plotly::subplot()}, \code{plotly::plotly_empty()}
}
\author{
  Michael Thrun, based on an idea by Winston Chang.
}
\keyword{hplot}

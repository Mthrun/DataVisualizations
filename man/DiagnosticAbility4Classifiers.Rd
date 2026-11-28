\name{DiagnosticAbility4Classifiers}
\alias{DiagnosticAbility4Classifiers}
\title{Diagnostic Ability of Binary Classifiers}
\description{
Compare binary classifiers or different conditions of a classifier in ROC,
precision-recall, or sensitivity-specificity space. Each prediction column
produces one operating point from its confusion matrix, preferably evaluated
on a test set. The result includes a Plotly widget and the plotted coordinates.
}
\usage{
DiagnosticAbility4Classifiers(TrueCondition_Cls, ManyPredictedCondition_Cls,

  NamesOfConditions = NULL, PlotType = "PRC", xlab = NULL,

  ylab = NULL, main = NULL, Colors = NULL, LineColor = NULL,

  Size = 8, LineWidth = 1, LineType = NULL, Showgrid = TRUE,

  SaveIt = FALSE, PositiveClass = NULL)
}
\arguments{
  \item{TrueCondition_Cls}{[1:n] Vector of reference class labels, preferably
from a test set, containing exactly two observed classes. Numeric, character,
logical and factor labels are supported. Missing values and non-finite numeric
labels are not allowed.}
  \item{ManyPredictedCondition_Cls}{[1:n,1:c] Matrix or data frame with one
prediction per reference observation and one classifier or condition per
column. At least one column is required. Predictions must use the reference
labels without missing or non-finite values. A column may predict only one
of the two classes. These are class labels, not probabilities or scores.}
  \item{NamesOfConditions}{[1:c] Names of the conditions, used in the legend
and point hover text. Defaults to the column names, or consecutive numbers
if column names are absent.}
  \item{PlotType}{One of \code{"PRC"} (precision-recall), \code{"ROC"}
(receiver operating characteristic), or \code{"SenSpec"}
(sensitivity-specificity). See Details for the coordinate conventions.}
  \item{xlab}{Optional x-axis title. \code{NULL} selects a title appropriate
to \code{PlotType}.}
  \item{ylab}{Optional y-axis title. \code{NULL} selects a title appropriate
to \code{PlotType}.}
  \item{main}{Optional plot title. \code{NULL} selects a title appropriate
to \code{PlotType}.}
  \item{Colors}{R color names or hexadecimal colors in prediction-column order.
Supply at least one color per column; names on this vector are ignored.
\code{NULL} uses \code{DefaultColorSequence}, repeated if necessary.}
  \item{LineColor}{Optional single R color. If supplied, connects the defined
points in prediction-column order. \code{NULL} draws points only.}
  \item{Size}{Positive finite marker size.}
  \item{LineWidth}{Non-negative finite width of the optional connecting line.}
  \item{LineType}{Plotly dash style: \code{"solid"}, \code{"dot"},
\code{"dash"}, \code{"longdash"}, \code{"dashdot"}, or
\code{"longdashdot"}. \code{NULL} selects \code{"solid"}.}
  \item{Showgrid}{Logical value controlling grid lines on both axes.}
  \item{SaveIt}{If \code{TRUE}, saves the widget as \code{PRC.html},
\code{ROC.html}, or \code{SenSpec.html} in the working directory, replacing
an existing file with that name. Saving a self-contained HTML file requires
\pkg{htmlwidgets} and a working Pandoc installation.}
  \item{PositiveClass}{The reference label treated as positive. If \code{NULL},
uses the first observed factor level of \code{TrueCondition_Cls}; for a
non-factor vector, uses the first level produced by \code{factor()}.
Unused factor levels are ignored. This retains the previous first-class
convention. For numeric 0/1 labels the default positive class is therefore 0;
set \code{PositiveClass = 1} explicitly when 1 denotes the positive condition.}
}
\details{
The confusion matrix uses predicted classes as rows and reference classes as
columns, with the positive class first on both axes. TP, FP, FN, and TN denote
true positives, false positives, false negatives, and true negatives.
Precision is TP/(TP+FP), recall (sensitivity) is TP/(TP+FN), the false positive
rate is FP/(FP+TN), and specificity is TN/(TN+FP).

The coordinate conventions are:
\describe{
  \item{PRC}{x = precision, y = recall. This retains the function's existing
axis order.}
  \item{ROC}{x = false positive rate, y = true positive rate (recall).
A dotted diagonal from (0,0) to (1,1) is added as a reference.}
  \item{SenSpec}{x = sensitivity, y = specificity.}
}
If a classifier predicts no positive observations, precision is undefined and
its returned PRC x coordinate is \code{NA_real_}. The corresponding point is
omitted with a warning; its recall and position in the returned vectors are
retained. If all PRC points are undefined, an empty widget with the requested
axes is returned. ROC and SenSpec coordinates remain defined because both
reference classes must be observed.

For imbalanced binary classes, precision-recall plots can reveal limitations
of positive predictions that are less apparent in ROC space; see
Saito and Rehmsmeier (2015). The function does not fit classifiers, select
thresholds, or compute a curve or area under a curve. A connecting line is
only a visual guide through the supplied operating points. Plotting requires
\pkg{plotly}.
}
\value{
A list with:
\item{Plot}{A Plotly htmlwidget.}
\item{X}{[1:c] Numeric vector of x-axis values in prediction-column order,
including \code{NA_real_} for undefined precision in PRC mode.}
\item{Y}{[1:c] Numeric vector of y-axis values in prediction-column order.}
}
\references{
Hoffmann, J., Rother, M., Kaiser, U., Thrun, M. C., Wilhelm, C., Gruen, A.,
Niebergall, U., Meissauer, U., Neubauer, A., and Brendel, C. (2020).
Determination of CD43 and CD200 surface expression improves accuracy of
B-cell lymphoma immunophenotyping. Cytometry Part B: Clinical Cytometry,
98(6), 476--482. \doi{10.1002/cyto.b.21936}.

Saito, T. and Rehmsmeier, M. (2015).
The Precision-Recall Plot Is More Informative than the ROC Plot When Evaluating
Binary Classifiers on Imbalanced Datasets. PLoS ONE, 10(3), e0118432.
\doi{10.1371/journal.pone.0118432}.
}
\author{Michael Thrun}
\note{Only binary classifications are supported.}
\seealso{\code{\link{Classplot}}}
\examples{
if (requireNamespace("plotly", quietly = TRUE)) {
  Truth = c(1, 1, 1, 2, 2, 2)
  Predictions = cbind(
    A = c(1, 1, 2, 1, 2, 2),
    B = c(1, 1, 1, 1, 2, 2),
    AllNegative = rep(2, 6)
  )
  Result = DiagnosticAbility4Classifiers(
    Truth, Predictions, PlotType = "ROC", PositiveClass = 1
  )
  Plot = Result$Plot
  X = Result$X
  Y = Result$Y
  X
  Y
}
}
\keyword{classif}
\keyword{hplot}

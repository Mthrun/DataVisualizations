\name{DiagnosticAbility4Classifiers}
\alias{DiagnosticAbility4Classifiers}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
DiagnosticAbility4Classifiers
}
\description{
DiagnosticAbility4Classifiers as applied in [...].
}
\usage{
DiagnosticAbility4Classifiers(TrueCondition_Cls, ManyPredictedCondition_Cls,

NamesOfConditions = NULL, PlotType = "PRC", xlab = "True Positive Rate",

ylab = "False Positive Rate", main = "ROC Space",

Colors, LineColor = NULL, Size = 8, LineWidth = 1,

LineType = NULL, Showgrid = TRUE, SaveIt = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{TrueCondition_Cls}{
[1:n] numeric vector of k classes (true classification), preferably of the testset
}
  \item{ManyPredictedCondition_Cls}{
[1:n,1:c] every col c is a Cls of one specific condition of the classifier trying to reproduce the classification (preferably on a test set)
}
  \item{NamesOfConditions}{
[1:c] character vector of c conditions, sets names of legend and the points
}
  \item{PlotType}{
possible are 'ROC':Receiver operating characteristic. 'PRC': Precision Recall, and 'SenSpec':Sensitivity-Specifity Plot
}

  \item{xlab}{
Optional, string
}
  \item{ylab}{
Optional, string
}
  \item{main}{
Optional, string
}
  \item{Colors}{
Optional, string
}
  \item{LineColor}{
Optional, name of color, then all points are connected by a curve
}
  \item{Size}{
Optional, number defining the Size of the curve
}
  \item{LineWidth}{
Optional, number defining the width of the curve
}
  \item{LineType}{
Optional, string defining the type of the curve
}
  \item{Showgrid}{
Optional, boolean
}
  \item{SaveIt}{
Optional, boolean, if true saves plot as html
}
}
\details{
For unbalanced binary classes PRC should be preferred and not ROC [Saito/Rehmsmeier, 2016].
}
\value{
 If it is a LIST, use
\item{Plot}{plotly handler}
\item{X}{[1:c] vector of xaxis values}
\item{Y}{[1:c] vector of y axis values}
}
\references{
[|] :Determination of CD43 and CD200 surface expression improves accuracy of B-cell lymphoma immunophenotyping, 2020.


[Saito/Rehmsmeier, 2016] Saito, Takaya and Rehmsmeier, Marc: The Precision-Recall Plot Is More Informative than the ROC Plot When Evaluating Binary Classifiers on Imbalanced Datasets, PlosOne, https://doi.org/10.1371/journal.pone.0118432, 2016.
}
\author{
Michael Thrun
}
\note{
Currently only for binary classifiers developed
}

\seealso{
\code{\link{Classplot}}
}
\examples{
#TODo
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ROC}% use one of  RShowDoc("KEYWORDS")
\keyword{Diagnostic}% __ONLY ONE__ keyword per line
\keyword{Classifiers}% __ONLY ONE__ keyword per line
\keyword{DiagnosticAbility}% __ONLY ONE__ keyword per line
\keyword{Precision}% __ONLY ONE__ keyword per line
\keyword{Recall}% __ONLY ONE__ keyword per line
\keyword{Sensitivity}% __ONLY ONE__ keyword per line
\keyword{true positive rate}
\keyword{false positive rate}
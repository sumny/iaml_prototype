% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/measures_iml.R
\name{mlr_measures_iml_number_of_features}
\alias{mlr_measures_iml_number_of_features}
\alias{MeasureIMLNF}
\title{IML Number of Features Used}
\description{
Measures the number of selected features used by a model according to Molnar et al. (2020).
If parameter \code{normalize} is set to \code{TRUE}, the relative number of features instead of the absolute
number of features is returned.
Note that the models must be stored to be able to extract this information.

This measure requires the \link[mlr3:Task]{mlr3::Task} and the \link[mlr3:Learner]{mlr3::Learner} for scoring.
}
\section{Dictionary}{

This \link{Measure} can be instantiated via the \link[mlr3misc:Dictionary]{dictionary} \link{mlr_measures} or with the associated sugar function \code{\link[=msr]{msr()}}:\preformatted{mlr_measures$get("iml_number_of_features")
msr("iml_number_of_features")
}
}

\section{Meta Information}{

\itemize{
\item Task type: \dQuote{NA}
\item Range: \eqn{[0, \infty)}{[0, Inf)}
\item Minimize: TRUE
\item Average: macro
\item Required Prediction: \dQuote{prob}
\item Required Packages: \CRANpkg{mlr3}
}
}

\section{Parameters}{
\tabular{llll}{
   Id \tab Type \tab Default \tab Levels \cr
   normalize \tab logical \tab FALSE \tab TRUE, FALSE \cr
}


\describe{
\item{\code{normalize}}{\code{logical(1)}\cr
Should the relative number of features instad of the absolute number be returned?
}
}
}

\references{
Molnar, Christoph, Casalicchio, Giuseppe, Bischl, Bernd (2020).
\dQuote{Quantifying Model Complexity via Functional Decomposition for Better Post-Hoc Interpretability.}
In \emph{Machine Learning and Knowledge Discovery in Databases}, 193--204.
}
\section{Super class}{
\code{\link[mlr3:Measure]{mlr3::Measure}} -> \code{MeasureIMLNF}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-new}{\code{MeasureIMLNF$new()}}
\item \href{#method-clone}{\code{MeasureIMLNF$clone()}}
}
}
\if{html}{
\out{<details open ><summary>Inherited methods</summary>}
\itemize{
\item \out{<span class="pkg-link" data-pkg="mlr3" data-topic="Measure" data-id="aggregate">}\href{../../mlr3/html/Measure.html#method-aggregate}{\code{mlr3::Measure$aggregate()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="mlr3" data-topic="Measure" data-id="format">}\href{../../mlr3/html/Measure.html#method-format}{\code{mlr3::Measure$format()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="mlr3" data-topic="Measure" data-id="help">}\href{../../mlr3/html/Measure.html#method-help}{\code{mlr3::Measure$help()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="mlr3" data-topic="Measure" data-id="print">}\href{../../mlr3/html/Measure.html#method-print}{\code{mlr3::Measure$print()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="mlr3" data-topic="Measure" data-id="score">}\href{../../mlr3/html/Measure.html#method-score}{\code{mlr3::Measure$score()}}\out{</span>}
}
\out{</details>}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-new"></a>}}
\if{latex}{\out{\hypertarget{method-new}{}}}
\subsection{Method \code{new()}}{
Creates a new instance of this \link[R6:R6Class]{R6} class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{MeasureIMLNF$new()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-clone"></a>}}
\if{latex}{\out{\hypertarget{method-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{MeasureIMLNF$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}

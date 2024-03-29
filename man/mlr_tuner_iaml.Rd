% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/TunerIAML.R
\name{mlr_tuner_iaml}
\alias{mlr_tuner_iaml}
\alias{TunerIAML}
\title{Multi-objective Hyperparameter Optimization, Feature Selection and Interaction and Monotonicity Constraints}
\description{
Performs joint multi-objective optimization of hyperparameters, feature selection and interaction and monotonicity
constraints of a suitable \link[mlr3:Learner]{mlr3::Learner}.

This requires an appropriate \link[mlr3:Learner]{mlr3::Learner}, that allows for selecting features, and setting interaction and
monotonicity constraints, e.g., xgboost.
}
\section{Dictionary}{

This \link{Tuner} can be instantiated via the \link[mlr3misc:Dictionary]{dictionary}
\link{mlr_tuners} or with the associated sugar function \code{\link[=tnr]{tnr()}}:\preformatted{TunerIAML$new()
mlr_tuners$get("iaml")
tnr("iaml")
}
}

\section{Parameters}{

\describe{
\item{\code{select_id}}{\code{character(1)}\cr
ID of param in Learner that selects features.}
\item{\code{interaction_id}}{\code{character(1)}\cr
ID of param in Learner that sets interaction constraints.}
\item{\code{monotone_id}}{\code{character(1)}\cr
ID of param in Learner that sets monotonicity constraints.}
\item{\code{batch_size}}{\code{integer(1)}\cr
Maximum number of points to try in a batch.}
}
}

\section{Progress Bars}{

\verb{$optimize()} supports progress bars via the package \CRANpkg{progressr}
combined with a \link{Terminator}. Simply wrap the function in
\code{progressr::with_progress()} to enable them. We recommend to use package
\CRANpkg{progress} as backend; enable with \code{progressr::handlers("progress")}.
}

\section{Logging}{

All \link{Tuner}s use a logger (as implemented in \CRANpkg{lgr}) from package
\CRANpkg{bbotk}.
Use \code{lgr::get_logger("bbotk")} to access and control the logger.
}

\concept{Tuner}
\section{Super class}{
\code{\link[mlr3tuning:Tuner]{mlr3tuning::Tuner}} -> \code{TunerIAML}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-new}{\code{TunerIAML$new()}}
\item \href{#method-clone}{\code{TunerIAML$clone()}}
}
}
\if{html}{
\out{<details open ><summary>Inherited methods</summary>}
\itemize{
\item \out{<span class="pkg-link" data-pkg="mlr3tuning" data-topic="Tuner" data-id="format">}\href{../../mlr3tuning/html/Tuner.html#method-format}{\code{mlr3tuning::Tuner$format()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="mlr3tuning" data-topic="Tuner" data-id="help">}\href{../../mlr3tuning/html/Tuner.html#method-help}{\code{mlr3tuning::Tuner$help()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="mlr3tuning" data-topic="Tuner" data-id="optimize">}\href{../../mlr3tuning/html/Tuner.html#method-optimize}{\code{mlr3tuning::Tuner$optimize()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="mlr3tuning" data-topic="Tuner" data-id="print">}\href{../../mlr3tuning/html/Tuner.html#method-print}{\code{mlr3tuning::Tuner$print()}}\out{</span>}
}
\out{</details>}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-new"></a>}}
\if{latex}{\out{\hypertarget{method-new}{}}}
\subsection{Method \code{new()}}{
Creates a new instance of this \link[R6:R6Class]{R6} class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{TunerIAML$new()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-clone"></a>}}
\if{latex}{\out{\hypertarget{method-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{TunerIAML$clone(deep = FALSE)}\if{html}{\out{</div>}}
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

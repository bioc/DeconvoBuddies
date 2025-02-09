% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_composition_bar.R
\name{plot_composition_bar}
\alias{plot_composition_bar}
\title{Create barplot of average cell type composition}
\usage{
plot_composition_bar(
  prop_long,
  sample_col = "RNum",
  x_col = "ALL",
  prop_col = "prop",
  ct_col = "cell_type",
  add_text = TRUE,
  min_prop_text = 0
)
}
\arguments{
\item{prop_long}{A \code{data.frame} of cell type portions in long form}

\item{sample_col}{A \code{character(1)} specifying the name of column in
\code{prop_long} that identifies samples.}

\item{x_col}{A \code{character(1)} specifying the name of column in
\code{prop_long} that specifies the category to divide samples by.}

\item{prop_col}{A \code{character(1)} specifying the name of column in
\code{prop_long} that contains proportion values.}

\item{ct_col}{A \code{character(1)} specifying the name of column in
\code{prop_long} containing cell type names.}

\item{add_text}{A \code{logical(1)} determining whether to add the rounded
proportion value to the bars.}

\item{min_prop_text}{A \code{numeric(1)} specifying the minimum proportion to
display text. Values greater than (>) \code{min_prop_text} will be displayed.}
}
\value{
A stacked barplot \code{ggplot2} object representing the mean proportion
of cell types for each group.
}
\description{
Given a long formatted \code{data.frame}, this function creates a barplot for
the average cell type composition among a set of samples (donors) using
\code{ggplot2}.
}
\examples{
# Load example data
data("rse_bulk_test")
data("est_prop")

# extract relevant colData from the example RangedSummarizedExperiment object
pd <- SummarizedExperiment::colData(rse_bulk_test) |>
    as.data.frame()

# combine with the example estimated proportions in a long style table
est_prop_long <- est_prop |>
    tibble::rownames_to_column("RNum") |>
    tidyr::pivot_longer(!RNum, names_to = "cell_type", values_to = "prop") |>
    dplyr::inner_join(pd |> dplyr::select(RNum, Dx))

est_prop_long

# Create composition bar plots
# Mean composition of all samples
plot_composition_bar(est_prop_long)

# Mean composition by Dx
plot_composition_bar(est_prop_long, x_col = "Dx")

# control minimum value of text to add
plot_composition_bar(est_prop_long, x_col = "Dx", min_prop_text = 0.1)

# plot all samples, then facet by Dx
plot_composition_bar(est_prop_long, x_col = "RNum", add_text = FALSE) +
    ggplot2::facet_wrap(~Dx, scales = "free_x")

}

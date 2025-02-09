% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_marker_express_List.R
\name{plot_marker_express_List}
\alias{plot_marker_express_List}
\title{Plot a nested list of genes as a multi-page pdf}
\usage{
plot_marker_express_List(
  sce,
  gene_list,
  pdf_fn = NULL,
  cellType_col = "cellType",
  gene_name_col = "gene_name",
  color_pal = NULL,
  plot_points = FALSE
)
}
\arguments{
\item{sce}{\link[SummarizedExperiment:SummarizedExperiment-class]{SummarizedExperiment-class} object}

\item{gene_list}{A named \code{list()} of \code{character()} vectors containing the
names of genes to plot.}

\item{pdf_fn}{A \code{character()} of the pdf filename to plot to, if \code{NULL} returns all plots}

\item{cellType_col}{The \code{character(1)} name of \code{colData()} column containing
cell type for \code{sce} data. It should match \code{cellType.target} in \code{stats}.}

\item{gene_name_col}{The \code{character(1)} name of \code{rowData()} matching the
gene name from \code{gene_list}.}

\item{color_pal}{A named \code{character(1)} vector that contains a color pallet
matching the \code{cell_type} values.}

\item{plot_points}{A \code{logical(1)} indicating whether to plot points over the
violin, defaults to \code{FALSE} as these often become over plotted and quite
large (especially when saved as PDF).}
}
\value{
A PDF file with violin plots for the expression of top marker genes
for all cell types.
}
\description{
This function plots a nested list of genes as a multi-page PDF, one for each
sub list. A use case is plotting known marker genes for multiple cell types
over cell type clusters with unknown identities.
}
\examples{
## Fetch sce example data
if (!exists("sce_DLPFC_example")) sce_DLPFC_example <- fetch_deconvo_data("sce_DLPFC_example")

## Create list-of-lists of genes to plot, names of sub-list become title of page
my_gene_list <- list(Inhib = c("GAD2", "SAMD5"), Astro = c("RGS20", "PRDM16"))

# Return a list of plots
plots <- plot_marker_express_List(
    sce_DLPFC_example,
    gene_list = my_gene_list,
    cellType_col = "cellType_broad_hc"
)

print(plots[[1]])

# Plot marker gene expression to PDF, one page per cell type in stats
pdf_file <- tempfile("test_marker_expression_List", fileext = ".pdf")

plot_marker_express_List(
    sce_DLPFC_example,
    gene_list = my_gene_list,
    pdf_fn = pdf_file,
    cellType_col = "cellType_broad_hc"
)

if (interactive()) browseURL(pdf_file)

}
\seealso{
Other expression plotting functions: 
\code{\link{plot_gene_express}()},
\code{\link{plot_marker_express}()},
\code{\link{plot_marker_express_ALL}()}
}
\concept{expression plotting functions}

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot.R
\name{plot_mosaic.tna_data}
\alias{plot_mosaic.tna_data}
\title{Plot State Frequencies as a Mosaic Between Two Groups}
\usage{
\method{plot_mosaic}{tna_data}(x, group, label = "Group", digits = 1, ...)
}
\arguments{
\item{x}{A \code{tna_data} object.}

\item{group}{A \code{character} string giving the column name of the (meta) data
to contrast the frequencies with or a vector of group indicators with the
the same length as the number of rows in the sequence data.}

\item{label}{An optional \code{character} string that specifies a label for the
grouping variable when \code{group} is not a column name of the data.}

\item{digits}{An \code{integer} that determines the number of digits to use
for the chi-square test statistic and the p-value in the plot.}

\item{...}{Ignored.}
}
\value{
A \code{ggplot} object.
}
\description{
Plot State Frequencies as a Mosaic Between Two Groups
}
\examples{
d <- data.frame(
  time = rep(1:5, rep = 4),
  group = rep(1:4, each = 5),
  event = sample(LETTERS[1:3], 20, replace = TRUE)
)
sequence_data <- prepare_data(
  d,
  time = "time",
  actor = "group",
  action = "event"
)
plot_mosaic(sequence_data, group = "group")

}
\seealso{
Basic functions
\code{\link{build_model}()},
\code{\link{hist.group_tna}()},
\code{\link{hist.tna}()},
\code{\link{plot.group_tna}()},
\code{\link{plot.tna}()},
\code{\link{plot_frequencies}()},
\code{\link{plot_frequencies.group_tna}()},
\code{\link{plot_mosaic}()},
\code{\link{plot_mosaic.group_tna}()},
\code{\link{print.group_tna}()},
\code{\link{print.summary.group_tna}()},
\code{\link{print.summary.tna}()},
\code{\link{print.tna}()},
\code{\link{summary.group_tna}()},
\code{\link{summary.tna}()},
\code{\link{tna-package}}
}
\concept{basic}

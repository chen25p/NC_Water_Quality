\name{plotwq}
\alias{plotwq}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plotting measurements of a parameter over time
}
\description{
This function allows the user to plot a graph of measurements of a certain parameter
using USGS Water Quality data of a certain site. Required inputs are a site identification number,
parameter name, a title, and x & y labels.}
\usage{
plotwq("site code", "parameter name", "title", "x-value", "y-value")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{"site code"}{Each USGS location has a unique 8 to 15 digit identification
  number. They can be obtained from various sources, including the
  National Water Dashboard at https://dashboard.waterdata.usgs.gov/app/nwd/en.}
  \item{"parameter name"}{Full parameter name with spaces. The parameter
  is what is being measured and along the y-axis (i.e. "Specific conductance").}
  \item{"title"}{Title of the graph.}
  \item{"x-value"}{Label for the x-axis. This should usually be "Years".}
  \item{"y-value"}{Label for the y-axis, with units. For example:
  "Specific conductance in µS/cm at 25 °C".}
}
\details{
All arguments must be inserted with quotations around them.
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
plotwq("02091500",
       "Specific conductance",
       "Specific Conductance over Time at Pee Dee River in Rockingham, NC",
       "Year",
       "Specific conductance in µS/cm at 25 °C")

## The function is currently defined as
function (x){
library(tidyverse)
  library(remotes)
  library(HASP)
  library(dataRetrieval)
  library(dplyr)
  library(ggplot2)
  qw_data <- readWQPqw(paste0("USGS-", a), c(b))
  plot <- ggplot(data = qw_data, aes(x = ActivityStartDate, y = ResultMeasureValue)) +
    geom_point() +
    labs(title = c,
         x= d,
         y = e,
         caption = "USGS Water Quality Data, 2024.") +
    theme_bw()
  plot
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.

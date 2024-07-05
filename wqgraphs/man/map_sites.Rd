\name{map_sites}
\alias{map_sites}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Mapping sites with measurements of a parameter
}
\description{
This function allows the user to map all the locations where a certain
parameter is tested in any one of the 50 states of the U.S. using
USGS Water Quality data. Inputs required on the part of the user
are the state and the parameter code, or "Pcode". Each Pcode corresponds
with a certain parameter, whether it be physical, chemical, or other. A
full list of Pcodes can be found at:
https://help.waterdata.usgs.gov/codes-and-parameters/parameters.
}
\usage{
map_sites("Pcode", "state", "state abrev.", "title")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{"Pcode""}{Full Pcode from the USGS site in the description.}
  \item{"state""}{Full state name in lowercase, including spaces.}
  \item{"state abrev."}{State abbreviation in uppercase.}
  \item{"title"}{Title of the map displayed.}
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

}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
map_sites("00600",
"north carolina",
"NC",
"USGS Locations that Meausure Total Nitrogen in Water across North Carolina")

# The function is currently defined as
function (x)
{
library(tidyverse)
  library(remotes)
  library(HASP)
  library(dataRetrieval)
  library(dplyr)
  library(ggplot2)
  library(sf)
  # What sites measure nitrogen in North Carolina?
  parameter_sites <- whatNWISsites(stateCd = z,
                                   parameterCd = x)
  # Plot the locations of these sites
  usa <- st_as_sf(maps::map("state", fill=TRUE, plot =FALSE),
                  crs = 4269)
  sf_parameter <- st_as_sf(parameter_sites,
                           coords = c("dec_long_va", "dec_lat_va"),
                           crs = 4269)
  # Map
  ggplot() +
    geom_sf(data = usa[ usa$ID == y ,]) +
    geom_sf(data = sf_parameter) +
    xlab(NULL)+
    ylab(NULL)+
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5)) +
    labs(title = w,
         caption = "USGS Water Quality Data, 2024")
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

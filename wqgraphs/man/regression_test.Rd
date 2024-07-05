\name{regression_test}
\alias{regression_test}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Creating a linear regression from water quality data
}
\description{
This function allows the user to generate a linear regression for a given
plot with the USGS water quality data. Note that this function doesn't
graph the line, but supplies the coefficients. Required inputs include
a Pcode and the parameter name (i.e. "Specific conductance"). A
full list of Pcodes can be found at:
https://help.waterdata.usgs.gov/codes-and-parameters/parameters.
}
\usage{
regression_test("Pcode", "parameter name")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{"Pcode"}{Full Pcode from the USGS site in the description.}
  \item{"parameter name"}{Full parameter name with spaces. The parameter
  is what is being measured and along the y-axis.}
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
regression_test("02129000", "Specific conductance")

## The function is currently defined as
function (x){
  library(olsrr)
  library(remotes)
  library(HASP)
  library(dataRetrieval)
wq_data <- readWQPqw(paste0("USGS-", w),
                       c(z))
y <- wq_data$ResultMeasureValue
x <- wq_data$ActivityStartDate
ols <- lm(y ~ x, data = wq_data)
ols
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

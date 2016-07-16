\name{DemandPrice}
\alias{DemandPrice}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Price and General Propeties Given Quantity
}
\description{
Finds the prices and returns general propeties when quantities are given of various economics demand systems including Linear, Translog, CES, LES and CREMR.
}
\usage{
DemandPrice(q, parameter, method, Plot, message)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
   \item{q}{
  	the quantity vector
}
  \item{parameter}{
  	the parameters of the economics demand system. When choosing CREMR demand, it should be three dimensional, otherwise it should be two dimensional.
}
  \item{method}{
    the demand function used, can be one of Linear, Translog, CES, LES and CREMR
}
  \item{Plot}{
    a logical value indicating whether the manifold should be plotted
}
  \item{message}{
    a logical value indicating whether an important message about the computed quantity should be printed
}

}
\value{
\item{price}{
		the computed price
}
\item{sales}{
	  the total sales (revenues)
}
\item{elasticity}{
	  the elasticity of demand
}
\item{convexity}{
	  the convexity of demand
}
\item{marginal.revenue}{
	  the marginal revenues
}
}
\examples{
#Set quantity vector
quantity<-c(1,1.1,1.2)
#Use Translog Demand Function
X<-DemandPrice(quantity, c(10,0.5), "Translog", Plot=TRUE, message=TRUE)
#Return the prices
X$price
#Return the demand elasticity
X$elasticity
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
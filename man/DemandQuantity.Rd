\name{DemandQuantity}
\alias{DemandQuantity}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Quantity and General Propeties Given Price
}
\description{
Finds the quantities (outputs) and returns general propeties when prices are given of various economics demand systems including Linear, Translog, CES, LES and CREMR.
}
\usage{
DemandQuantity(p, parameter, method, Plot, message)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
   \item{p}{
  	the price vector
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
\item{quantity}{
		the computed quantity
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
#Set price vector
price<-c(1,1.1,1.2)
#Use Linear Demand Function
X<-DemandQuantity(price,c(10,0.5), "Linear", Plot=TRUE, message=TRUE)
#Return the quantities
X$quantity
#Return the marginal revenues
X$marginal.revenue
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
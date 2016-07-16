DemandQuantity<-function(p, parameter, method, Plot, message)
{
if(method=="Linear")
{
  # Given p, return q
  q<-parameter[1]/parameter[2]-p/parameter[2]
  
  # Print important note
  if(message){
    if(!(all(q>=0))){print("Note some quantities are negative")}}
  
  # Total revenue
  r<-p*q
  
  # Elasticity
  e<-p/(parameter[2]*q)
  
  # Convexity
  c<-rep(0, length(q))
  
  # Marginal revenue
  mr<-parameter[1]-2*parameter[2]*q
  
  # Manifold plot
  if(Plot)
  {
    plot(e, c, xlab="Elasticity",ylab='Convexity',main="Manifold of Linear Demand System")}
}
if(method=="LES")
{# Given p, return q
  q<-parameter[1]/p-parameter[2]
  
  # Print important note
  if(message){
    if(!(all(q>=0))){print("Note some quantities are negative")}}
  
  # Total revenue
  r<-p*q
  
  # Elasticity
  e<-1+parameter[2]/q
  
  # Convexity
  c<-(-2*q)/(q+parameter[2])
  
  # Marginal revenue
  mr<-parameter[1]/(q+parameter[2])-parameter[1]*q/(q+parameter[2])^2
  
  # Manifold plot
  if(Plot)
  {
    plot(e, c, xlab="Elasticity",ylab='Convexity',main="Manifold of LES Demand System")}
}
if(method=="CES")
{# Given p, return q
  q<-(p/parameter[1])^(-parameter[2])
  
  # Print important note
  if(message){
    if(!(all(q>=0))){print("Note some quantities are negative")}}
  
  # Total revenue
  r<-p*q
  
  # Elasticity
  e<-parameter[2]
  
  # Convexity
  c<-1/parameter[2]+1
  
  # Marginal revenue
  mr<-(1-1/parameter[2])*parameter[1]*q^(-1/parameter[2])
  
  # Manifold plot
  if(Plot)
  {
    plot(e, c, xlab="Elasticity",ylab='Convexity',main="Manifold of CES Demand System")}
}
if(method=="Translog"){
  # Given p, return q
  q<-1/p*(parameter[1]-parameter[2]*log(p))
  
  # Print important note
  if(message){
    if(!(all(q>=0))){print("Note some quantities are negative")}}
  
  # Total revenue
  r<-p*q
  
  # Elasticity
  e<-(parameter[2]+r)/r
  
  # Convexity
  c<-(parameter[1]-parameter[2]*log(p))*(2*parameter[2]+2*parameter[2]-2*parameter[2]*log(p)+parameter[2])/(parameter[2]+parameter[1]-parameter[2]*log(p))^2
  
  # Marginal revenue
  mr<-p-p^2*q/(parameter[1]+parameter[2]-parameter[2]*log(p))
  
  # Manifold plot
  if(Plot)
  {
    plot(e, c, xlab="Elasticity",ylab='Convexity',main="Manifold of Translog Demand System")}
  
}
if(method=="CREMR")
{
  # Given p, return q
  # Use inverse function to solve
  inverse = function (f, lower = -100, upper = 100) {
    function (y) uniroot((function (x) f(x) - y), lower = lower, upper = upper)[1]
  }
  price<-function(q)
  {
    parameter[1]/q*(q-parameter[2])^(1-1/parameter[3])
  }
  fr<-inverse(price,0.0001, 10^10)
  q<-rep(NA,length(p))
  for(i in 1:length(p))
  {
    q[i]<-fr(p[i])[[1]]
  }
  
  
  # Print important note
  if(message){
    if(!(all(q>=0))){print("Note some quantities are negative")}}
  
  # Total revenue
  r<-p*q
  
  # Elasticity
  e<-(q-parameter[2])*parameter[3]/(q-parameter[2]*parameter[3])
  
  # Convexity
  c<-2-1/(parameter[3]-1)*(e-1)^2/e
  
  # Marginal revenue
  mr<-parameter[1]*(1-1/parameter[3])*(q-parameter[2])^(-1/parameter[3])
  
  # Manifold plot
  if(Plot)
  {
    plot(e, c, xlab="Elasticity",ylab='Convexity',main="Manifold of CREMR Demand System")}
  
  
}
  
object<-list(quantity=q, sales=r, elasticity=e, convexity=c, marginal.revenue=mr)
object
  
}
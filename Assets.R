
AssetsTickerDay = function(ticker,time){
  #CvQbcdE7syVkOIP5dAqccpOeK2o0iup4
  #CBQouP6k8C92g2XWhofZB5PCGgxI6lOk
  API = paste0("https://api.polygon.io/v2/aggs/ticker/",ticker, "/range/1/minute/")
  API = paste0(API,format(time,"%Y-%m-%d"),"/",format(time,"%Y-%m-%d"))
  API = paste0(API,"?adjusted=true&sort=asc&limit=5000&apiKey=CBQouP6k8C92g2XWhofZB5PCGgxI6lOk")
  raw = jsonlite::fromJSON(API)
  Data = raw$results
  if(length(Data)==1){ print("Holiday");  return(matrix(0,0,3)) }
  Data = cbind(Data$t,Data$vw,Data$v)
  return(Data)
}

AssetsTickerMonth = function(ticker,month){
  mon = as.POSIXlt(month)$mon;  start = 0; end = 0;  w = TRUE
  while(w){
    w = FALSE
    if(mon == as.POSIXlt(month+start)$mon){ start = start - 1;  w = TRUE }
    if(mon == as.POSIXlt(month+end)$mon){ end = end + 1;  w = TRUE }
  }
  Data = matrix(0,0,3)
  s = as.POSIXlt(month+start+1)$wday-start-1
  for(i in (start+1):(end-1)){
    if(((i+s)%%7==0) | ((i+s)%%7==6)){ next }
    print(month+i)
    Data = rbind(Data,AssetsTickerDay(ticker,month+i))
    Sys.sleep(12)
  }
  return(Data)
}

AssetsRead = function(year){
  Assets = list()
  for(i in c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")){
    try({
      Assets = append(Assets,list(readRDS(paste0("Assets/Assets",year,i))))
    }, silent=TRUE)
  }
  return(Assets)
}

AssetsSum = function(Assets,step){
  m = length(Assets);  Assets.New = list()
  for(i in 1:m){
    d = length(Assets[[i]][1,]);  n = length(Assets[[i]][,1])
    Assetstep = matrix(0,n%/%step,d);  S = numeric(d-1);  k = 0
    for(j in 1:n){
      S = S + Assets[[i]][j,-1];  k = k + 1
      if(k >= step){ Assetstep[j%/%step,-1] = S;  k = 0;  S = numeric(d-1) }
    }
    Assetstep[,1] = Assets[[i]][10*(1:(n%/%step))-9,1]
    Assets.New = append(Assets.New,list(Assetstep))
  }
  return(Assets.New)
}

AssetsCombine(AssetsRead("22"))


A = AssetsRead("22")[[1]][[2]]

A[-1,2] = diff(log(A[,2]))
A = A[-1,]

NPC = kropula::NPVC(A[,2,drop=FALSE],1)
NPCVol = kropula::NPVC(A[,3:2,drop=FALSE],1)

D = (kropula::NPVCSample(100000,99,NPC)%*%rep(1,100))[1:100000]
DVol = kropula::NPVCSample(100000,99,NPCVol)[,2*(1:100)]%*%rep(1,100)
DVol = DVol[!is.na(DVol)]

sum((D-mean(D))^4/100000)/var(D)^2
sum((DVol-mean(DVol))^4/100000)/var(DVol)^2
hist(DVol)
B = AssetsRead("22")[[2]][[2]]
B[-1,2] = diff(log(B[,2]))
B = B[-1,]
BVarVol = matrix(0,nrow(B)%/%10-1,3)

plot(B)


for(i in 2:nrow(BVarVol)){
  BVarVol[i-1,1] = sum(B[10*i-10+1:10,2])
  BVarVol[i-1,2] = sort(kropula::NPVCSample(10000,9,NPCVol,B[10*i-10,3:2])[,2*(1:10)]%*%rep(1,10))[500]
  BVarVol[i-1,3] = sort(kropula::NPVCSample(10000,9,NPC,B[10*i-10,2])[,1:10]%*%rep(1,10))[500]
  print(i)
}
sum(is.na(kropula::NPVCSample(10000,9,NPCVol)[,2*(1:10)]%*%rep(1,10)))
sqrt(var(kropula::NPVCSample(10000,9,NPCVol)[,2*(1:10)]%*%rep(1,10)))

cor(A[-1,2],A[-7556,2])

acf(A[,2])
acf(B[,2])

plot(B[-c(1,2),2],B[-c(16460,16459),2])

sqrt(var(B[,2]))
sqrt(var(BVarVol[,1]))

hist(BVarVol[,1],100)

plot(BVarVol[,1],type="l")
lines(BVarVol[,2],col="red")
lines(BVarVol[,3],col="blue")


SimVAR = function(n,mu,Sigma,Rho){
  k = length(mu);  r = matrix(rnorm(n*k),n,k)
  r = r%*%chol(Sigma);  mean = VaR = ES = matrix(0,n,k)
  r[1,] = r[1,] + mu;  mean[1,] = mu;  VaR[1,] = mu+diag(Sigma)*qnorm(0.05);  ES[1,] = mu - diag(Sigma)*dnorm(qnorm(0.05))/0.05
  for(i in 2:n){
    r[i,] = r[i-1,]%*%Rho + r[i,] + mu
    mean[i,] = r[i-1,]%*%Rho + mu;
    VaR[i,] = mean[i,]+diag(Sigma)*qnorm(0.05);  ES[i,] = mean[i,] - diag(Sigma)*dnorm(qnorm(0.05))/0.05
  }
  return(list(r,mean,VaR,ES))
}

EstVAR = function(X){
  n = nrow(X)
  mu = rep(0,ncol(X))
  A = solve(t(X[-n,])%*%X[-n,])%*%t(X[-n,])%*%X[-1,]
  r = X[-1,]-X[-n,]%*%A
  Sigma = var(r)
  return(list(mu,A,Sigma))
}


AR = SimVAR(10000,rep(0,2),matrix(c(1,0.5,0.5,1),2,2),matrix(c(0.7,0.2,0.5,0.3),2,2))

plot(AR[[1]][1:1000,1],type="l")
lines(AR[[2]][1:1000,1],col="red")
lines(AR[[3]][1:1000,1],col="blue")

sum(AR[[1]][,1]<AR[[3]][,1])


NPCVAR = kropula::NPVC(AR[[1]],1)

Ahat = EstVAR(AR[[1]])

VaRhat = numeric(1000)
VaRNPC = numeric(1000)

for(i in 2:1000){
  VaRhat[i] = (AR[[1]][i-1,]%*%Ahat[[2]] + Ahat[[1]]+diag(Ahat[[3]])*qnorm(0.05))[2]
  VaRNPC[i] = sort(kropula::NPVCSample(10000,1,NPCVAR,AR[[1]][i-1,])[,4])[500]
  print(i)
}

plot(AR[[1]][1:1000,1],type="l")
lines(AR[[3]][1:1000,1],col="red")
lines(VaRhat[1:1000],col="blue")
lines(VaRNPC[1:1000],col="green")

sum(AR[[1]][1:1000,1]<AR[[3]][1:1000,1])
sum(AR[[1]][1:1000,1]<VaRhat[1:1000])
sum(AR[[1]][1:1000,1]<VaRNPC[1:1000])

sqrt(sum((AR[[3]][1:1000,1]-VaRhat[1:1000])^2)/1000)
sqrt(sum((AR[[3]][1:1000,1]-VaRNPC[1:1000])^2)/1000)
sqrt(sum((VaRNPC[1:1000]-VaRhat[1:1000])^2)/1000)





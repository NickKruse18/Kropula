
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
    if(mon == as.POSIXlt(month+start)$mon){
      start = start - 1;  w = TRUE
    }
    if(mon == as.POSIXlt(month+end)$mon){
      end = end + 1;  w = TRUE
    }
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

AssetsAlign = function(Assets){
  d = length(Assets);  times = cbind(Assets[[1]][,1],Assets[[1]][,2],matrix(0,length(Assets[[1]][,1]),d-1))
  for(i in 2:d){
    t = 1;  D = Assets[[i]];  j = 1
    while(j <= length(D[,1])){
      if(times[t,1] == D[j,1]){ times[t,1+i] = D[j,2];  t = t + 1;  j = j + 1 }
      else if(times[t,1] < D[j,1]){ t = t + 1 }
      else if(times[t,1] > D[j,1]){
        times = rbind(times[1:(t-1),],c(D[j,1],numeric(i-1),D[j,2],numeric(d-i)),times[(t:length(times[,1])),])
        t = t + 1;  j = j + 1
      }
    }
  }
  return(times)
}

AssetsSame = function(Assets){
  Assets[is.na(Assets)] = 0
  Assets[,-1] = log(Assets[,-1]);  Assets[,1] = cumsum(Assets[,1])
  Miss = c(0,(1:length(Assets[,1]))[Assets[,-1]%*%rep(1,length(Assets[1,])-1)==-Inf],length(Assets[,1])+1)
  DiffAssets = Assets;  ind = 0
  for(i in 1:(length(Miss)-1)){
    if(Miss[i]+2 >= Miss[i+1]){ next }
    Ind = (ind+1):(ind+Miss[i+1]-Miss[i]-2)
    DiffAssets[Ind,] = diff(Assets[(Miss[i]+1):(Miss[i+1]-1),])
    ind = ind + Miss[i+1] - Miss[i] - 2
  }
  DiffAssets = DiffAssets[1:ind,]
  return(DiffAssets)
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

AssetsCombine = function(Assets){
  d = length(Assets);  Assets.New = list()
  for(i in 1:d){
    Assets.New = append(Assets.New,list(AssetsSame(AssetsAlign(Assets[[i]]))))
  }
  return(Assets.New)
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

ls()

#Assets.Mar21 = list()
#for(i in 1:12){
#  Assets.Mar21 = append(Assets.Mar21,list(AssetsTickerMonth(I[i],as.Date("2021-03-01"))))
#}

#saveRDS(Assets.Mar21,"AssetsMar21")


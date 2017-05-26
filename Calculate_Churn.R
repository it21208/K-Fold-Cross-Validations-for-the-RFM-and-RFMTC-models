######### calculate churn - seasonal random walk ###########
cdf=read.table("C:/Users/alex/Documents/R/Projects/RFM CDNOW Implementation/CDNOW_master/CDNOW_master.txt",header=FALSE)
cdf=as.data.frame(cbind((cdf[,c(1,2)]), rep(0,times=length(cdf[,1])))); 
colnames(cdf)=c("id","char_date","date")
cdf$char_date=as.character(cdf$char_date)
cdf$date=as.Date(cdf$char_date,"%Y%m%d") 
cdf$char_date=format.Date(cdf$date,"%Y%d%m") 
totalChurnZones=9
dates_vector=seq(min(cdf$date), max(cdf$date),length.out=totalChurnZones)
dates_vector[c(totalChurnZones)]=as.Date(dates_vector[c(totalChurnZones)])+1
totalCustomers=length(unique(cdf$id))
mat_id_churn=matrix(data=0,nrow=totalCustomers,ncol=length(dates_vector)-1,byrow=FALSE,dimnames=NULL)
colnames(mat_id_churn)=as.character(dates_vector[1:length(dates_vector)-1])
i<-1; while (dates_vector[i] < (tail(dates_vector, n=1))){
for (j in 1:length(cdf$id)){if ((dates_vector[i]<=cdf[j,3])&&(cdf[j,3]<dates_vector[i+1])){mat_id_churn[cdf[j,1],i]=1}}
i=i+1}
df_id_churn=as.data.frame(mat_id_churn)
v=sapply(1:totalCustomers, function(x){round(sum(df_id_churn[x,])/(totalChurnZones-1))})
write.csv(v,"CDNOW_master_churn.txt",row.names = TRUE)

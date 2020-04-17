
library(EpiEstim)

names(sa.cs)
file.choose()
sa.cs=read.csv("C:\\Users\\bvalc\\Documents\\south america.csv",head=T,sep=",")
View(sa.cs)

extract.countries <- function (df, country.name) {
 df2<- df%>%
  filter(countries==country.name)
  return(df2)
}

rt.estimate <- function (time.country){
 res <- estimate_R (incid = time.country, 
                   method = "parametric_si",
                   config = make_config(list( 
                    mean_si = 3.96, std_si = 4.75,
                     t_start = 2:(nrow(time.country)-5), 
                     t_end = 7:nrow(time.country))))
  return(res)
 }


rt.estimate.SA <- function(df) {
  countries <- unique(df$countries)
  output <- vector(mode="list", length=length(countries))
  for (country in countries) {
    print(country)
    country.epid <- extract.countries(df, country)
    output[[country]] <- rt.estimate(country.epid$cases)
  }
  return(output)
}




t = function (df) {
 tab<-data.frame(df$R$t_start, df$R$t_end,df$R$Mean, df$R$Quantile.0.025, df$R$Quantile.0.25)
 names(tab)<-c("day.start","day.end","Rt","lower","upper")
 tail(tab,1)
return (tab)
}

str(sa.cs)

bra=sa.cs%>%
 filter(countries=="Brazil")%>%
 select(cases)%>%
 rt.estimate()

per=sa.cs%>%
 filter(countries=="Peru")%>%
 select(cases)%>%
 rt.estimate()

arg=sa.cs%>%
 filter(countries=="Argentina")%>%
 select(cases)%>%
 rt.estimate()

bol=sa.cs%>%
 filter(countries=="Bolivia")%>%
 select(cases)%>%
 rt.estimate()

chi=sa.cs%>%
 filter(countries=="Chile")%>%
 select(cases)%>%
 rt.estimate()

uru=sa.cs%>%
 filter(countries=="Uruguay")%>%
 select(cases)%>%
 rt.estimate()

par=sa.cs%>%
 filter(countries=="Paraguay")%>%
 select(cases)%>%
 rt.estimate()

ven=sa.cs%>%
 filter(countries=="Venezuela")%>%
 select(cases)%>%
 rt.estimate()

col=sa.cs%>%
 filter(countries=="Colombia")%>%
 select(cases)%>%
 rt.estimate()

ecu=sa.cs%>%
 filter(countries=="Ecuador")%>%
 select(cases)%>%
 rt.estimate()

write.csv (sa.cs, file = "south america.csv")

br<-tail(t(bra),1)
ch<-tail(t(chi),1)
ec<-tail(t(ecu),1)
ar<-tail(t(arg),1)
pe<-tail(t(per),1)
co<-tail(t(col),1)
pa<-tail(t(par),1)
bo<-tail(t(bol),1)
ur<-tail(t(uru),1)
ve<-tail(t(ven),1)

country<-c("braz","chil","ecua","arge","peru","colo","para","bolo","urug","vene")

rt<-rbind(br,ch,ec,ar,pe,co,pa,bo,ur,ve)
rt<-cbind(country,rt)
str(rt)

write.table(rt, file="rt countries.txt", sep=",",
            ,row.names=T,quote=F)




#########################


br<-t(bra)
ch<-t(chi)
ec<-t(ecu)
ar<-t(arg)
pe<-t(per)
co<-t(col)
pa<-t(par)
bo<-t(bol)
ur<-t(uru)
ve<-t(ven)

library(ggplot2)
plot(bra)
length(br$Rt)
str(br)
br
names(br)

br

ggplot(br, aes(x=day.end,y=Rt))+
 geom_line(size=1)+
 geom_ribbon(aes(ymin = lower, ymax = upper),alpha = .25)+
 geom_hline(yintercept=1,linetype="dashed",size=0.8)+
 theme_bw()+
 xlab("Days")+
 ylab("Time-varying \n reproduction number")
 





















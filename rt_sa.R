library(dplyr)
library(tidyr)
library(EpiEstim)

names(sa.cs)
file.choose()
#sa.cs=read.csv("C:\\Users\\bvalc\\Documents\\south america.csv",head=T,sep=",")
sa.cs<-read.csv("south america.csv",head=T,sep=",")
View(sa.cs)

extract.countries <- function (df, country.name) {
  df2 <- df%>%
        filter(countries==country.name)
  return(df2)
}

rt.estimate <- function (time.country) {
  epid.time <- nrow(time.country)
  # country.config contains parameters to estimate the
  # 5-day rolling window Rt, based on a serial interval
  # following a Gamma distribution with mean 3.96 and sd 4.75
  country.config <- make_config(list( 
                                mean_si = 3.96, 
                                std_si = 4.75,
                                t_start = 2:(epid.time-5), 
                                t_end = 7:epid.time))
  res <- estimate_R (incid = time.country, 
                     method = "parametric_si",
                     config = country.config)
  return(res)
}


rt.estimate.SA <- function(df) {
  countries <- unique(df$countries)
  output <- vector(mode="list", length=length(countries))
  for (country in countries) {
    country.epid <- extract.countries(df, country)
    output[[country]] <- rt.estimate(country.epid$cases)
  }
  return(output)
}




t = function (df) {
  tab <- data.frame(df$R$t_start, 
                    df$R$t_end,
                    df$R$Mean, 
                    df$R$Quantile.0.025, 
                    df$R$Quantile.0.25)
  names(tab) <- c("day.start","day.end","Rt","lower","upper")
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

write.table(rt, file="rt countries.txt", sep=","
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

na.pad <- function(x,len){
  x[1:len]
}

pad.len <- nrow(br)

#Rt time series
rt.ts <- data.frame(br$day.end,
                    br$Rt,
                    na.pad(ch$Rt, pad.len),
                    na.pad(ec$Rt, pad.len),
                    na.pad(ar$Rt, pad.len),
                    na.pad(pe$Rt, pad.len),
                    na.pad(co$Rt, pad.len), 
                    na.pad(pa$Rt, pad.len),
                    na.pad(bo$Rt, pad.len),
                    na.pad(ur$Rt, pad.len),
                    na.pad(ve$Rt, pad.len))

#Lower bound for 95% CrI
rt.cri.lb.ts <- data.frame(br$day.end,
                           br$lower,
                           na.pad(ch$lower, pad.len),
                           na.pad(ec$lower, pad.len),
                           na.pad(ar$lower, pad.len),
                           na.pad(pe$lower, pad.len),
                           na.pad(co$lower, pad.len), 
                           na.pad(pa$lower, pad.len),
                           na.pad(bo$lower, pad.len),
                           na.pad(ur$lower, pad.len),
                           na.pad(ve$lower, pad.len))

#Upper bound for 95% CrI
rt.cri.ub.ts <- data.frame(br$day.end,
                           br$upper,
                           na.pad(ch$upper, pad.len),
                           na.pad(ec$upper, pad.len),
                           na.pad(ar$upper, pad.len),
                           na.pad(pe$upper, pad.len),
                           na.pad(co$upper, pad.len), 
                           na.pad(pa$upper, pad.len),
                           na.pad(bo$upper, pad.len),
                           na.pad(ur$upper, pad.len),
                           na.pad(ve$upper, pad.len))
ts.headers <- c("Day",
                "Brazil",
                "Chile",
                "Ecuador",
                "Argentina",
                "Peru",
                "Colombia",
                "Paraguay",
                "Bolivia",
                "Uruguay",
                "Venezuela")

names(rt.ts) <- ts.headers
names(rt.cri.lb.ts) <- ts.headers
names(rt.cri.ub.ts) <- ts.headers


rt.ts.no.day <- rt.ts[, 2:11]

melt.ts <- rt.ts %>%
  select(Day, Brazil, Chile, Ecuador, Argentina, Peru, Colombia, Paraguay, Bolivia, Uruguay, Venezuela) %>%
  gather(key = "Country", value="Rt", -Day)

melt.lb.ts <- rt.cri.lb.ts %>%
  select(Day, Brazil, Chile, Ecuador, Argentina, Peru, Colombia, Paraguay, Bolivia, Uruguay, Venezuela) %>%
  gather(key = "Country", value="Lower", -Day)

melt.ub.ts <- rt.cri.lb.ts %>%
  select(Day, Brazil, Chile, Ecuador, Argentina, Peru, Colombia, Paraguay, Bolivia, Uruguay, Venezuela) %>%
  gather(key = "Country", value="Upper", -Day)

melt.all.ts <- merge(x=melt.ts, y=melt.lb.ts, by=c("Day", "Country"), all.x = TRUE)
melt.all.ts <- merge(x=melt.all.ts, y=melt.ub.ts, by=c("Day", "Country"), all.x = TRUE)

ggplot(melt.ts, aes(x = Day, y = Rt)) + 
  geom_line(aes(color = Country), size = 0.5) +
  xlab("Days since first case") +
  labs(y=expression(R[t][])) +
  expand_limits(x=0) + 
  theme_minimal()

ggplot(melt.all.ts, aes(x = Day, y = Rt)) +
  geom_line(size = 0.5) +
  facet_wrap(~Country, nrow=2, ncol=5) +
  geom_ribbon(aes(ymin = Lower, ymax = Upper, group=Country), alpha = .25) +
  xlab("Days since first case") +
  labs(y=expression(R[t][])) +
  geom_hline(yintercept=1,linetype="dashed",size=0.5, alpha = 0.7) +
  expand_limits(x=0) + 
  theme_minimal()



head(melt.ts)

plot.ts(rt.ts.no.day)

library(ggplot2)
plot(bra)
length(br$Rt)
str(br)
br
names(br)

br

ggplot(br, aes(x=day.end,y=Rt))+
 geom_line(size=1)+
 geom_ribbon(aes(ymin = lower, ymax = upper),alpha = .25) +
 geom_hline(yintercept=1,linetype="dashed",size=0.8)+
 theme_bw()+
 xlab("Days")+
 ylab("Time-varying \n reproduction number")
 


br



















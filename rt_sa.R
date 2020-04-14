
library(EpiEstim)

names(sa.cs)

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
                     t_end = 7:nrow(time.country)))
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


rt.estimate.SA (sa.cs)





bra=sa.cs%>%
 filter(countries=="Brazil")%>%
 select(cases)
%>%
 rt.estimate()

per=sa.cs%>%
 filter(countries=="Peru")%>%
 select(cases)

%>%
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





per[R]

 R$t_start

 df$t_end, 

per$R$Mean, R$Quantile.0.025, R$Quantile.0.25











unique(sa.cs$countries)


res<- estimate_R(incid = peru$cases,
  method = "parametric_si",
  config = make_config(list(
           mean_si = 3.96, std_si = 4.75,
  t_start = 2:33, t_end=7:38)))


res
plot(res)



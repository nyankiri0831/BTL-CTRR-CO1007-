library(lubridate)
library(tidyverse)
library(Hmisc)
library(data.table)
library(gridExtra)

#data_input_global_declare
rm(list = ls(all.names = TRUE))
source <- read.csv(file.choose())
data <- source[source[,2]!="",]
data[,5] <- abs(data[,5])
data[,6] <- abs(data[,6])
options("scipen"=10)

world_data <- subset(source, source$location == "World")
iso_code <- c(data$iso_code)
continent <- c(data$continent)
new_cases <- c(data$new_cases)
death_cases <- c(data$new_deaths)

i <- function()
{
    #i1
    date_format <- data
    date_format$date <- as.POSIXct(date_format$date , format = "%m/%d/%Y")
    outputi1 <- rbind(unique(format(date_format$date, format = "%Y")))
    View(outputi1)
    
    #i2
    iso_code1 <- unique(data[,1])
    location1 <- unique(data[,3])
    output_i2 <- rbind(cbind(iso_code1[1:10],
                             location1[1:10]),
                       cbind("Count",length(iso_code1)))
    colnames(output_i2) <- c("iso_code","location")
    View(output_i2)
    
    #i3
    conti <- cbind(unique(data$continent))
    conti_trans <- rbind("Chau A","Chau Au",
                         "Chau Phi","Bac Mi",
                         "Nam Mi","Chau Dai Duong")
    output_i3 <- data.frame(
      conti,
      conti_trans
    )
    output_i3 = rbind(c("Continent",length(conti)),output_i3)
    View(output_i3)
    
    #i4
    con_data <- as.numeric(table(data$continent))
    con <- sort(unique(data$continent))
    sum <- c("Tong:",sum(con_data))
    con_data <- data.frame(rbind(cbind(con,con_data),sum))
    colnames(con_data) <- c("Continent:", "Observations")
    rownames(con_data) <- c(1:nrow(con_data))
    View(con_data)
    
    #i5
    con_data <- as.numeric(table(data$iso_code))
    coun <- sort(unique(data$iso_code))
    sum <- c("Tong:",sum(con_data))
    con_data <- data.frame(rbind(cbind(coun,con_data),sum))
    colnames(con_data) <- c("iso_code", "Observations")
    rownames(con_data) <- c(1:nrow(con_data))
    View(tail(con_data, n = 11))
 
    #i8 9
    coun_data <- as.numeric(table(data$location))
    coun <- sort(unique(data$location))
    coun_data <- data.frame(cbind(coun,coun_data))
    colnames(coun_data) <- c("Country", "Observations")
    rownames(coun_data) <- c(1:nrow(coun_data))
    coun_data[,2] <- as.numeric(coun_data[,2])
    View(rbind(subset(coun_data, 
                      Observations == min(coun_data$Observations)),
               subset(coun_data, 
                      Observations == max(coun_data$Observations))))
    
    #i10 11
    date <- c(data$date)
    iso1_code <- c(data$iso_code)
    Date <- data.frame(iso1_code,date)
    
    Date <- Date %>% arrange(mdy(Date$date))
    df <- data.frame(setDT(Date)[,list(numData=.N),Date$date])
    #i10
    output_i10 = subset(df, numData == min(numData))
    View(output_i10)
    #i11
    output_i11 = subset(df, numData == max(numData))
    View(output_i11)
    
    #i12 13 14
    df <- data.frame(data)
    df <- subset(df, continent != "")
    date_sort <- df %>% arrange(mdy(df$date))
    #View(date_sort)
    output_i12 <- date_sort %>% group_by(date, continent) %>% summarise(value = n())
    output_i12 <- output_i12 %>% arrange(mdy(output_i12$date))
    #i12
    View(output_i12)
    #i13
    output_i13 <- subset(output_i12, value == max(value))
    View(output_i13)
    #i14
    output_i14 <- subset(output_i12, value == min(value))
    View(output_i14)
    
    #15
    df <- data.frame(iso_code, continent,date, new_cases,death_cases)
    df$date<-as.Date(df$date,format="%m/%d/%Y") 
    k=readline(prompt="Enter the k date with format %Y/%m/%d: ")
    t=readline(prompt="Enter the t continent: ")
    datefilter = as.Date(k)
    df <- df %>% filter(continent == t &date == datefilter)
    print(df)
    
    #i16
    sub_con_data <- subset(con_data,duplicated(con_data[,2])|
                             duplicated(con_data[,2],fromLast=TRUE))
    sub_con_data <- sub_con_data[order(sub_con_data[,2]),]
    View(sub_con_data)
    
    #i17
    temp <- cbind(unique(data$iso_code),unique(data$location))
    output_i17 <- temp[nchar(unique(data$iso_code))>3,]
    View(output_i17)
}

ii <- function()
{
  #ii1 -> 6
  ii <- function(subdata, col)
  {
    sum <- summary(subdata[,col])
    min <- as.numeric(sum[1])
    max <- as.numeric(sum[6])
    Q1 <- as.numeric(sum[2])
    Q2 <- as.numeric(sum[3])
    Q3 <- as.numeric(sum[5])
    avg <- as.numeric(sum[4])
    std <- sd(subdata[,col],na.rm = TRUE)
    outlier <- 0
    for(i in 1:nrow(subdata))
    {
      if(is.na(subdata[i,col])) next
      if(subdata[i,col] < (Q1 - (1.5*(Q3 - Q1))) || 
         subdata[i,col] > (Q3 + (1.5*(Q3 - Q1))))
      {
        outlier <- outlier + 1
      }
      else {}
    }
    return(as.numeric(c(min,Q1,Q2,Q3,max,avg,std,outlier)))
  }
  dfCountry <- data.frame(Countries = c("Indonesia", 
                                        "Japan", "Vietnam"))
  id_data <- subset(data, location == "Indonesia")
  jp_data <- subset(data, location == "Japan")
  vn_data <- subset(data, location == "Vietnam")
  
  tmp <- data.frame(rbind(ii(id_data,5),ii(jp_data,5),ii(vn_data,5)))
  colnames(tmp) <- c("Min", "Q1", "Q2", "Q3",
                     "Max", "Avg", "Std", "Outlier")
  dfCountry_case <- cbind(dfCountry,tmp)
  View(dfCountry_case)
  
  tmp <- data.frame(rbind(ii(id_data,6),ii(jp_data,6),ii(vn_data,6)))
  colnames(tmp) <- c("Min", "Q1", "Q2", "Q3", 
                     "Max", "Avg", "Std", "Outlier")
  dfCountry_death <- cbind(dfCountry,tmp)
  View(dfCountry_death)
  
  #ii7
  boxplot(id_data[,5], jp_data[,5], vn_data[,5],
          main = "Plotbox for new cases",
          at = c(1,2,3),
          names = c("Indonesia", "Japan", "Vietnam"),
          col = c("orange","red","yellow"),
          border = "black",
          horizontal = TRUE,
          notch = TRUE
  )
  boxplot(id_data[,6], jp_data[,6], vn_data[,6],
          main = "Plotbox for new deaths",
          at = c(1,2,3),
          names = c("Indonesia", "Japan", "Vietnam"),
          col = c("orange","red","yellow"),
          border = "black",
          horizontal = TRUE,
          notch = TRUE
  )
}

iii <- function()
{
  indo <- data[data[,1]=="IDN",]
  vn <- data[data[,1]=="VNM",]
  jp <- data[data[,1]=="JPN",]
  
  #iii1
  nnreportcindo <- indo[is.na(indo[,5]) | indo[,5]==0,]
  indo1.1 <- nrow(nnreportcindo)
  view(indo1.1)
  nnreportdindo <- indo[is.na(indo[,6]) | indo[,6]==0,]
  indo1.2 <- nrow(nnreportdindo)
  view(indo1.2)
  
  nnreportcvn <- vn[is.na(vn[,5]) | vn[,5]==0,]
  vn1.1 <- nrow(nnreportcvn)
  view(vn1.1)
  nnreportdvn <- indo[is.na(vn[,6]) | vn[,6]==0,]
  vn1.2 <- nrow(nnreportdvn)
  view(vn1.1)
  
  nnreportcjp <- jp[is.na(jp[,5]) | jp[,5]==0,]
  jp1.1 <- nrow(nnreportcjp)
  view(jp1.1)
  nnreportdjp <- jp[is.na(jp[,6]) | jp[,6]==0,]
  jp1.2 <- nrow(nnreportdjp)
  view(jp1.1)
  
  #iii2
  newreportindo <- indo[!is.na(indo[,5]) & !is.na(indo[,6]) 
                        & indo[,5]!=0 & indo[,6]!=0,]
  newreportvn <- vn[!is.na(vn[,5]) & !is.na(vn[,6]) 
                    & vn[,5]!=0 & vn[,6]!=0,]
  newreportjp <- jp[!is.na(jp[,5]) & !is.na(jp[,6]) 
                    & jp[,5]!=0 & jp[,6]!=0,]
  newreport <- rbind(newreportindo,newreportvn,newreportjp)
  
  indominc <- newreportindo[newreportindo[,5]==min(newreportindo[,5]),4]
  indo2.1 <- length(indominc)
  view(indo2.1)
  indomind <- newreportindo[newreportindo[,6]==min(newreportindo[,6]),4] 
  indo2.2 <- length(indomind)
  view(indo2.2)
  
  vnminc <- newreportvn[newreportvn[,5]==min(newreportvn[,5]),4] 
  vn2.1 <- length(vnminc)
  view(vn2.1)
  vnmind <- newreportvn[newreportvn[,6]==min(newreportvn[,6]),4] 
  vn2.2 <- length(vnmind)
  view(vn2.2)
  
  jpminc <- newreportjp[newreportjp[,5]==min(newreportjp[,5]),4] 
  jp2.1 <- length(jpminc)
  view(jp2.1)
  jpmind <- newreportjp[newreportjp[,6]==min(newreportjp[,6]),4] 
  jp2.2 <- length(jpmind)
  view(jp2.2)
  
  #iii3
  indomaxc <- newreportindo[newreportindo[,5]==max(newreportindo[,5]),4] 
  indo3.1 <- length(indomaxc)
  view(indo3.1)
  indomaxd <- newreportindo[newreportindo[,6]==max(newreportindo[,6]),4] 
  indo3.2 <- length(indomaxd)
  view(indo3.2)
  
  vnmaxc <- newreportvn[newreportvn[,5]==max(newreportvn[,5]),4] 
  vn3.1 <- length(vnmaxc)
  view(vn3.1)
  vnmaxd <- newreportvn[newreportvn[,6]==max(newreportvn[,6]),4] 
  vn3.2 <- length(vnmaxd)
  view(vn3.2)
  
  jpmaxc <- newreportjp[newreportjp[,5]==max(newreportjp[,5]),4] 
  jp3.1 <- length(jpmaxc)
  view(jp3.1)
  jpmaxd <- newreportjp[newreportjp[,6]==max(newreportjp[,6]),4] 
  jp3.2 <- length(jpmaxd)
  view(jp3.2)
  
  #iii4
  nnreport <- rbind(indo[is.na(indo[,5]) | is.na(indo[,6]) 
                         | indo[,6]==0 | indo[,5]==0,],
                    vn[is.na(vn[,5]) | is.na(vn[,6]) 
                       | vn[,5]==0 | vn[,6] ==0,],
                    jp[is.na(jp[,5]) | is.na(jp[,6]) 
                       | jp[,5]==0 | jp[,6]==0,]) 
  iii4.1 <- nnreport[,-c(1,2,4)]
  colnames(iii4.1) <- c("Countries","Infections value","Deaths value")
  view(iii4.1)
  iii4.2 <- newreport[,-c(1,2,4)]
  colnames(iii4.2) <- c("Countries","Infections value","Deaths value")
  view(iii4.2)
  
  #iii5
  indo5_case <- 0
  for (i in 1:nrow(indo)){
    if (is.na(indo[i,5])){
      indo5_case <- indo5_case + 1
      break
    }
  }
  view(indo5_case)
  indo5_death <- 0
  for (i in 1:nrow(indo)){
    if (is.na(indo[i,6])){
      indo5_death <- indo5_death + 1
      break
    }
  }
  view(indo5_death)
  
  vn5_case <- 0
  for (i in 1:nrow(vn)){
    if (is.na(vn[i,5])){
      vn5_case <- vn5_case + 1
      break
    }
  }
  view(vn5_case)
  vn5_death <- 0
  for (i in 1:nrow(vn)){
    if (is.na(vn[i,6])){
      vn5_death <- vn5_death + 1
      break
    }
  }
  view(vn5_death)
  
  jp5_case <- 0
  for (i in 1:nrow(jp)){
    if (is.na(jp[i,5])){
      jp5_case <- jp5_case + 1
      break
    }
  }
  view(jp5_case)
  jp5_death <- 0
  for (i in 1:nrow(jp)){
    if (is.na(jp[i,6])){
      jp5_death <- jp5_death + 1
      break
    }
  }
  view(jp5_death)
  
  #iii6
  indo6_case <- 0
  temp <- 0
  for (i in 1:nrow(indo)){
    if (is.na(indo[i,5])){
      temp <- temp + 1
      if (temp>indo6_case) {
        indo6_case <- temp
      }
    }
    else{
      temp <- 0
    }
  }
  view(indo6_case)
  indo6_death <- 0
  temp <- 0
  for (i in 1:nrow(indo)){
    if (is.na(indo[i,6])){
      temp <- temp + 1
      if (temp>indo6_death) {
        indo6_death <- temp
      }
    }
    else{
      temp <- 0
    }
  }
  view(indo6_death)
  
  vn6_case <- 0
  temp <- 0
  for (i in 1:nrow(vn)){
    if (is.na(vn[i,5])){
      temp <- temp + 1
      if (temp>vn6_case) {
        vn6_case <- temp
      }
    }
    else{
      temp <- 0
    }
  }
  view(vn6_case)
  vn6_death <- 0
  temp <- 0
  for (i in 1:nrow(vn)){
    if (is.na(vn[i,6])){
      temp <- temp + 1
      if (temp>vn6_death) {
        vn6_death <- temp
      }
    }
    else{
      temp <- 0
    }
  }
  view(vn6_death)
  
  jp6_case <- 0
  temp <- 0
  for (i in 1:nrow(jp)){
    if (is.na(jp[i,5])){
      temp <- temp + 1
      if (temp>jp6_case) {
        jp6_case <- temp
      }
    }
    else{
      temp <- 0
    }
  }
  view(jp6_case)
  jp6_death <- 0
  temp <- 0
  for (i in 1:nrow(jp)){
    if (is.na(jp[i,6])){
      temp <- temp + 1
      if (temp>jp6_death) {
        jp6_death <- temp
      }
    }
    else{
      temp <- 0
    }
  }
  view(jp6_death)
  
  #iii7
  indo7 <- 0
  for (i in 1:nrow(nnreportcindo)){
    if (nnreportcindo[i,5]==0){
      indo7 <- indo7 + 1
      break
    }
  }
  view(indo7)
  
  vn7 <- 0
  for (i in 1:nrow(nnreportcvn)){
    if (nnreportcvn[i,5]==0){
      vn7 <- vn7 + 1
      break
    }
  }
  view(vn7)
  
  jp7 <- 0
  for (i in 1:nrow(nnreportcjp)){
    if (nnreportcindo[i,5]==0){
      jp7 <- jp7 + 1
      break
    }
  }
  view(jp7)
  
  #iii8
  indo8 <- 0
  temp <- 0
  for (i in 1:nrow(indo)){
    if(!is.na(indo[i,5]) & indo[i,5]==0){
      temp <- 0
    }
    else{
      temp <- temp + 1
      if (temp>indo8) {
        indo8 <- temp
      }
    }
  }
  view(indo8)
  
  vn8 <- 0
  temp <- 0
  for (i in 1:nrow(vn)){
    if(!is.na(vn[i,5]) & vn[i,5]==0){
      temp <- 0
    }
    else{
      temp <- temp + 1
      if (temp>vn8) {
        vn8 <- temp
      }
    }
  }
  view(vn8)
  
  jp8 <- 0
  temp <- 0
  for (i in 1:nrow(jp)){
    if(!is.na(jp[i,5]) & jp[i,5]==0){
      temp <- 0
    }
    else{
      temp <- temp + 1
      if (temp>jp8) {
        jp8 <- temp
      }
    }
  }
  view(jp8)
}

iv <- function()
{
  #pre iv1 iv2
  freg <- data[!duplicated(data[,c('location')]),]
  coun_num <- nrow(freg)
  x_con <- c("Africa","Asia","Europe",
             "North America","Oceania","South America")
  
  #iv1
  y_con <- cumsum(as.numeric(table(freg$continent)))
  df_iv1 <- data.frame(Continent = x_con,Freg = y_con)
  ggplot(data = df_iv1, aes(x = Continent, y = Freg, fill = Continent)) +
    geom_bar(stat = "Identity", colour = "black")
  
  #iv2
  y_con <- y_con/coun_num
  df_iv2 <- data.frame(Continent = x_con,Freg = y_con)
  ggplot(data = df_iv2, aes(x = Continent, y = Freg, fill = Continent)) +
    geom_bar(stat = "Identity", colour = "black")
  
  #pre iv3 iv4
  x_coun <- c("Indonesia","Japan","Vietnam")
  id_data <- subset(data, location == "Indonesia")
  jp_data <- subset(data, location == "Japan")
  vn_data <- subset(data, location == "Vietnam")
  
  id_date <- tail(id_data[order(as.Date(id_data$date,
                                        format="%d/%m/%Y")),],n=7)
  jp_date <- tail(jp_data[order(as.Date(jp_data$date,
                                        format="%d/%m/%Y")),],n=7)
  vn_date <- tail(vn_data[order(as.Date(vn_data$date,
                                        format="%d/%m/%Y")),],n=7)
  
  dfDate <- data.frame(rbind(id_date,jp_date,vn_date))
  
  #iv3
  ggplot(data = dfDate, aes(x = location, y = new_cases, fill = date)) + 
    geom_bar(stat = "Identity",colour = "black",position = "dodge")
  
  #iv4
  ggplot(data = dfDate, aes(x = location, y = new_deaths, fill = date)) + 
    geom_bar(stat = "Identity",colour = "black",position = "dodge")
  
  #pre iv5 6
  iv_5_6 <- function(data, name, col)
  {
    subdata <- subset(data,location==name)
    sum <- summary(subdata[,col])
    Q1 <- as.numeric(sum[2])
    Q3 <- as.numeric(sum[5])
    outlier <- 0
    for(i in 1:nrow(subdata))
    {
      if(is.na(subdata[i,col])) next
      if(subdata[i,col] < (Q1 - (1.5*(Q3 - Q1))) 
         || subdata[i,col] > (Q3 + (1.5*(Q3 - Q1))))
      {
        outlier <- outlier + 1
      }
    }
    return(c(name,outlier))
  }
  
  coun_name <- unique(data[,3])
  #iv5
  dfOutlier <- data.frame(Countries = c("tmp"), Outlier = c(0))
  for(i in 1:length(coun_name))
  {
    dfOutlier <- rbind(dfOutlier, iv_5_6(data,coun_name[i],5))
  }
  dfOutlier[,2] <- as.numeric(dfOutlier[,2])
  dfOutlier <- subset(dfOutlier, Outlier != 0)
  view_dfOutlier <- data.frame(rbind(subset(dfOutlier, 
                                            Countries == "Indonesia"),
                                     subset(dfOutlier, 
                                            Countries == "Japan"),
                                     subset(dfOutlier, 
                                            Countries == "Vietnam")))
  ggplot(data = dfOutlier, aes(x = Countries, y = Outlier))+ 
    geom_bar(stat = "Identity", colour = "black")
  ggplot(data = view_dfOutlier, 
         aes(x = Countries, y = Outlier, fill = Countries))+ 
    geom_bar(stat = "Identity", colour = "black")
  
  #iv6
  dfOutlier <- data.frame(Countries = c("tmp"), Outlier = c(0))
  for(i in 1:length(coun_name))
  {
    dfOutlier <- rbind(dfOutlier, iv_5_6(data,coun_name[i],6))
  }
  dfOutlier[,2] <- as.numeric(dfOutlier[,2])
  dfOutlier <- subset(dfOutlier, Outlier != 0)
  view_dfOutlier <- data.frame(rbind(subset(dfOutlier, 
                                            Countries == "Indonesia"),
                                     subset(dfOutlier, 
                                            Countries == "Japan"),
                                     subset(dfOutlier, 
                                            Countries == "Vietnam")))
  ggplot(data = dfOutlier, aes(x = Countries, y = Outlier))+ 
    geom_bar(stat = "Identity", colour = "black")
  ggplot(data = view_dfOutlier, aes(x = Countries, y = Outlier, 
                                    fill = Countries))+ 
    geom_bar(stat = "Identity", colour = "black")
  
}

v_vi_vii_viii <- function()
{
  #pre v vi vii viii
  #MADE
  data2 <- data[data[,3]%in%c("Vietnam","Japan","Indonesia"),]
  data2 <- rbind(data2,world_data)
  data2[,4] <- as.POSIXct(data2[,4], format = "%m/%d/%Y")
  y2020 <- data2[format(data2[,4],format="%Y")=="2020" & 
                   (format(data2[,4],format="%m")=="02" | 
                      format(data2[,4],format="%m")=="01" | 
                      format(data2[,4],format="%m")=="07" | 
                      format(data2[,4],format="%m")=="09"),]
  
  y2021 <- data2[format(data2[,4],format="%Y")=="2021" & 
                   (format(data2[,4],format="%m")=="02" | 
                      format(data2[,4],format="%m")=="01" | 
                      format(data2[,4],format="%m")=="07" | 
                      format(data2[,4],format="%m")=="09"),]
  
  y2022 <- data2[format(data2[,4],format="%Y")=="2022" & 
                   (format(data2[,4],format="%m")=="02" | 
                      format(data2[,4],format="%m")=="01" | 
                      format(data2[,4],format="%m")=="07" | 
                      format(data2[,4],format="%m")=="09"),]
  
  
  #last_month
  y2020_1 <- data2[format(data2[,4],format="%Y")=="2020" & 
                     (format(data2[,4],format="%m")=="11" | 
                        format(data2[,4],format="%m")=="12"),]
  
  y2021_1 <- data2[format(data2[,4],format="%Y")=="2021" & 
                     (format(data2[,4],format="%m")=="11" | 
                        format(data2[,4],format="%m")=="12"),]
  
  y2022_1 <- data2[format(data2[,4],format="%Y")=="2022" & 
                     (format(data2[,4],format="%m")=="11" | 
                        format(data2[,4],format="%m")=="12"),]
  
  #function
  draw_chart <- function(year_data, month_data, yyyy,
                         cases_or_deaths,avg_or_not="")
  {
    if(dim(year_data) == 0)
      return(ggplot()+labs(x="",y=paste(cases_or_deaths,"",yyyy))+
               theme(legend.position="top")+ggtitle("NA"))
    
    amedumb <- 5
    pain <- "Cases"
    if(cases_or_deaths == "deaths"){
      pain <- "Deaths"
      amedumb <- 6 
    }
    if(avg_or_not == "avg")
    {
      tmp <- ave_handle(year_data, month_data, amedumb)
      year_data[,amedumb] <- tmp
    }
    
    chart_out <- ggplot(data=year_data, 
                        aes(x=format(year_data[,4],format="%d"),y=year_data[,amedumb],
                            color=month_data,group=month_data))+
      geom_line(lwd=1)+
      labs(x="",y=paste(pain,"",yyyy))+
      theme(legend.position="top")
    return(chart_out)
  }
  
  cum_rel <- function(year_data, month_data, yyyy, 
                      cases_or_deaths,avg_or_not="")
  {
    if(dim(year_data) == 0)
      return(ggplot()+labs(x="",y=paste(cases_or_deaths,"",yyyy))+
               theme(legend.position="top")+ggtitle("NA"))
    amedumb <- 5
    pain <- "Cases"
    if(cases_or_deaths == "deaths"){
      pain <- "Deaths"
      amedumb <- 6 
    }
    if(avg_or_not == "avg")
    {
      tmp <- ave_handle(year_data,month_data,amedumb)
      year_data[,amedumb] <- tmp
    }
    
    cum_sum_data <- cbind(cumsum(year_data[,amedumb]))
    prob <- cum_sum_data/sum(year_data[,amedumb])
    year_data[,amedumb] <- prob
    chart_out <- ggplot(data=year_data, aes(x=year_data[,4],
                                            y=year_data[,amedumb],group=1))+
      geom_line(lwd=1)+
      labs(x="Dates",y=paste(pain,"_crf",yyyy))+
      scale_x_datetime(date_labels = "%m/%d/%Y",date_breaks = "1 week")+
      theme(legend.position="top")
    return(chart_out)
  }
  
  cum <- function(year_data, month_data,yyyy,cases_or_deaths,avg_or_not="")
  {
    if(dim(year_data) == 0) return(ggplot()+labs(x="",y=paste(cases_or_deaths,"",yyyy))+
                                     theme(legend.position="top")+ggtitle("NA"))
    amedumb <- 5
    pain <- "Cases"
    if(cases_or_deaths == "deaths"){
      pain <- "Deaths"
      amedumb <- 6 
    }
    if(avg_or_not == "avg")
    {
      tmp <- ave_handle(year_data, month_data, amedumb)
      year_data[,amedumb] <- tmp
    }
    
    cum_sum_data <- cbind(cumsum(year_data[,amedumb]))
    prob <- cum_sum_data
    year_data[,amedumb] <- prob
    chart_out <- ggplot(data=year_data, 
                        aes(x=format(year_data[,4],format="%d"),y=year_data[,amedumb],
                            color=month_data,group=month_data))+
      geom_line(lwd=1)+
      labs(x="",y=paste("Cumulavite of",pain,"",yyyy))+
      theme(legend.position="top")
    return(chart_out)
  }
  
  two_line_chart <- function(year_data, month_data, yyyy,
                             cases_or_deaths,avg_or_not="")
  {
    if(dim(year_data) == 0) 
      return(ggplot()+labs(x="",y=paste("Cases and Deaths","",yyyy))+
               theme(legend.position="top")+ggtitle("NA"))
    cases <- c()
    deaths <- c()
    mon_uni <- cbind(month_data)
    if(avg_or_not == "avg")
    {
      tmp <- ave_handle(year_data, month_data, 5)
      year_data[,5] <- tmp
      tmp <- ave_handle(year_data, month_data, 6)
      year_data[,6] <- tmp
    }
    
    for(x in 1:length(mon_uni))
    {
      cases <- rbind(cases,paste("New_cases",toString(mon_uni[x])))
      deaths <- rbind(deaths,paste("New_deaths",toString(mon_uni[x])))
    }
    cases_and_deaths <- rbind(cases,deaths)
    chart_out <- ggplot(data=year_data,
                        aes(x=format(year_data[,4],format="%d"),group=month_data))+
      geom_line(lwd=1, aes(y = year_data[,5], colour = cases))+
      geom_line(lwd=1, aes(y = year_data[,6], colour = deaths))+
      labs(x="",y=paste("Cases and Deaths","",yyyy))+
      theme(legend.position="top")
    return(chart_out)
  }
  
  ave_7days <- function(mon)
  {
    i <- 1
    j <- 1
    arr <- cbind(mon)
    arr[is.na(arr)] <- 0
    wah <- arr
    lenlen <- length(arr)
    while(i < lenlen + 1)
    {
      wah[i] <- arr[j:i]/(i - j + 1)
      if(i >= 7)
      {
        j <- j + 1
      }
      i <- i + 1
    }
    while(j < i)
    {
      wah[j] <- arr[j:i]/(i - j + 1)
      #print(i - j + 1)
      j <- j + 1
    }
    #View(cases)
    #cases <- cases[!(cases %in% NA)]
    #print(sum(cases))
    return(wah)
  }
  
  ave_handle <- function(df, mon, amedumb)
  {
    mon_uni <- cbind(unique(mon))
    df[is.na(df)] <- 0
    arr <- c()
    for(x in mon_uni)
    {
      tmp <- df[format(df[,4],format="%m")==x,]
      arr <- rbind(arr,ave_7days(tmp[,amedumb]))
    }
    return(arr)
  }
  
  country_chart <- function(country, type_w, made, cases_or_deaths = "", 
                            chart_name, avg_or_not = "")
  {
    Months_2020<-format(y2020[y2020[,3]==country,4],format="%m")
    Months_2021<-format(y2021[y2021[,3]==country,4],format="%m")
    Months_2022<-format(y2022[y2022[,3]==country,4],format="%m")
    
    Last_months_2020<-format(y2020_1[y2020_1[,3]==country,4],format="%m")
    Last_months_2021<-format(y2021_1[y2021_1[,3]==country,4],format="%m")
    Last_months_2022<-format(y2022_1[y2022_1[,3]==country,4],format="%m")
    if(type_w == "line_chart")
    {
      if(made == "2_1_7_9")
      {
        chart_2020 <- draw_chart(y2020[y2020[,3] == country,], 
                                 Months_2020, "2020", cases_or_deaths,avg_or_not)
        chart_2021 <- draw_chart(y2021[y2021[,3] == country,], 
                                 Months_2021, "2021", cases_or_deaths,avg_or_not)
        chart_2022 <- draw_chart(y2022[y2022[,3] == country,], 
                                 Months_2022, "2022", cases_or_deaths,avg_or_not)
        ggsave(filename = paste(chart_name,country,".jpeg"), 
               plot = arrangeGrob(chart_2020, chart_2021, chart_2022), 
               device = "jpeg", scale = 1, width = 9, height = 9)
      }
      else
      {
        chart_2020 <- draw_chart(y2020_1[y2020_1[,3] == country,],
                                 Last_months_2020, "2020", cases_or_deaths,avg_or_not)
        chart_2021 <- draw_chart(y2021_1[y2021_1[,3] == country,],
                                 Last_months_2021, "2021", cases_or_deaths,avg_or_not)
        chart_2022 <- draw_chart(y2022_1[y2022_1[,3] == country,],
                                 Last_months_2022, "2022", cases_or_deaths,avg_or_not)
        ggsave(filename = paste(chart_name,country,".jpeg"), 
               plot = arrangeGrob(chart_2020, chart_2021, chart_2022), 
               device = "jpeg", scale = 1, width = 9, height = 9)
      }
    }
    else if(type_w == "two_line")
    {
      if(made == "2_1_7_9")
      {
        chart_2020 <- two_line_chart(y2020[y2020[,3] == country,], 
                                     Months_2020, "2020", cases_or_deaths,avg_or_not)
        chart_2021 <- two_line_chart(y2021[y2021[,3] == country,], 
                                     Months_2021, "2021", cases_or_deaths,avg_or_not)
        chart_2022 <- two_line_chart(y2022[y2022[,3] == country,], 
                                     Months_2022, "2022", cases_or_deaths,avg_or_not)
        ggsave(filename = paste(chart_name,country,".jpeg"), 
               plot = arrangeGrob(chart_2020, chart_2021, chart_2022), 
               device = "jpeg", scale = 1, width = 9, height = 9)
      }
      else
      {
        chart_2020 <- two_line_chart(y2020_1[y2020_1[,3] == country,],
                                     Last_months_2020, "2020", cases_or_deaths,avg_or_not)
        chart_2021 <- two_line_chart(y2021_1[y2021_1[,3] == country,],
                                     Last_months_2021, "2021", cases_or_deaths,avg_or_not)
        chart_2022 <- two_line_chart(y2022_1[y2022_1[,3] == country,],
                                     Last_months_2022, "2022", cases_or_deaths,avg_or_not)
        ggsave(filename = paste(chart_name,country,".jpeg"), 
               plot = arrangeGrob(chart_2020, chart_2021, chart_2022), 
               device = "jpeg", scale = 1, width = 9, height = 9)
      }
    }
    else if(type_w == "cum") 
    {
      if(made == "2_1_7_9")
      {
        chart_2020 <- cum(y2020[y2020[,3] == country,],
                          Months_2020, "2020", cases_or_deaths,avg_or_not)
        chart_2021 <- cum(y2021[y2021[,3] == country,], 
                          Months_2021, "2021", cases_or_deaths,avg_or_not)
        chart_2022 <- cum(y2022[y2022[,3] == country,], 
                          Months_2022, "2022", cases_or_deaths,avg_or_not)
        ggsave(filename = paste(chart_name,country,".jpeg"), 
               plot = arrangeGrob(chart_2020, chart_2021, chart_2022), 
               device = "jpeg", scale = 1, width = 9, height = 9)
      }
      else if(made == "11_12")
      {
        chart_2020 <- cum(y2020_1[y2020_1[,3] == country,], 
                          Last_months_2020, "2020", cases_or_deaths,avg_or_not)
        chart_2021 <- cum(y2021_1[y2021_1[,3] == country,], 
                          Last_months_2021, "2021", cases_or_deaths,avg_or_not)
        chart_2022 <- cum(y2022_1[y2022_1[,3] == country,], 
                          Last_months_2022, "2022", cases_or_deaths,avg_or_not)
        ggsave(filename = paste(chart_name,country,".jpeg"), 
               plot = arrangeGrob(chart_2020, chart_2021, chart_2022), 
               device = "jpeg", scale = 1, width = 9, height = 9)
      }
    }
    else 
    {
      if(made == "2_1_7_9")
      {
        chart_2020 <- cum_rel(y2020[y2020[,3] == country,], 
                              Months_2020, "2020", cases_or_deaths,avg_or_not)
        chart_2021 <- cum_rel(y2021[y2021[,3] == country,],
                              Months_2021, "2021", cases_or_deaths,avg_or_not)
        chart_2022 <- cum_rel(y2022[y2022[,3] == country,], 
                              Months_2022, "2022", cases_or_deaths,avg_or_not)
        ggsave(filename = paste(chart_name,country,".jpeg"), 
               plot = arrangeGrob(chart_2020, chart_2021, chart_2022), 
               device = "jpeg", scale = 1, width = 9, height = 9)
      }
      else if(made == "11_12")
      {
        chart_2020 <- cum_rel(y2020_1[y2020_1[,3] == country,], 
                              Last_months_2020, "2020", cases_or_deaths,avg_or_not)
        chart_2021 <- cum_rel(y2021_1[y2021_1[,3] == country,], 
                              Last_months_2021, "2021", cases_or_deaths,avg_or_not)
        chart_2022 <- cum_rel(y2022_1[y2022_1[,3] == country,], 
                              Last_months_2022, "2022", cases_or_deaths,avg_or_not)
        ggsave(filename = paste(chart_name,country,".jpeg"), 
               plot = arrangeGrob(chart_2020, chart_2021, chart_2022), 
               device = "jpeg", scale = 1, width = 9, height = 9)
      }
    }
  }
  #v
  #v1
  country_chart("Vietnam","line_chart","2_1_7_9","cases","v1")
  country_chart("Japan","line_chart","2_1_7_9","cases","v1")
  country_chart("Indonesia","line_chart","2_1_7_9","cases","v1")
  #v2
  country_chart("Vietnam","line_chart","2_1_7_9","deaths","v2")
  country_chart("Japan","line_chart","2_1_7_9","deaths","v2")
  country_chart("Indonesia","line_chart","2_1_7_9","deaths","v2")
  #v3
  country_chart("Vietnam","two_line","2_1_7_9","","v3")
  country_chart("Japan","two_line","2_1_7_9","","v3")
  country_chart("Indonesia","two_line","2_1_7_9","","v3")
  #v4
  country_chart("Vietnam","line_chart","11_12","cases","v4")
  country_chart("Japan","line_chart","11_12","cases","v4")
  country_chart("Indonesia","line_chart","11_12","cases","v4")
  #v5
  country_chart("Vietnam","line_chart","11_12","deaths","v5")
  country_chart("Japan","line_chart","11_12","deaths","v5")
  country_chart("Indonesia","line_chart","11_12","deaths","v5")
  # #v6
  country_chart("Vietnam","two_line","11_12","","v6")
  country_chart("Japan","two_line","11_12","","v6")
  country_chart("Indonesia","two_line","11_12","","v6")
  # #v7
  country_chart("Vietnam","cum","2_1_7_9","cases","v7")
  country_chart("Japan","cum","2_1_7_9","cases","v7")
  country_chart("Indonesia","cum","2_1_7_9","cases","v7")
  # #v8
  country_chart("Vietnam","cum","2_1_7_9","deaths","v8")
  country_chart("Japan","cum","2_1_7_9","deaths","v8")
  country_chart("Indonesia","cum","2_1_7_9","deaths","v8")
  
  #vi
  #v1
  country_chart("Vietnam","line_chart","2_1_7_9","cases","vi1","avg")
  country_chart("Japan","line_chart","2_1_7_9","cases","vi1","avg")
  country_chart("Indonesia","line_chart","2_1_7_9","cases","vi1","avg")
  #v2
  country_chart("Vietnam","line_chart","2_1_7_9","deaths","vi2","avg")
  country_chart("Japan","line_chart","2_1_7_9","deaths","vi2","avg")
  country_chart("Indonesia","line_chart","2_1_7_9","deaths","vi2","avg")
  #v3
  country_chart("Vietnam","two_line","2_1_7_9","","vi3","avg")
  country_chart("Japan","two_line","2_1_7_9","","vi3","avg")
  country_chart("Indonesia","two_line","2_1_7_9","","vi3","avg")
  #v4
  country_chart("Vietnam","line_chart","11_12","cases","vi4","avg")
  country_chart("Japan","line_chart","11_12","cases","vi4","avg")
  country_chart("Indonesia","line_chart","11_12","cases","vi4","avg")
  #v5
  country_chart("Vietnam","line_chart","11_12","deaths","vi5","avg")
  country_chart("Japan","line_chart","11_12","deaths","vi5","avg")
  country_chart("Indonesia","line_chart","11_12","deaths","vi5","avg")
  #v6
  country_chart("Vietnam","two_line","11_12","","vi6","avg")
  country_chart("Japan","two_line","11_12","","vi6","avg")
  country_chart("Indonesia","two_line","11_12","","vi6","avg")
  #v7
  country_chart("Vietnam","cum","2_1_7_9","cases","vi7","avg")
  country_chart("Japan","cum","2_1_7_9","cases","vi7","avg")
  country_chart("Indonesia","cum","2_1_7_9","cases","vi7","avg")
  #v8
  country_chart("Vietnam","cum","2_1_7_9","deaths","vi8","avg")
  country_chart("Japan","cum","2_1_7_9","deaths","vi8","avg")
  country_chart("Indonesia","cum","2_1_7_9","deaths","vi8","avg")
  
  #vii
  #vii1
  country_chart("World","line_chart","2_1_7_9","cases","vii1")
  #vii2
  country_chart("World","line_chart","2_1_7_9","deaths","vii2")
  #vii3
  country_chart("World","line_chart","11_12","cases","vii3")
  #vii4
  country_chart("World","line_chart","11_12","deaths","vii4")
  #vii5
  country_chart("World","cum_rel","11_12","cases","vii5")
  #vii6
  country_chart("World","cum_rel","11_12","deaths","vii6")
  
  #viii
  #viii1
  country_chart("World","line_chart","2_1_7_9","cases","viii1","avg")
  #viii2
  country_chart("World","line_chart","2_1_7_9","deaths","viii2","avg")
  #viii3
  country_chart("World","line_chart","11_12","cases","viii3","avg")
  #viii4
  country_chart("World","line_chart","11_12","deaths","viii4","avg")
  #viii5
  country_chart("World","cum","11_12","cases","viii5","avg")
  #viii6
  country_chart("World","cum","11_12","deaths","viii6","avg")
}

ix <- function()
{
  #pre ix1
  need_con <- c("Vietnam", "Japan", "Indonesia")
  #need_month <- c("02", "01", "07", "09")
  three_country <- data %>% filter(location %in% need_con)
  three_country$date <- as.POSIXct(three_country$date, format = "%m/ %d/ %Y")
  
  row.names(three_country) <- 1:nrow(three_country)
  
  Vie <- three_country %>% filter(three_country$location == "Vietnam")
  Jap <- three_country %>% filter(three_country$location == "Japan")
  In <- three_country  %>% filter(three_country$location == "Indonesia")
  options("scipen"=10)
  #View(Vie)
  #function
  draw_chart <- function(country_data, country_name)
  {
    new_cases_data <- country_data$new_cases
    new_deaths_data <- country_data$new_deaths
    sum_cases <- sum(new_cases_data, na.rm = TRUE)
    sum_deaths <- sum(new_deaths_data, na.rm = TRUE)
    cases_cumu <- cbind(new_cases_data)
    deaths_cumu <- cbind(new_deaths_data)
    cases_cumu[is.na(cases_cumu)] <- 0
    deaths_cumu[is.na(deaths_cumu)] <- 0
    for(x in 1:(length(new_cases_data) - 1))
    {
      cases_cumu[x + 1] <- cases_cumu[x] + cases_cumu[x + 1]
      deaths_cumu[x + 1] <- deaths_cumu[x] + deaths_cumu[x + 1]
    }
    for(x in 1:length(new_cases_data))
    {
      cases_cumu[x] <- (cases_cumu[x] / sum_cases)*100
      deaths_cumu[x] <- (deaths_cumu[x] / sum_deaths)*100
    }
    name = country_data
    name = name$date
    chart_data <- data.frame(name, cases_cumu, deaths_cumu)
    #View(chart_data)
    title = paste(country_name, " New_cases and New_deaths line chart")
    line_chart <- ggplot(data = chart_data, aes(x = name))+
      geom_line(aes(y = cases_cumu, colour = "New_cases"), size = 1.2)+
      geom_line(aes(y = deaths_cumu, colour = "New_deaths"), size = 1.2)+
      ylab("New_cases and New_deaths percent")+
      xlab("Month")+
      ylab("Percent")+
      theme(text = element_text(size = 16))+
      ggtitle(title)
    return(line_chart)
  }
  
  #ix1
  ix1 <- function()
  {
    #View(Vie)
    Vie_chart <- draw_chart(Vie[,3:6], "Vietnam")
    ggsave(filename = "ix1Vietnam.jpeg", plot = Vie_chart, device = "jpeg", scale = 1, width = 8, height = 8)
    Jap_chart <- draw_chart(Jap[,3:6], "Japan")
    ggsave(filename = "ix1Japan.jpeg", plot = Jap_chart, device = "jpeg", scale = 1, width = 8, height = 8)
    In_chart <- draw_chart(In[,3:6], "Indonesia")
    ggsave(filename = "ix1Indonesia.jpeg", plot = In_chart, device = "jpeg", scale = 1, width = 8, height = 8)
  }
  ix1()
  #ix2_ix3
  
    #ix - 2,3
    
    #Prep for 2 and 3
    ix_cor_MY <- function(subdata, stt_month)
    {
      data_my <- subdata %>% mutate(date = mdy(date),Month_Yr = format_ISO8601(date, precision = "ym"))
      data_my <- subset(data_my, !is.na(data_my[,5]))
      data_my <- subset(data_my, !is.na(data_my[,6]))
      subMonth <- subset(data_my, Month_Yr == stt_month)
      if(nrow(subMonth) == 0)
      {
        plot(NULL, xlim=c(0,1), ylim=c(0,1), main = stt_month, sub = paste("No Correlation"), ylab="Deaths", xlab="Cases")
        abline(h = 0.5, col = "red", lwd = 3)
      }
      else
      {
        if(nrow(subMonth) == 1)
        {
          plot(subMonth[,5], subMonth[,6], pch = 10, col = "black", main = stt_month, sub = paste("No Correlation"), ylab="Deaths", xlab="Cases")
          abline(h = subMonth[1,6], col = "red", lwd = 3)
        }
        else 
        {
          num <- cor(subMonth[,5], subMonth[,6], method = "pearson")
          if(is.na(num)) plot(subMonth[,5], subMonth[,6], pch = 10, col = "black", main = stt_month, sub = paste("No Correlation"), ylab="Deaths", xlab="Cases")
          else plot(subMonth[,5], subMonth[,6], pch = 10, col = "black", main = stt_month, sub = paste("Correlation: ", round(num,2)), ylab="Deaths", xlab="Cases")
          abline(lm(subMonth[,6] ~ subMonth[,5]), col = "red", lwd = 3)
        }
      }
    }
    
    id_data <- subset(data, location == "Indonesia")
    jp_data <- subset(data, location == "Japan")
    vn_data <- subset(data, location == "Vietnam")
    my_months <- c("2020-01", "2020-02", "2020-07", "2020-09", "2021-01", "2021-02", "2021-07", "2021-09", "2022-01", "2022-02")
    
    #ix - 2
    
    #Indonesia
    par(mfrow=c(3,4))
    for(i in 1:length(my_months))
    {
      ix_cor_MY(id_data,my_months[i])
    }
    
    #Japan
    par(mfrow=c(3,4))
    for(i in 1:length(my_months))
    {
      ix_cor_MY(jp_data,my_months[i])
    }
    
    #Vietnam
    par(mfrow=c(3,4))
    for(i in 1:length(my_months))
    {
      ix_cor_MY(vn_data,my_months[i])
    }
    
    #ix - 3
    #Avg 7 days function
    
    avg7 <- function(subdata,col)
    {
      new_avg7 <- subdata[,col]
      j <- 1
      for(i in 1:length(new_avg7))
      {
        new_avg7[i] <- (sum(new_avg7[j:i],na.rm = TRUE)/(i-j+1))
        if(i >= 7) j <- j + 1
      }
      return(new_avg7)
    }
    
    id_data[,5] <- avg7(id_data,5)
    id_data[,6] <- avg7(id_data,6)
    jp_data[,5] <- avg7(jp_data,5)
    jp_data[,6] <- avg7(jp_data,6)
    vn_data[,5] <- avg7(vn_data,5)
    vn_data[,6] <- avg7(vn_data,6)
    
    #Indonesia
    par(mfrow=c(3,4))
    for(i in 1:length(my_months))
    {
      ix_cor_MY(id_data,my_months[i])
    }
    
    #Japan
    par(mfrow=c(3,4))
    for(i in 1:length(my_months))
    {
      ix_cor_MY(jp_data,my_months[i])
    }
    
    #Vietnam
    par(mfrow=c(3,4))
    for(i in 1:length(my_months))
    {
      ix_cor_MY(vn_data,my_months[i])
    }
  
  ix2_ix3()
}
i()
ii()
iii()
iv()
v_vi_vii_viii()
ix()

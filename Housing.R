library(dplyr)


h_train=read.csv(".\\Data\\housing_train.csv",stringsAsFactors = F)
h_test=read.csv(".\\Data\\housing_test.csv",stringsAsFactors = F)

h_test$Price=NA

h_test$data='test'
h_train$data='train'
h_all=rbind(h_train,h_test)



CreateDummies=function(data,var,freq_cutoff=0){
  t=table(data[,var])
  t=t[t>freq_cutoff]
  t=sort(t)
  categories=names(t)[-1]
  
  for( cat in categories){
    name=paste(var,cat,sep="_")
    name=gsub(" ","",name)
    name=gsub("-","_",name)
    name=gsub("\\?","Q",name)
    name=gsub("<","LT_",name)
    name=gsub("\\+","",name)
    
    data[,name]=as.numeric(data[,var]==cat)
  }
  
  data[,var]=NULL
  return(data)
}

h_all=h_all %>%
  select(-Address)


h_all %>% 
  group_by(Method) %>% 
  summarise('means'= mean(Price, na.rm = T)) %>% 
  arrange(desc(means))

mean(h_all$Price,na.rm=T)


h_all=h_all %>% 
  mutate(M_8=as.numeric(Method=='SP'),
         M_10=as.numeric(Method %in% c("PI")),
         M_11=as.numeric(Method %in% c("S","VB")),
         M_12=as.numeric(Method %in% c("SA"))) %>% 
  select(-Method)


round(tapply(h_all$Price,h_all$YearBuilt,mean,na.rm=T))

h_all=h_all %>% 
  mutate(
    Y_1800=as.numeric(YearBuilt %in% c("1800","1899")),
    Y_1900=as.numeric(YearBuilt %in% c("1900","1999")),
    Y_2000=as.numeric(YearBuilt %in% c("2000","2099"))) %>% 
  select(-YearBuilt)

names(h_all)[sapply(h_all,function(x) is.character(x))]

cat_cols = c(
  "Suburb"      ,    "Type" ,       "SellerG" ,   
  "CouncilArea" )

lapply(h_all,function(x) length(unique(x)))



for(cat in cat_cols){
  h_all=CreateDummies(h_all,cat,50)
}

glimpse(h_all)

sum(sapply(h_all,function(x) is.character(x)))


for(col in names(h_all)){
  
  if(sum(is.na(h_all[,col]))>0 & !(col %in% c("data","Price"))){
    
    h_all[is.na(h_all[,col]),col]=mean(h_all[,col],na.rm=T)
  }
  
}

######## 
h_train=h_all %>% filter(data=='train') %>% select(-data)
h_test=h_all %>% filter(data=='test') %>% select (-data,-Price)

##########
set.seed(2)
s=sample(1:nrow(h_train),0.8*nrow(h_train))
h_train1=h_train[s,]
h_train2=h_train[-s,]

dim(h_train)
dim(h_train1)
dim(h_train2)


############
library(car)

form <-
  as.formula(paste0('Price ~ ', paste0(setdiff(
    names(h_train1), c('Price','M_12','Y_1800')
  ), collapse = ' + ')))

for_vif=lm(form,data=h_train1)

sort(vif(for_vif),decreasing = T)[1:3]

vars=attributes(alias(for_vif)$Complete)$dimnames[[1]]
vars

for_vif=lm(form,data=h_train1)

sort(vif(for_vif),decreasing = T)[1:3]

fir=step(for_vif)
formula(fir)

fir=lm(Price ~ Rooms + Distance + Postcode + Bedroom2 + Bathroom + Car + 
         Landsize + BuildingArea + M_8 + M_10 + Y_1900 + Suburb_Hadfield + 
         Suburb_Abbotsford + Suburb_HeidelbergWest + Suburb_NorthMelbourne + 
         Suburb_OakleighSouth + Suburb_CoburgNorth + Suburb_HeidelbergHeights + 
         Suburb_Malvern + Suburb_SouthMelbourne + Suburb_BrunswickEast + 
         Suburb_SunshineNorth + Suburb_AvondaleHeights + Suburb_Fawkner + 
         Suburb_AltonaNorth + Suburb_Armadale + Suburb_Burwood + Suburb_Williamstown + 
         Suburb_Melbourne + Suburb_SunshineWest + Suburb_BrunswickWest + 
         Suburb_KeilorEast + Suburb_Prahran + Suburb_SurreyHills + 
         Suburb_Kensington + Suburb_Toorak + Suburb_Maribyrnong + 
         Suburb_Newport + Suburb_Doncaster + Suburb_AscotVale + Suburb_Thornbury + 
         Suburb_Hampton + Suburb_Balwyn + Suburb_MalvernEast + Suburb_Camberwell + 
         Suburb_PortMelbourne + Suburb_PascoeVale + Suburb_BrightonEast + 
         Suburb_Hawthorn + Suburb_BalwynNorth + Suburb_Coburg + Suburb_Kew + 
         Suburb_Brighton + Suburb_Glenroy + Suburb_GlenIris + Suburb_Essendon + 
         Suburb_Brunswick + Suburb_SouthYarra + Suburb_StKilda + Suburb_Preston + 
         Suburb_Richmond + Suburb_Reservoir + Type_u + Type_h + SellerG_Raine + 
         SellerG_Kay + SellerG_Gary + SellerG_Miles + SellerG_Greg + 
         SellerG_RT + SellerG_Biggin + SellerG_Ray + SellerG_Marshall + 
         SellerG_hockingstuart + SellerG_Jellis + CouncilArea_Whitehorse + 
         CouncilArea_Brimbank + CouncilArea_HobsonsBay + CouncilArea_Melbourne + 
         CouncilArea_Darebin + CouncilArea_MooneeValley + CouncilArea_Boroondara + 
         CouncilArea_
       ,data=h_train1)

summary(fir)
#######
library(pROC)
val.pred=predict(fir,newdata=h_train2,type='response')
auc(roc(h_train2$y,val.pred))

######
errors=h_train2$Price-val.pred

rmse=errors**2 %>% mean() %>% sqrt()
rmse

Score =round(212467/rmse,2)
Score


val.score=predict(fir,newdata = h_train2,type='response')
train.score <- predict(fir,newdata=h_train1, type='response')
auc(roc(h_train2$Price,val.score))
auc(roc(h_train1$Price,train.score))


log.fit.final=glm(Price ~ Rooms + Distance + Postcode + Bedroom2 + Bathroom + Car + 
                    Landsize + BuildingArea + M_8 + M_10 + Y_1900 + Suburb_Hadfield + 
                    Suburb_Abbotsford + Suburb_HeidelbergWest + Suburb_NorthMelbourne + 
                    Suburb_OakleighSouth + Suburb_CoburgNorth + Suburb_HeidelbergHeights + 
                    Suburb_Malvern + Suburb_SouthMelbourne + Suburb_BrunswickEast + 
                    Suburb_SunshineNorth + Suburb_AvondaleHeights + Suburb_Fawkner + 
                    Suburb_AltonaNorth + Suburb_Armadale + Suburb_Burwood + Suburb_Williamstown + 
                    Suburb_Melbourne + Suburb_SunshineWest + Suburb_BrunswickWest + 
                    Suburb_KeilorEast + Suburb_Prahran + Suburb_SurreyHills + 
                    Suburb_Kensington + Suburb_Toorak + Suburb_Maribyrnong + 
                    Suburb_Newport + Suburb_Doncaster + Suburb_AscotVale + Suburb_Thornbury + 
                    Suburb_Hampton + Suburb_Balwyn + Suburb_MalvernEast + Suburb_Camberwell + 
                    Suburb_PortMelbourne + Suburb_PascoeVale + Suburb_BrightonEast + 
                    Suburb_Hawthorn + Suburb_BalwynNorth + Suburb_Coburg + Suburb_Kew + 
                    Suburb_Brighton + Suburb_Glenroy + Suburb_GlenIris + Suburb_Essendon + 
                    Suburb_Brunswick + Suburb_SouthYarra + Suburb_StKilda + Suburb_Preston + 
                    Suburb_Richmond + Suburb_Reservoir + Type_u + Type_h + SellerG_Raine + 
                    SellerG_Kay + SellerG_Gary + SellerG_Miles + SellerG_Greg + 
                    SellerG_RT + SellerG_Biggin + SellerG_Ray + SellerG_Marshall + 
                    SellerG_hockingstuart + SellerG_Jellis + CouncilArea_Whitehorse + 
                    CouncilArea_Brimbank + CouncilArea_HobsonsBay + CouncilArea_Melbourne + 
                    CouncilArea_Darebin + CouncilArea_MooneeValley + CouncilArea_Boroondara + 
                    CouncilArea_ ,data=h_train)
summary(log.fit.final)

test.prob.score= predict(log.fit.final,newdata = h_test,type='response')
write.table(test.prob.score,"Project.csv",row.names = F,col.names = "Price")


  

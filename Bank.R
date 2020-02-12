library(dplyr)

bk_train=read.csv(".Data\\bank-full_train.csv",stringsAsFactors = F)
bk_test=read.csv(".Data\\bank-full_test.csv",stringsAsFactors = F)


glimpse(bk_test)
glimpse(bk_train)
setdiff(names(bk_train),names(bk_test))

table(bk_train$y)
prop.table(bk_train$y)



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

bk_test$y=NA

bk_train$data='train'
bk_test$data='test'

bk_all=rbind(bk_train,bk_test)

glimpse(bk_all)

bk_all=bk_all %>% 
  mutate(df_yes=as.numeric( default=="yes")) %>% 
  select(- default)

bk_all=bk_all %>% 
  mutate(hsng_yes=as.numeric(  housing =="yes")) %>% 
  select(-  housing )

bk_all=bk_all %>% 
  mutate(l_yes=as.numeric(  loan =="yes")) %>% 
  select(- loan )

glimpse(bk_all)

unique(bk_all$education)

bk_all$y=(bk_all$y=="yes")+0
glimpse(bk_all)

head(bk_all)


names(bk_all)[sapply(bk_all,function(x) is.character(x))]

cat_cols = c(
  cat_cols = c(
    "job" ,      
    "marital",
    "education" ,
    "contact" ,
    "month"   ,
    "poutcome" 
  
  )
  
)

for(cat in cat_cols){
  bk_all=CreateDummies(bk_all,cat,50)
}
glimpse(bk_all)

sum(sapply(h_all,function(x) is.character(x)))



for(col in names(bk_all)){
  
  if(sum(is.na(bk_all[,col]))>0 & !(col %in% c("data","y"))){
    
    bk_all[is.na(bk_all[,col]),col]=mean(bk_all[,col],na.rm=T)
  }
  
}

##########
bk_train=bk_all %>% filter(data=='train') %>% select(-data)
bk_test=bk_all %>% filter(data=='test') %>% select (-data,-y)

set.seed(2)
s=sample(1:nrow(bk_train),0.7*nrow(bk_train))
bk_train1=bk_train[s,]
bk_train2=bk_train[-s,]

form <-
  as.formula(paste0('y ~ ', paste0(setdiff(
    names(bk_train1), c('y')
  ), collapse = ' + ')))

for_vif=lm(form,data=bk_train1)
sort(vif(for_vif),decreasing = T)[1:3]

fit=step(for_vif)
formula(fit)

fit=lm(y ~ age + balance + day + duration + campaign + pdays + ID + 
         hsng_yes + l_yes + job_student + job_unemployed + job_entrepreneur + 
         job_self_employed + job_retired + job_services + job_admin. + 
         job_technician + job_management + job_blue_collar + marital_married + 
         contact_unknown + contact_cellular + month_mar + month_sep + 
         month_oct + month_jan + month_nov + month_jun + month_jul + 
         month_may + poutcome_other + poutcome_failure + poutcome_unknown
       ,data=bk_train1)


val.score=predict(fit,newdata = bk_train2,type='response')
train.score <- predict(fit,newdata=bk_train1, type='response')

auc(roc(bk_train2$y,val.score))
auc(roc(bk_train1$y,train.score))


log_fit_final=glm(y ~ age + balance + day + duration + campaign + pdays + ID + 
                    hsng_yes + l_yes + job_student + job_unemployed + job_entrepreneur + 
                    job_self_employed + job_retired + job_services + job_admin. + 
                    job_technician + job_management + job_blue_collar + marital_married + 
                    contact_unknown + contact_cellular + month_mar + month_sep + 
                    month_oct + month_jan + month_nov + month_jun + month_jul + 
                    month_may + poutcome_other + poutcome_failure + poutcome_unknown,data=bk_train,family = "binomial")

variable= predict(log_fit_final,newdata = bk_test,type='response')
write.table(test.prob.score,"Project.csv",row.names = F,col.names = "y")


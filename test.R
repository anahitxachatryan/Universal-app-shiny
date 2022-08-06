library(ggplot2)
library(dplyr)
data=read.csv("oil.csv")
data=read.csv("output_df.csv")
# ggplot(data=data, aes(y=CLOSE, x=""))+ geom_boxplot()


to_date = function(colname, data){
  data[,colname]<-as.Date(data[,colname], format="%d-%b-%y") 
  return (data)
  
}
data=to_date("Date", data)
head(data)

as.Date("2011-01-01", format = "%Y-%d-%m")
as.Date("01-01-2011", format = "%m-%d-%Y")
as.Date("1-Oct-12", format = "%d-%b-%y")

as.Date("01-01-2011", format = "%d-%m-%Y")
as.Date("Oct-01-2011", format = "%b-%d-%Y")
as.Date("01-01-11", format = "%b-%d-%Y")

data=read.csv("Car_company_customers.csv")

factorize = function(data){
  data_cat = select(data, is.character)
  col_names = names(data_cat)
  data[col_names] = lapply(data[col_names] , as.factor)
  return (data)
}


data = factorize(data)
str(data)


data=read.csv("oil.csv")

get_mode = function(col){
  return(names(sort(table(col), decreasing = T, na.last = T)[1]))
}


fill_missing_vals = function(data, stat_measure){
  
  for (i in 1:ncol(data)){
    if(is.numeric(data[,i])){
      if (stat_measure == 'mean'){
        data[,i][is.na(data[,i])] = round(mean(data[,i], na.rm = TRUE))
      }
      if (stat_measure == 'std'){
        data[,i][is.na(data[,i])] = round(sd(data[,i],  na.rm = TRUE))
      }
      if (stat_measure == 'median'){
        data[,i][is.na(data[,i])] =round( median(data[,i],  na.rm = TRUE))
      }
    }
    else{
      data[,i][is.na(data[,i])] = get_mode(data[,i])
    }
  }
  
  return(data)
}

data = fill_missing_vals(data, "std")
head(data)

factorize = function(data) {
  
  for (i in 1:length(data)){
    num_uniques = 0
    num_uniques = length(unique(data[[i]]))
    print(names(data[i]))
    print(num_uniques)
    if(num_uniques <= 25){
      if(is.numeric(data[,i])){
        data[,i] = as.character(data[,i])
        
      }
      
    }
  }
  data_cat = select(data, is.character)
  col_names = names(data_cat)
  data[col_names] = lapply(data[col_names] , as.factor)
  return (data)
}

data=read.csv("output_df.csv")
head(data)
data = factorize(data)
str(data)
head(data)
select_if(data, is.factor)


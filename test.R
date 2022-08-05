library(ggplot2)
library(dplyr)
data=read.csv("oil.csv")
# ggplot(data=data, aes(y=CLOSE, x=""))+ geom_boxplot()


to_date = function(colname, data){
  data[,colname]<-as.Date(data[,colname], format="%d-%b-%y") 
  return (data)
  
}
data=to_date("DATE", data)
head(data)

as.Date("7-Oct-12", format = "%d-%b-%y")
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




library(plotly)
fig <- plot_ly(y = ~rnorm(50), type = "box")
fig <- fig %>% add_trace(y = ~rnorm(50, 1))

fig

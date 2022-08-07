library(ggplot2)

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

#############################
library(dplyr)

to_date = function(colname, data) {
  possible_formats = c("%Y-%d-%m", "%d-%b-%y", "%m-%d-%Y", "%b-%d-%Y", "%d-%m-%Y")
  for (i in 1:length(possible_formats)) {
    old_col = data[, colname]
    data[, colname] = as.Date(data[, colname], format = possible_formats[i])
    
    if (sum(is.na(data[, colname])) == nrow(data)) {
      data[, colname] = old_col
    }
  }
  data[, colname] <- as.Date(data[, colname], format = format_given)
  return (data)
  
}

factorize = function(data) {
  
  for (i in 1:length(data)){
    num_uniques = 0
    num_uniques = length(unique(data[[i]]))
    print(names(data[i]))
    print(num_uniques)
    if(num_uniques <= 100){
      if(is.numeric(data[,i])){
        data[,i] = as.character(data[,i])
        
        
      }
      data[,i] = as.factor(data[,i])
      
    }
  }

  return (data)
}


outlier_detection = function(data) {
  for (i in 1:ncol(data)) {
    if (is.numeric(data[, i])) {
      quartiles <- quantile(data[, i], probs = c(.25, .75), na.rm = FALSE)
      IQR <- IQR(data[, i])
      
      Lower <- quartiles[1] - 1.5 * IQR
      Upper <- quartiles[2] + 1.5 * IQR
      
      data <- subset(data, data[, i] > Lower & data[, i] < Upper)
      
    }
  }
  return (data)
}

data=read.csv("output_df.csv")
ggplot(data = data, aes_string(y = "Count")) + geom_boxplot(fill = "#ADD8E6")


head(data)
data = factorize(data)
str(data)
head(data)
data = to_date("Date",data)
select_if(data, is.factor)

data=read.csv("output_df.csv")
data[1,4] = NA
write.csv(data,"output_df_changed.csv", row.names = FALSE)

data_s = read.csv("saved.csv")
str(data_s)
library(dplyr)
grouped_df = data_s %>%
  group_by(Gender, Ever_Married) %>%
  summarise(Age_m = mean(Age), na.rm = TRUE)
grouped_df

data_s = outlier_detection(data_s)
str(data_s)




corr_mat <- round(cor(data[, unlist(lapply(data, is.numeric))], use = "complete.obs"),2)
corr_mat
melted_corr_mat <- melt(corr_mat)

ggplot(data = melted_corr_mat, aes(x=Var1, y=Var2, fill = value)) +
  geom_tile() +
  geom_text(aes(Var2, Var1, label = value),
            color = "white", size = 10)

head(data)
get_mode = function(col) {
  return(names(sort(
    table(col), decreasing = T, na.last = T
  )[1]))
}

fill_missing_vals = function(data, stat_measure) {
  for (i in 1:ncol(data)) {
    if (is.numeric(data[, i])) {
      if (stat_measure == 'mean') {
        data[, i][is.na(data[, i])] = round(mean(data[, i], na.rm = TRUE),
                                            digits = 2)
      }
      if (stat_measure == 'std') {
        data[, i][is.na(data[, i])] = round(sd(data[, i],  na.rm = TRUE),
                                            digits = 2)
      }
      if (stat_measure == 'median') {
        data[, i][is.na(data[, i])] = round(median(data[, i],  na.rm = TRUE),
                                            digits = 2)
      }
    }
    else{
      data[, i][is.na(data[, i])] = get_mode(data[, i])
    }
  }
  
  return(data)
}

outlier_detection = function(data) {
  for (i in 1:ncol(data)) {
    if (is.numeric(data[,i])) {

      quartiles <- quantile(data[, i], probs = c(.25, .75), na.rm=TRUE)
      IQR <- IQR(data[, i])
      Lower <- quartiles[1] - 1.5 * IQR
      Upper <- quartiles[2] + 1.5 * IQR
      data <- subset(data, data[, i] >= Lower & data[, i] <= Upper)
      
    }
  }
  return (data)
}
data= read.csv("data/DS_prject.csv")
data = fill_missing_vals(data, "mean")
is.numeric(data$amount_request)

length(unique(data$amount_request))
data = outlier_detection(data)
str(data)
data= read.csv("saved.csv")
data= read.csv("data/output_df_changed.csv")
head(data)
data = factorize(data)
data = fill_missing_vals(data, "mean")

head(data)
str(data)
data = outlier_detection(data)
groupby = "Traffic.Type"

grouped_df = data %>%
  group_by(!!!rlang::syms(groupby)) %>%
  summarise(g = mean(amount_request))

head(grouped_df)

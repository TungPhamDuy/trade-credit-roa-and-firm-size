library(dplyr) #for manipulate data
library(readxl) #library use to read excel file
library(ggplot2) #library for plotting and data visualization

#Task 2: Create Dataset
#import file data
df = read_excel("C:/Users/Admin/Desktop/courses/Gói phần mềm ứng dụng trong tài chính 2/040522 Data Mid-term test Final.xlsx")
View(df)

#dataframe have the last row is not an observations, so we drop it out
df = df[-(nrow(df)), ]

#filter data for just HANOI STOCK EXCHANGE companies to a new dataset called hnx.df
hnx.df = filter(df, exchangename == "HANOI STOCK EXCHANGE") 

#create a new dataset (sample.df) with 100 autoselected samples
set.seed(961)
sample.df = hnx.df[sample(1:nrow(hnx.df), 100, replace = F),]
View(sample.df) #note: 1row no 517, 100row 655

#scan the dataset to see whether we have NA values
colSums(is.na(sample.df))
sum(is.na(sample.df))

#impute the missing values with median of the corresponding variable
sample.df %>% 
  mutate_if(is.numeric, function(x) ifelse(is.na(x), median(x, na.rm = T), x)) -> sample.df

#recheck for the NA values
colSums(is.na(sample.df))
sum(is.na(sample.df))


#Task 3: Report
#5 firms with highest trade credit (receivable) 
max(sample.df$receivable)
highest_receivable = sample.df[order(sample.df$receivable,decreasing=T)[1:5],]
View(highest_receivable)
cat(paste('Top 5 companies with the highest trade credit (receivable): \n', list(highest_receivable$firmname)))

#5 firms with lowest trade credit (receivable) 
min(sample.df$receivable)
lowest_receivable = sample.df[order(sample.df$receivable,decreasing=F)[1:5],]
View(lowest_receivable)
cat(paste('Top 5 companies with the lowest trade credit (receivable): \n', list(lowest_receivable$firmname)))

#the name of industries which the firms belong to
select(lowest_receivable, firmname, industry)
select(highest_receivable, firmname, industry)
a=highest_receivable$industry
b=lowest_receivable$industry
c=c(a,b)
cat(paste('Name of industries which the firms belong to: \n', list(unique(c))))


#add company size in to dataset
company.size=c()
for (i in rownames(sample.df)) {
  if (sample.df[i, 'totalequity'] <= 3000000000 | sample.df[i, 'revenue'] <= 10000000000) {
    company.size <- append(company.size, 'Micro')
  } else if ((10000000000 < sample.df[i, 'revenue'] & sample.df[i, 'revenue'] < 100000000000) | sample.df[i, 'totalequity'] < 50000000000) {
    company.size <- append(company.size, 'Small')
  } else if ((100000000000 > sample.df[i, 'totalequity']) & (300000000000 > sample.df[i, 'revenue'])) {
    company.size <- append(company.size, 'Medium')
  } else {
    company.size <- append(company.size, 'Large')
  }
}
company.size
#count number of company types
table(company.size)

#append into sample.df as new column named companysize
sample.df$companysize = company.size

#group data by the discrete variable and calculate median, mean, max, min, standard deviation of trade credit 
sample.df %>% 
  mutate(companysize = factor(companysize, levels = c("Large", "Medium", "Small", "Micro"))) %>%
  group_by(companysize) %>%
  summarise(
    median_tc = median(receivable),
    mean_tc = mean(receivable),
    max_tc = max(receivable),
    min_tc = min(receivable),
    sd_tc = sd(receivable)
  )

#median of ROA
median(sample.df$roa)


#group data by the continuous variable and calculate median, mean, max, min, standard deviation of trade credit
sample.df %>% 
  mutate(x=cut(sample.df$roa,breaks=c(min(sample.df$roa), 
                                      median(sample.df$roa), 
                                      max(sample.df$roa)),
               labels=c("Below Median","Above Median"))) %>% 
  group_by(x) %>% 
  filter(!is.na(x)) %>%  # filter out any rows where x is NA
  summarise(mean_tc=mean(receivable), 
            median_tc=median(receivable), 
            max_tc=max(receivable), 
            min_tc=min(receivable), 
            std_tc=sd(receivable)
  )

#Task 4: Data visualization
#histogram plot for trade credit receivable
ggplot(sample.df, aes(x = receivable)) +
  geom_histogram(fill = "steelblue", color = "black", bins = 100) +
  labs(title = "Histogram of Trade Credit/Receivable", x = "Trade Credit", y = "Frequency")


#scatter plot of ROA and receivable
ggplot(sample.df, aes(x=roa, y=receivable)) + 
  geom_point(size=3, color="steelblue") +
  labs(title = "Scatter plot of ROA and Trade Credit/Receivable", x="ROA", y="Trade Credit / Receivable") +
  theme_bw()

#reorder company by company size
sample.df$companysize <- factor(sample.df$companysize, levels = c("Micro", "Small", "Medium", "Large"))

#box plot of Trade credit with company size
ggplot(sample.df, aes(x=companysize, y=receivable, fill=companysize)) +
  geom_boxplot() +
  labs(title = "Box plot of Trade Credit/Receivable with Company Size", x="Company Size", y="Trade Credit / Receivable") +
  theme_bw()

#Scatter plot of ROA, Trade Credit/Receivable and Company Size
ggplot(sample.df, aes(x = receivable, y = roa , color = companysize)) +
  geom_point(size=3) +
  scale_color_manual(values = c("#66c2a5", "#fc8d62", "#8da0cb", "#e78ac3")) +
  labs(title = "Scatter plot of ROA, Trade Credit/Receivable and Company Size", x = "Trade Credit / Receivable", y = "ROA") +
  theme_bw()

#Task 5: Regression
#regression 
y = sample.df$receivable
x1 = sample.df$roa
x2 = sample.df$companysize
model = lm(y ~ x1 + x2, data = sample.df)

summary(model)

#Heteroscedastic test
#plot the graph of the model
par(mfrow = c(2, 2))
plot(model)

#Multicollinearity test
#install.packages("olsrr")
#library("olsrr")
#ols_vif_tol(model)


#Task 6: Using LOOP
#the LOOP 1
# choose the industry you want to count how many firm, example here is "Industrials"
chosen.industry01 = "Industrials"
firm.count01 = 0
for (i in 1:nrow(df)) {
  if (df[i, "industry"] == chosen.industry01) {
    firm.count01 = firm.count01 + 1
  }
}
#print the firm count
cat(paste("The number of companies in", chosen.industry01, "is", firm.count01))

#the LOOP 2
# choose the industry you want to count how many firm, example here is "Technology"
# and choose the trade credit limit for counting number of company above of, example here is "10ty"
chosen.industry02 = "Technology"
tradecredit.above.limit = 10000000000
firm.count02 = 0
for (i in 1:nrow(df)) {
  if (df[i, "industry"] == chosen.industry02 & df[i, "receivable"]> tradecredit.above.limit) {
    firm.count02 = firm.count02 + 1
  }
}
#print the firm count
cat(paste("The number of companies in", chosen.industry02, "and have trade credit/receivable above", tradecredit.above.limit, "is", firm.count02))

#end-------


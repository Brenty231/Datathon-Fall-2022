## The goal of this project is identify successful investment strategies for trading the two largest Cryptocurrencies by market cap with backtesting 

## Two largest Cryptocurrencies by market cap:
#  Bitcoin(BTC), Ethereum(ETH)

## The rise of cryptocurriencies in the last few years has taken the investment world by storm.
## Many investors, speculative young ones particularly, have attempted to garner investment profits
#  from this surge in popularity 

## they typically enter these investments with limited knowledge of crypto's key differences
#  with other types of investments such as regulation, ledger structure, and intrinsic value 

## On January 2, 2022, Bitcoin's hashrate(Total amount of computing power used by miners to mine new bitcoins) is at an all-time high)
#  Not only is this more efficient, it makes the network more robust and resistant to attacks

## This could entice more investors to enter this market as it will become more competitive but even more prone to volatility
## Cryptocurrencies are not supported by any business operations or assets. 
## There is essentially no simple way for investors and even portfolio managers to gauge how cryptocurrencies are valued and why their prices fluctuate 

## In other words, crypto markets seem to be primarily driven by speculative and psychological factors 
#  Therefore, 'passive' strategies are far less practical than they are with equities

## Crypto markets may be the most feasible for active investment strategies 

## Therefore, a major part of our trading strategies should be volatility 

## We will use the same time span for each token

library(data.table)
library(quantmod)
library(ggplot2)
library(rpart)
library(corrplot)
library(plotly)
library(psych)
library(aplpack)

bitData=getSymbols('BTC-USD', to='2021-12-31', auto.assign=F)
ethData=getSymbols('ETH-USD', to='2021-12-31', auto.assign=F)
bitData=data.frame(bitData)
setDT(bitData, keep.rownames = TRUE)
ethData=data.frame(ethData)
setDT(ethData, keep.rownames=TRUE)

# Let's do some exploratory analysis 
# To account for/leverage crypto's volatility, we should calculate returns for each year/qtr pair 
names(bitData)
names(ethData)
names(bitData)=c('Date', 'Open', 'High', 'Low', 'Close', 'Volume', 'Adjusted')
names(ethData)=c('Date', 'Open', 'High', 'Low', 'Close', 'Volume', 'Adjusted')

# Add columns for returns/clean data:
bitData[, quarter:=quarter(Date)]
bitData[, year:=year(Date)]
bitData[, month:=month(Date)]
bitData[, c('p1', 'p5', 'p10', 'p20', 'p45', 'p60'):=shift(Adjusted, n=c(1,5,10,20,45,60), type='lead'), by=.(year, quarter)]
bitData[, ret1:=(p1/Adjusted)-1]
bitData[, ret5:=(p5/Adjusted)-1]
bitData[, ret10:=(p10/Adjusted)-1]
bitData[, ret20:=(p20/Adjusted)-1]
bitData[, ret45:=(p45/Adjusted)-1]
bitData[, ret60:=(p60/Adjusted)-1]

ethData[, year:=year(Date)]
ethData[, quarter:=quarter(Date)]
ethData[, c('p1', 'p5', 'p10', 'p20', 'p45', 'p60'):=shift(Adjusted, n=c(1,5,10,20,45,60), type='lead'), by=.(year, quarter)]
ethData[, ret1:=(p1/Adjusted)-1]
ethData[, ret5:=(p5/Adjusted)-1]
ethData[, ret10:=(p10/Adjusted)-1]
ethData[, ret20:=(p20/Adjusted)-1]
ethData[, ret45:=(p45/Adjusted)-1]
ethData[, ret60:=(p60/Adjusted)-1]

# What is the best investment horizon for Bitcoin?

summary(bitData$ret1)
summary(bitData$ret5)
summary(bitData$ret10)
summary(bitData$ret20)
summary(bitData$ret45)
summary(bitData$ret60)

# Our mean return has a positive relationship with investment horizon for Bitcoin
# Will our mean return be higher if we expand our investment horizon into different quarters?
bits=getSymbols('BTC-USD', to='2021-12-31', auto.assign=F)
bitsy=data.frame(bits)
setDT(bitsy, keep.rownames=TRUE)
names(bitsy)=c('Date', 'Open', 'High', 'Low', 'Close', 'Volume', 'Adjusted')
bitsy[, year:=year(Date)]
bitsy[, c('p1', 'p5', 'p10', 'p20', 'p45', 'p60'):=shift(Adjusted, n=c(1,5,10,20,45,60), type='lead'), by=year]
bitsy[, ret1:=(p1/Adjusted)-1]
bitsy[, ret5:=(p5/Adjusted)-1]
bitsy[, ret10:=(p10/Adjusted)-1]
bitsy[, ret20:=(p20/Adjusted)-1]
bitsy[, ret45:=(p45/Adjusted)-1]
bitsy[, ret60:=(p60/Adjusted)-1]
summary(bitsy$ret1)
summary(bitsy$ret5)
summary(bitsy$ret10)
summary(bitsy$ret20)
summary(bitsy$ret45)
summary(bitsy$ret60)
## Some returns are higher for quarterly groupings and some for yearly 

## To adjust for outliers, let's use the geometric mean for comparison:
geometric.mean(bitsy[is.finite(ret1),ret1])
geometric.mean(bitData[is.finite(ret1), ret1])
geometric.mean(bitsy[is.finite(ret5), ret5])
geometric.mean(bitData[is.finite(ret5), ret5])
geometric.mean(bitsy[is.finite(ret10),ret10])
geometric.mean(bitData[is.finite(ret10), ret10])
geometric.mean(bitsy[is.finite(ret20),ret20])
geometric.mean(bitData[is.finite(ret20), ret20])
geometric.mean(bitsy[is.finite(ret45), ret45])
geometric.mean(bitData[is.finite(ret45), ret45])
geometric.mean(bitsy[is.finite(ret60),ret60])
geometric.mean(bitData[is.finite(ret60), ret60])
rm(bits, bitsy)

## We will use quarterly return intervals because they garner higher geometric mean returns for each horizon
# and account for bias with Bitcoin's milestones over the years
## Although quarterly intervals have higher returns, there is bias in the data from the 'na' values created by quarter grouping
## let's see what months had the highest returns for each horizon and quantify the risk levels with certain investment strategies

posbit=bitData[ret1>0]
boxplot(ret1~month, data=posbit, main='Daily Positive Return Distributions by Month: Bitcoin')
pairs(bitData[,c('month', 'ret1')])
sd(bitData[month==2, ret1])


bit_returns=data.frame(bitData[is.finite(ret1)& is.finite(ret5) & is.finite(ret10) & is.finite(ret20) 
                               &is.finite(ret45)& is.finite(ret60), 
                               .(quarter,ret1, ret5, ret10, ret20, ret45, ret60)])
corbit=cor(bit_returns)
corrplot(corbit, method='ellipse', main='')
summary(corbit)
corbit

## Takeaways from correlation plot:
## the risk of buying Bitcoin is amplified with longer investment horizons 
  # the darker and thinner the blue ovals are in the graph, the stronger positive correlation
  # All the ovals are blue, confirming our expectation on Bitcoin's speculative fluctuations 
  # All returns have slight positive correlations with the horizon's quarter, the strongest being with 10 day-returns 
   # This also tells us that returns were generally slightly higher later in years
   # 20 day trading intervals have the highest median return 

## Now let's find the months with the highest returns for each investment horizon:
## Let's visualize the relationship between month and returns for each horizon

# daily returns:

ggplot(data=bitData[is.finite(ret1)&ret1>0], aes(x=month))+geom_bar(fill='darksalmon')+scale_x_continuous(breaks=1:12)+ggtitle('Positive Daily Returns by Month: Bitcoin, September 2014 - December 2021')
## The plot shows that the Q4 months had the highest frequency of positive daily returns 
## 3 Highest counts(descending): October, November, December (Q4 clearly has highest count of positive returns)
NROW(bitData[is.finite(ret1)&ret1>0&month==10]) ## October has 144 positive daily returns

## 5-day returns:

ggplot(data=bitData[is.finite(ret5)&ret5>0], aes(x=month))+geom_bar(fill='darkseagreen4')+scale_x_continuous(breaks=1:12)+ggtitle('Positive 5-Day Returns by Month: Bitcoin, September 2014 - December 2021')
## 3 Highest counts(descending): October, April, February
NROW(bitData[is.finite(ret5)&ret5>0&month==10]) ## October has 170 positive 5-day returns
## let's see which quarter had the highest number of 5-day returns 
ggplot(data=bitData[is.finite(ret5)&ret5>0], aes(x=quarter))+geom_bar(fill='tan1')+scale_x_continuous(breaks=1:4)+ggtitle('Positive 5-Day Returns by Quarter: Bitcoin, September 2014 - December 2021')
# Q4 again has the highest frequency of 5-day returns 

## 10-day returns: 

ggplot(data=bitData[is.finite(ret10)&ret10>0], aes(x=month))+geom_bar(fill='firebrick2')+scale_x_continuous(breaks=1:12)+ggtitle('Positive 10-Day Returns by Month: Bitcoin, September 2014 - December 2021')
## 3 highest counts(descending): October, April, February
NROW(bitData[is.finite(ret10)&ret10>0&month==10]) ## October has 186 positive 10-day returns
## let's see which quarter had the highest number of 10-day returns 
ggplot(data=bitData[is.finite(ret10)&ret10>0], aes(x=quarter))+geom_bar(fill='saddlebrown')+scale_x_continuous(breaks=1:12)+ggtitle('Positive 10-Day Returns by Quarter: Bitcoin, September 2014 - December 2021')
## Q4 has the highest number of 10-day returns

## 20-day returns:

ggplot(data=bitData[is.finite(ret20)&ret20>0], aes(x=month))+geom_bar(fill='cyan3')+scale_x_continuous(breaks=1:12)+ggtitle('Positive 20-Day Returns by Month: Bitcoin, September 2014 - December 2021')
## 3-highest counts(descending): October, April, July 
NROW(bitData[is.finite(ret20)&ret20>0&month==10]) ## October has 190 positive 20-day returns
## Which quarter had the highest number of 20-day returns?
ggplot(data=bitData[is.finite(ret20)&ret20>0], aes(x=quarter))+geom_bar(fill='cyan')+scale_x_continuous(breaks=1:12)+ggtitle('Positive 20-Day Returns by Quarter: Bitcoin, September 2014 - December 2021')
## Q4 has the highest number of 20-day returns

## 45-day returns:

ggplot(data=bitData[is.finite(ret45)&ret45>0], aes(x=month))+geom_bar(fill='chocolate3')+scale_x_continuous(breaks=1:12)+ggtitle('Positive 45-Day Returns by Month: Bitcoin, September 2014 - December 2021')
## 3-highest counts(descending): October, April, January 
NROW(bitData[is.finite(ret45)&ret45>0&month==10]) ## October has 157 positive 45-day returns
## Which quarter had the highest number of 45-day returns?
ggplot(data=bitData[is.finite(ret45)&ret45>0], aes(x=quarter))+geom_bar(fill='chocolate2')+scale_x_continuous(breaks=1:12)+ggtitle('Positive 45-Day Returns by Quarter: Bitcoin, September 2014 - December 2021')
NROW(bitData[is.finite(ret45)&ret45>0&quarter==4])  ## 222 positive 45-day returns in Q4
NROW(bitData[is.finite(ret45)&ret45>0&quarter==2])  ## 220 positive 45-day returns in Q2
## Q4 has the highest number of 45-day returns

## 60-day returns:

ggplot(data=bitData[is.finite(ret60)&ret60>0], aes(x=month))+geom_bar(fill='chocolate1')+scale_x_continuous(breaks=1:12)+ggtitle('Positive 60-Day Returns by Month: Bitcoin, September 2014 - December 2021')
## 3 highest counts: April, October, January
NROW(bitData[is.finite(ret60)&ret60>0&month==4]) ## April has 148 positive 60-day returns
## Which quarter had the highest number of 60-day returns?
ggplot(data=bitData[is.finite(ret60)&ret60>0], aes(x=quarter))+geom_bar(fill='yellowgreen')+scale_x_continuous(breaks=1:12)+ggtitle('Positive 60-Day Returns by Quarter: Bitcoin, September 2014 - December 2021')
## Q2 had the highest number of 60-day returns 

## Let's use bagplots to see which months had the most number of outliers for each horizon:
## The inner oval represents 50% of the data points while the red points outside the 'fence' or outside oval, represent outliers

## Daily returns:
bit1=bitData[is.finite(ret1)]
bagplot(bit1$month, bit1$ret1, cex=1.2, col.baghull='dodgerblue4', main='Bitcoin Daily Return Distribution by Month', xlab='Month', ylab='Return')
## the beginning and end of years appear to have the most volatility as illustrated by outliers
## August and October have the fewest number of outliers 

## Let's find the standard deviation of daily returns for each month to better gauge volatility
a=1:12
sd1=c()
for (i in a){
  sd1[i]=(sd(bitData[is.finite(ret1)&month==i, ret1]))
}
sdm=data.frame(month=a, sd1 )
sdm
## January had the highest standard deviation for daily returns, followed by March and December

## 5-day returns:
bit5=bitData[is.finite(ret5)]
bagplot(bit5$month, bit5$ret5, cex=1.2, main='Bitcoin 5-Day Return Distribution by Month', xlab='Month', ylab='Return')
## December has the most volatile 5 day price fluctuations 
## The distribution of 5-day returns for October has no outliers and appears to be skewed above the median value 

sd5=c()
for (i in a){
  sd5[i]=(sd(bitData[is.finite(ret5)&month==i, ret5]))
}
sdm=data.frame(month=a, sd5 )
sdm
## December had the highest standard deviation for 5-day returns, followed by January and March

## 10-day returns:
bit10=bitData[is.finite(ret10)]
bagplot(bit10$month, bit10$ret10, cex=1.2, main='Bitcoin 10-Day Return Distribution by Month', xlab='Month', ylab='Return')
## November shows the most volatility, September shows the least
## Again all returns for October lie within the 'fence' and appear to be skewed above the median, as is the case with February
sd10=c()
for (i in a){
  sd10[i]=(sd(bitData[is.finite(ret10)&month==i, ret10]))
}
sdm=data.frame(month=a, sd10 )
sdm
## November has the highest standard deviation, followed by December and January 

## 20-day returns:
bit20=bitData[is.finite(ret20)]
bagplot(bit20$month, bit20$ret20, cex=1.2, main='Bitcoin 20-Day Return Distribution by Month', xlab='Month', ylab='Return')
## November is the most volatile month, although all outliers are positive
## The least volatility occurs in September and March 
sd20=c()
for (i in a){
  sd20[i]=(sd(bitData[is.finite(ret20)&month==i, ret20]))
}
sdm=data.frame(month=a, sd20 )
sdm
## November has the highest standard deviation, followed by May and January 

## 45-day returns:
bit45=bitData[is.finite(ret45)]
bagplot(bit45$month, bit45$ret45, cex=1.2,  main='Bitcoin 45-Day Return Distribution by Month', xlab='Month', ylab='Return')
## October and November show the most outliers(December is not included since our horizons are contained within quarters)
## Returns for October, among other months, are skewed above the median 
sd45=c()
for (i in a){
  sd45[i]=(sd(bitData[is.finite(ret45)&month==i, ret45]))
}
sdm=data.frame(month=a, sd45 )
sdm
## November has the highest standard deviation, followed by October and April

## 60-day returns:
bit60=bitData[is.finite(ret60)]
bagplot(bit60$month, bit60$ret60, cex=1.2,  main='Bitcoin 60-Day Return Distribution by Month', xlab='Month', ylab='Return')
## October shows the most volatility, with values again shifted above the median 
sd60=c()
for (i in a){
  sd60[i]=(sd(bitData[is.finite(ret60)&month==i, ret60]))
}
sdm=data.frame(month=a, sd60 )
sdm

## October has the highest standard deviation, followed by November and April 
## Let's use a 3d plot to visualize 20-day Bitcoin returns in October over the eight year period(2014-2021)
Octoberbit=bitData[month==10, .(Date,ret1,ret5, ret10, ret20, ret45, ret60,year)]
setDT(Octoberbit)
Octoberbit[, day:=mday(Date)]
Octoberbit$sign=as.factor(Octoberbit$sign)
october=plot_ly(Octoberbit, x=~day, y=~year, z=~ret20, marker=list(color=~ret20, colorscale=c('saddlebrown', 'dodgerblue4'), showscale=TRUE))
october=october %>% add_markers()
october=october %>% layout(scene=list(xaxis=list(title='Day'), yaxis=list(title='year'), zaxis=list(title='ret20')))
october
## Let's see what part of October had the best returns for each interval:
#  This time let's use a regression tree
#  If the condition at a node is true, evaluate the criteria to the bottom left
#  The regression tree predicts the value of daily return based on the conditions at the nodes 
## The plot shows that October 2015 featured some very high returns for buying Bitcoin and the higher 20-day returns came in the middle of the month 
## On October 22, 2015 The EU declared no value added taxes on Bitcoin an made it a 'bona fide' currency
## this is insightful because any events that make the token easier to buy and more available should make the price rise. Perhaps similar to the price behavior we see with stock splits
rm(bit1, bit10, bit20, bit5, bitfinale, bit45, bit60, bit_returns, posbit, corbit, sdm, october)

# Daily returns:
Octoberbit[, day:=mday(Date)]
Octoberbit1=Octoberbit[, c('ret1', 'day')]
retrel=rpart(ret1~., data=Octoberbit1)
plot(retrel)
text(retrel, cex=1.5, col='firebrick3', xpd=TRUE)
## The regression tree predicts that trading on the 4th-14th days of the month yield a return of 1.05%, and trading on the 24th day yields a 1.05% return

## 5-day returns:
Octoberbit1=Octoberbit[, c('ret5', 'day')]
retrel=rpart(ret5~., data=Octoberbit1)
plot(retrel)
text(retrel, cex=1.5, col='firebrick3', xpd=TRUE)
## The regression tree predicts that trading on the 23rd-31st days of the month yield a return of 5.07%, and trading on the 3-12th days yields a 4.90% return

## 10-day returns:
Octoberbit[, day:=mday(Date)]
Octoberbit1=Octoberbit[, c('ret10', 'day')]
retrel=rpart(ret10~., data=Octoberbit1)
plot(retrel)
text(retrel, cex=1.5, col='firebrick3', xpd=TRUE)
## The regression tree predicts that trading on the 23th-31st days of the month yield a return of 8.97%, and trading on the first 11 days yields a 8.25% return

# 20-day returns:
Octoberbit1=Octoberbit[, c('ret20', 'day')]
retrel=rpart(ret20~., data=Octoberbit1)
plot(retrel)
text(retrel, cex=1.5, col='firebrick3', xpd=TRUE)
## the regression tree for 20-day returns predicts that buying on the first 24 days of the month yields a return of 13.3%

##Regression trees are not feasible for 45 and 60-day returns, so we will use a linear model instead
## 45-day returns:
Octoberbit1=Octoberbit[, c('ret45', 'day')]
lm(ret45~day, data=Octoberbit1)
## The linear model predicts that raising the day of October by 1 lowers the 45-day return by 0.01856%

## 60-day returns:
Octoberbit1=Octoberbit[, c('ret60', 'day')]
lm(ret60~day, data=Octoberbit1)
## The linear model predicts that raising the day of October by 1 lowers the 60-day return by 0.35%

# ** Key insights from Bitcoin price analysis ** :

## Keeping investment horizons within distinct quarters has proven to be more fruitful
## Q4 has garnered the highest number of positive returns for each investment timeline 
## October has had more positive 20-day returns than daily, 5-day, and 10-day returns despite having more 'na' values in the data
## October has been the most successful month for every investment horizon except 60 days. It has been top 2 for every horizon

## It is important to note that investors have different preferences and their own ideal strategies based on risk tolerance and other factors 
## Therefore, a single investment strategy will not be optimal for every investor
#  However, the insights from this analysis are still useful for any investor looking to enhance their Bitcoin trading profits 

## Our trading strategy to be tested for Bitcoin will entail trading in 20-day intervals during October 
## going back to our correlation plot earlier in the script, daily returns had the lowest correlation with 20-day returns
#  Therefore, this would be the best option for hedging the risk of trading in 20-day intervals 

## We will buy $2000 of Bitcoin every October from 2014-2021 on the first day of the month, $1000 of which we will sell the next day and the other $1000 after 20 days
## What would be our compound return

Octoberbit[, opdate:=first(Date), by=year]
bitfinale=Octoberbit[Date==opdate]
dayreturn=mean(bitfinale[,ret1])
dayreturn  # -0.48%
twentiesreturn=mean(bitfinale[,ret20])
twentiesreturn # 12.51%
sum=(8000*(1+dayreturn))+(8000*(1+twentiesreturn)) # final value = 16,962.21

comreturn=(sum/16000)^(1/8)-1  # 0.73%
## Results of strategy: 
## We earned a total return over the eight years of 12.03% and an annual compound return of 0.73%

## However, we should note that this was a rather risk averse strategy by Crypto standards since we used two different investment horizons
## Furthermore, October had relatively low volatility for 1 and 20 day intervals as shown earlier in the bagplots and sd tables 
## What if we put all our money into the 20 day trading intervals?:
sum1=16000*(1+twentiesreturn) # final value = $18,001.93
comreturn=(sum1/16000)^(1/8)-1  # 1.48%

## If we trade in only 20-day intervals, we walk away with more money but this strategy comes with higher risk than the previous one 
# A risk tolerant investor may prefer this strategy over the preceding 
## We can adjust the risk level of our strategy by changing the month(s) we trade in, our investment horizon(s), and using different factors in conjunction with each other(yielding different correlations)

## Lastly let's test a strategy using 2022 data 
# Earlier in the analysis we saw that April and February had the second and third highest counts of positive returns, although February had a higher standard deviation
## Let's load Bitcoin price data from 2022 and trade in 5 and 10-day intervals in February 2022
## This time we will invest $100,000 into Bitcoin on February 1st, selling $50,000 on the 6th and the rest on the 11th(we will buy at the opening price on the 1st and sell at the closing price on the 6th and 11th)

bitfeb=getSymbols('BTC-USD', from='2022-02-01', to='2022-02-11', auto.assign=F)
Bitfeb=data.frame(bitfeb)
setDT(Bitfeb)
names(Bitfeb)=c('opening','high' ,'low', 'closing', 'Volume', 'Adjusted')
Bitfeb[, c('p5', 'p10'):=shift(Adjusted, n=c(5,10), type='lead')]
Bitfeb[, ret5:=(p5/opening)-1]
Bitfeb[, ret10:=(p10/opening)-1]
fiveret=Bitfeb[1, ret5]
tenret=Bitfeb[1,ret10]
sum=(50000*(1+fiveret))+(50000*(1+tenret))##$110,208.52
(sum/100000)-1 ## 10.21% total return 

## We earned a 10.21% return with this strategy. If we wanted to take this a step further, we could use a regression tree to predict the parts of February with the highest 5 and 10-day returns

rm(bitfeb, Bitfeb, bitfinale, retrel,)

## Ethereum:

## Before exploring Ethereum data, we should note some differences between Bitcoin and Ethereum that may be insightful in our analysis:
#  Both tokens are traded using blockchain networks
#  As time progressed, people began to recognize the alternative uses of blockchain technology
#  Ethereum allows for the development of smart contracts and decentralized applications to be built and used without any runtime, control,fraud, or interference from third parties
#  Smart contracts and entail an agreement between the buyer and seller written into code. These allow transactions and agreements to be executed without any authority or legal systems. This makes transactions transparent, traceable and irreversible 
#  Decentralized apps are digital applications or programs that run on blockchain technology 
#  Ethereum transactions are executed faster than Bitcoin, giving it a tendency to be more competitve and volatile

## Let's begin exploring the data:

# What is the best investment horizon for Ethereum?

summary(ethData$ret1) #0.2947 %
summary(ethData$ret5) #1.19 %
summary(ethData$ret10)#2.16 %
summary(ethData$ret20)#4.60 %
summary(ethData$ret45)#8.66 %
summary(ethData$ret60)#10.32%

# Our mean return has a positive relationship with investment horizon for Ethereum
# Will our mean return be higher if we expand our investment horizon into different quarters?

ets=getSymbols('ETH-USD', to='2021-12-31', auto.assign=F)
etsy=data.frame(ets)
setDT(etsy, keep.rownames=TRUE)
names(etsy)=c('Date', 'Open', 'High', 'Low', 'Close', 'Volume', 'Adjusted')
etsy[, year:=year(Date)]
etsy[, c('p1', 'p5', 'p10', 'p20', 'p45', 'p60'):=shift(Adjusted, n=c(1,5,10,20,45,60), type='lead'), by=year]
etsy[, ret1:=(p1/Adjusted)-1]
etsy[, ret5:=(p5/Adjusted)-1]
etsy[, ret10:=(p10/Adjusted)-1]
etsy[, ret20:=(p20/Adjusted)-1]
etsy[, ret45:=(p45/Adjusted)-1]
etsy[, ret60:=(p60/Adjusted)-1]
summary(etsy$ret1) #0.2939 %
summary(etsy$ret5) #1.3309 %
summary(etsy$ret10)#2.469 %
summary(etsy$ret20)#4.643 %
summary(etsy$ret45)#8.683 %
summary(etsy$ret60)#9.9   %
# Again the results are mized for arithmetic average returns, now let's use the geometric average:

geometric.mean(etsy[is.finite(ret1),ret1])
geometric.mean(ethData[is.finite(ret1), ret1])
geometric.mean(etsy[is.finite(ret5), ret5])
geometric.mean(ethData[is.finite(ret5), ret5])
geometric.mean(etsy[is.finite(ret10),ret10])
geometric.mean(ethData[is.finite(ret10), ret10])
geometric.mean(etsy[is.finite(ret20),ret20])
geometric.mean(ethData[is.finite(ret20), ret20])
geometric.mean(etsy[is.finite(ret45), ret45])
geometric.mean(ethData[is.finite(ret45), ret45])
geometric.mean(etsy[is.finite(ret60),ret60])
geometric.mean(ethData[is.finite(ret60), ret60])
rm(etsy, ets)
## The geometric mean for quarterly intervals is higher, so we will group our horizons by quarter again 

## Which month had the highest count of positive returns?
eth_returns=data.frame(ethData[is.finite(ret1)& is.finite(ret5) & is.finite(ret10) & is.finite(ret20) 
                               &is.finite(ret45)& is.finite(ret60), 
                               .(quarter,ret1, ret5, ret10, ret20, ret45, ret60)])
coreth=cor(eth_returns)
corrplot(coreth, method='ellipse', main='')
summary(coreth)

## Takeaways from correlation plot:
# Returns for all investment horizons have a negative correlation with the quarter. Returns tend to be higher earlier in the year 
# As was the case with Bitcoin, an Ethereum investment becomes more risky with a longer investment horizon 

## Now let's find the months with the highest returns for each investment horizon:
## Let's visualize the relationship between month and returns for each horizon
ethData[, month:=month(Date)]
ethData[, qtr:=quarter(Date)]

# daily returns:

ggplot(data=ethData[is.finite(ret1)&ret1>0], aes(x=month))+geom_bar(fill='darksalmon')+scale_x_continuous(breaks=1:12)+ggtitle('Positive Daily Returns by Month: Ethereum, September 2014 - December 2021')
## 3 highest return counts: December, April, November 
NROW(ethData[is.finite(ret1)&ret1>0 & month==12]) ## 77 positive daily returns in December
## What quarter had the highest number of positive returns?
ggplot(data=ethData[is.finite(ret1)&ret1>0], aes(x=qtr))+geom_bar(fill='dodgerblue4')+scale_x_continuous(breaks=1:4)+ggtitle('Positive Daily Returns by Quarter: Ethereum, September 2014-December 2021')
## Q4 had the highest number of positive daily returns 

## 5-day returns:
ggplot(data=ethData[is.finite(ret5)&ret5>0], aes(x=month))+geom_bar(fill='firebrick2')+scale_x_continuous(breaks=1:12)+ggtitle('Positive 5-day Returns by Month, Ethereum, September 2014-December 2021')
## 3 highest return counts: April, October, November 
NROW(ethData[is.finite(ret5)&ret5>0 &month==4]) ## 88 positive 5-day returns in April 
# What quarter had the highest number of positive 5-day returns?
ggplot(data=ethData[is.finite(ret5)&ret5>0], aes(x=qtr))+geom_bar(fill='coral')+scale_x_continuous(breaks=1:4)+ggtitle('Positive 5-day returns by Quarter: Ethereum, September 2014-December 2021')
##Q4 had the highest number of 5-day returns 

## 10-Day returns:
ggplot(data=ethData[is.finite(ret10)&ret10>0], aes(x=month))+geom_bar(fill='firebrick2')+scale_x_continuous(breaks=1:12)+ggtitle('Positive 10-day Returns by Month, Ethereum, September 2014-December 2021')
## 3 highest return counts: April, October, January 
NROW(ethData[is.finite(ret10)&ret10>0 &month==4]) ## 99 positive 10-day returns in April 
# What quarter had the highest number of positive 5-day returns?
ggplot(data=ethData[is.finite(ret10)&ret10>0], aes(x=qtr))+geom_bar(fill='coral')+scale_x_continuous(breaks=1:4)+ggtitle('Positive 10-day returns by Quarter: Ethereum September 2014-December 2021')
##Q4 had the highest number of 10-day returns 

## 20-Day returns:
ggplot(data=ethData[is.finite(ret20)&ret20>0], aes(x=month))+geom_bar(fill='darkgoldenrod1')+scale_x_continuous(breaks=1:12)+ggtitle('Positive 20-day Returns by Month, Ethereum, September 2014-December 2021')
## 3 highest return counts: April, October, January 
NROW(ethData[is.finite(ret20)&ret20>0 &month==4]) ## 106 positive 20-day returns in April 
# What quarter had the highest number of positive 20-day returns?
ggplot(data=ethData[is.finite(ret20)&ret20>0], aes(x=qtr))+geom_bar(fill='coral')+scale_x_continuous(breaks=1:4)+ggtitle('Positive 20-day returns by Quarter: Ethereum September 2014-December 2021')
##Q4 had the highest number of 20-day returns 

## 45-Day returns:
ggplot(data=ethData[is.finite(ret45)&ret45>0], aes(x=month))+geom_bar(fill='darkgoldenrod1')+scale_x_continuous(breaks=1:12)+ggtitle('Positive 45-day Returns by Month, Ethereum, September 2014-December 2021')
## 3 highest return counts: April, July, January 
NROW(ethData[is.finite(ret45)&ret45>0 &month==4]) ## 103 positive 45-day returns in April 
# What quarter had the highest number of positive 45-day returns?
ggplot(data=ethData[is.finite(ret45)&ret45>0], aes(x=qtr))+geom_bar(fill='coral')+scale_x_continuous(breaks=1:4)+ggtitle('Positive 45-day returns by Quarter: Ethereum September 2014-December 2021')
##Q4 had the highest number of 45-day returns 

## 60-Day returns:
ggplot(data=ethData[is.finite(ret60)&ret60>0], aes(x=month))+geom_bar(fill='darkgoldenrod1')+scale_x_continuous(breaks=1:12)+ggtitle('Positive 60-day Returns by Month, Ethereum, September 2014-December 2021')
## 3 highest return counts: April, July, January 
NROW(ethData[is.finite(ret60)&ret60>0 &month==4]) ## 92 positive 60-day returns in April 
# What quarter had the highest number of positive 60-day returns?
ggplot(data=ethData[is.finite(ret60)&ret60>0], aes(x=qtr))+geom_bar(fill='coral')+scale_x_continuous(breaks=1:4)+ggtitle('Positive 60-day returns by Quarter: Ethereum September 2014-December 2021')
##Q2 had the highest number of 20-day returns 

## Let's use bagplots again to see which months had the most number of outliers for each horizon:
## The inner oval represents 50% of the data points while the red points outside the 'fence' or outside oval, represent outliers

## Daily returns:
et1=ethData[is.finite(ret1), .(ret1, month)]
bagplot(et1$month, et1$ret1, cex=1.2, col.baghull='dodgerblue4', main='Ethereum Daily Return Distribution by Month', xlab='Month', ylab='Return')
x=c()
for(i in a){
  x[i]=sd(et1[month==i&is.finite(ret1), ret1])
}
y=data.frame(month=a, x)
y
## May has the most volatility, followed by January and March 

## 5-day returns:
et5=ethData[is.finite(ret5), .(ret5,month)]
bagplot(et5$month, et5$ret5, cex=1.2, col.baghull='dodgerblue4',  main='Ethereum 5-Day Return Distribution by Month', xlab='Month', ylab='Return')
x=c()
for(i in a){
  x[i]=sd(et5[month==i&is.finite(ret5), ret5])
}
y=data.frame(month=a, x)
y
## December has the most outliers
## January and March both have 2 outliers
## April has no outliers, with points slightly skewed above the median
## October is the month with the tightest spread of values 
## January has the most volatility, followed by December and March 

## 10-day returns:
et10=ethData[is.finite(ret10), .(ret10,month)]
bagplot(et10$month, et10$ret10, cex=1.2, col.baghull='dodgerblue4',  main='Ethereum 10-Day Return Distribution by Month', xlab='Month', ylab='Return')
x=c()
for(i in a){
  x[i]=sd(et10[month==i&is.finite(ret10), ret10])
}
y=data.frame(month=a, x)
y
## December is the only month with outliers 
## April's distribution is skewed above the median 
## October again has the tightest spread of values 
## December is the most volatile month, followed by May and January 

## 20-day returns
et20=ethData[is.finite(ret20), .(ret20,month)]
bagplot(et20$month, et20$ret20, cex=1.2, col.baghull='dodgerblue4',  main='Ethereum 20-Day Return Distribution by Month', xlab='Month', ylab='Return')
x=c()
for(i in a){
  x[i]=sd(et20[month==i&is.finite(ret20), ret20])
}
y=data.frame(month=a, x)
y
## October has the lowest volatility
## the plot contains no outliers at all 
## The most volatile month is July, followed by November and December
## April's values are skewed above the mean

##45-day returns:
et45=ethData[is.finite(ret45), .(ret45,month)]
bagplot(et45$month, et45$ret45, cex=1.2, col.baghull='dodgerblue4',  main='Ethereum 45-Day Return Distribution by Month', xlab='Month', ylab='Return')
x=c()
for(i in a){
  x[i]=sd(et45[month==i&is.finite(ret45), ret45])
}
y=data.frame(month=a, x)
y
## November has the highest volatility, followed by July and May 

##60-day returns:
et60=ethData[is.finite(ret60), .(ret60,month)]
bagplot(et60$month, et60$ret60, cex=1.2, col.baghull='dodgerblue4',  main='Ethereum 60-Day Return Distribution by Month', xlab='Month', ylab='Return')
x=c()
for(i in a){
  x[i]=sd(et60[month==i&is.finite(ret60), ret60])
}
y=data.frame(month=a, x)
y
## November has the highest volatility, followed by May and July 
## The plot contains no outliers

## Looking back at the return count analysis, April was the most successful month for trading Ethereum 
## Let's use a 3d plot again to visualize 20-day returns in April across different days and years 
aprileth=ethData[month==4, .(Date,ret1,ret5, ret10, ret20, ret45, ret60,year)]
setDT(aprileth)
aprileth[, day:=mday(Date)]
aprileth$sign=as.factor(aprileth$sign)
april=plot_ly(aprileth, x=~day, y=~year, z=~ret20, marker=list(color=~ret20, colorscale=c('saddlebrown', 'dodgerblue4'), showscale=TRUE))
april=april %>% add_markers()
april=april %>% layout(scene=list(xaxis=list(title='Day'), yaxis=list(title='year'), zaxis=list(title='ret20')))
april
## April 2018, 2019, and 2021 were the best years for 20-day Ethereum returns in April 
## A series of upgrades were made to the Ethereum network in 2021 and one of these came in April
# A protocol of the 'Berlin Hard Fork' introduced new transaction types that lowered gas costs(cost of energy to execute transactions) and improved overall processing on the network
# This, among other upgrades took place in March 2021. Once, they were fully implemented by April more traders were likely drawn to the network and this likely drove the price up

## Before we test an investment strategy, lets use regression trees to see which parts of April had the best returns
rm(bitfinale, Octoberbit, Octoberbit1)

# Daily returns:

aprileth[, day:=mday(Date)]
aprileth1=aprileth[, c('ret1', 'day')]
retrel=rpart(ret1~., data=aprileth1)
plot(retrel)
text(retrel, cex=1.5, col='firebrick3', xpd=TRUE)
## the regression tree predicts that trading on the first 2 days of April yields a daily return of 3.9% and trading on the 25-30th days give produces a 2% return 

## 5-day returns:
aprileth1=aprileth[, c('ret5', 'day')]
retrel=rpart(ret5~., data=aprileth1)
plot(retrel)
text(retrel, cex=1.5, col='firebrick3', xpd=TRUE)
## The regression tree predicts that trading on the last 4 days of the month yield a 5-day return of 11.25%%, and trading before this date yields a return of 6.53%

## 10-day returns:
aprileth1=aprileth[, c('ret10', 'day')]
retrel=rpart(ret10~., data=aprileth1)
plot(retrel)
text(retrel, cex=1.5, col='firebrick3', xpd=TRUE)
## The regression tree predicts that trading on the 23th-31st days of the month yields a 10-day return of 17.22%, and trading on the first 22 days yields a 8.25% return

# 20-day returns:
aprileth1=aprileth[, c('ret20', 'day')]
retrel=rpart(ret20~., data=aprileth1)
plot(retrel)
text(retrel, cex=1.5, col='firebrick3', xpd=TRUE)
## the regression tree for 20-day returns predicts that buying on the first 26 days of the month yields a return of 29.03% and trading on the last 4 days yields 18.05%

## 45-day returns:
aprileth1=aprileth[, c('ret45', 'day')]
retrel=rpart(ret45~., data=aprileth1)
plot(retrel)
text(retrel, cex=1.5, col='firebrick3', xpd=TRUE)
## the regression tree for 45-day returns predicts that buying on the first 2 days of the month yields a return of 65% and trading on the 3rd-6th days yields 52.2%

## 60-day returns:
aprileth1=aprileth[, c('ret60', 'day')]
retrel=rpart(ret60~., data=aprileth1)
plot(retrel)
text(retrel, cex=1.5, col='firebrick3', xpd=TRUE)
## the regression tree for 60-day returns predicts that buying on the first 5 days of the month yields a return of 54.62% and trading on the last 6-11th days yields 40.52%

rm(et1, et10, et20, et45, et60, eth_returns, retrel, y, coreth, et5, aprileth1, april)

## **Key insights from Ethereum price analysis**

## April was the most successful month for trading Ethereum in 5 of the 6 investment horizons used 
#  20-day trading intervals had the highest count of positive returns for April 
## December and January were particularly volatile months for daily, 5, 10, and 20-day returns
## November was especially volatile for 45 and 60-day returns, indicating high price volatility in December and January
## The regression trees predicted abnormally high returns for 20, 45, and 60-day intervals at certain points in April 

## Now let's test a strategy using our insights from the analysis
## We will back test a strategy where we invest across the first 3 days of April 2022 buying $5,000 of Ethereum at each day's opening price and selling at the adj. closing 
## Remember that Bitcoin tends to be more ambiguous and volatile than Bitcoin. 
#Therefore, we should be even more active in our strategies to take advantage of short term flucuations

aprileth=getSymbols('ETH-USD', from='2022-04-01', to='2022-04-30', auto.assign=F)
aprileth=data.frame(aprileth)
setDT(aprileth)
names(aprileth)=c('open', 'high', 'low', 'close', 'volume', 'adjusted')
aprileth[, p1:=shift(adjusted, n=1, type='lead')]
aprileth=aprileth[1:3]
aprileth[, ret1:=(adjusted/open)-1]
aprilret=mean(aprileth$ret1)
aprilrets=as.vector(aprileth$ret1)
fin=sum(5000*(1+aprilrets))
ret=(fin/15000)-1
## we earned a 2.9% return using this strategy 

## To end this project, let's test a strategy entailing a diversified portfolio of Bitcoin and Ethereum 
## this will garner some insights for the benefits of diversification in crypto markets
## We will use the same strategies as last time in conjunction with each other 
## Let's use our February 2022 Bitcoin strategy and our April 2022 Ethereum strategy 
## we Will use a total of $57,500 for each strategy, since we invested a combined $115,000 across the two 
## for clarity, we are investing $100,000 into Bitcoin on February 1st, selling $50,000 on the 6th and the rest on the 11th(we will buy at the opening price on the 1st and sell at the closing price on the 6th and 11th)
#  $28,750 for each interval 
## The information for the Ethereum strategy is listed above 
## let's find our arithmetic and compound return:

#Diversified:

##Bitcoin:
sum=(28750*(1+fiveret))+(28750*(1+tenret))##$63,369.90

##Ethereum:
fin=sum(19166.67*(1+aprilrets))##$58,882.81
((sum+fin)/115000)-1
## We earned a 6.31% return for this strategy 

## Finally, let's see how these strategies would have done on their own 

# Bitcoin:

sum=(57500*(1+fiveret))+(57500*(1+tenret))##$126,739.80
(sum/115000)-1 #10.21%

# Ethereum:
fin=sum(38333.33*(1+aprilrets))##$58,882.81
(fin/115000)-1 #2.4%

## The effects of diversification seem the same as if we were trading equities 
## If we want a higher return, we have to take on more risk by trading only one token
# However, bearing this extra risk can also entail a lower return 

## ** Final Summary of Project Findings/Insights ** ##
## Some months historically have been more fruitful than others for trading both tokens 
## October was the best month for trading Bitcoin, April was the best month for trading Ethereum
## Bitcoin Prices were particularly volatile in January and December for daily, 5, and 10-day intervals
#  Prices were volatile in October and November for 20, 45, and 60-day Bitcoin returns
## Ethereum was most volatile in December for 5 and 10-day intervals
#  May was the most volatile month for daily price swings
#  July and November were notably volatile for 20, 45, and 60-day returns 
## Although price swings are difficult to gauge, any breakthroughs that make tokens more accessible, secure, efficient, or cheaper to trade should expand the market and drive the price up
## Successful trading strategies can be premised on the insights from this analysis as demonstrated
## The risk level of a given strategy can be adjusted based on time interval, the month, and asset composition
## Crypto markets are far more ambiguous than equity and debt markets, but this analysis provides some useful information for traders in the crypto markets

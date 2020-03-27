## from: https://towardsdatascience.com/visualize-the-pandemic-with-r-covid-19-c3443de3b4e4

# remotes::install_github("GuangchuangYu/nCov2019")
require(nCov2019)
require(dplyr)

x <- get_nCov2019()
y <- load_nCov2019()

#obtain top 10 country
d <- y['global'] #extract global data
l1 <- which(d$country != 'China')
d <- d[l1,] #exclude China
n <- d %>% filter(time == time(y)) %>%
 top_n(10, cum_confirm) %>%
 arrange(desc(cum_confirm))


#plot top 10

require(ggplot2)
require(ggrepel)

ggplot(filter(d, country %in% n$country, d$time > "2020-02-15"),
 aes(time, cum_confirm, color=country)) +
 geom_line() +
 geom_text_repel(aes(label=country),
 function(d) d[d$time == time(y),]) +
 theme_minimal(base_size=14) +
 theme(legend.position = "none")
 
 
x <- get_nCov2019()
plot(x) #plot global map


#visualize global growth over time
library(magick)
y <- load_nCov2019()
d <- c(paste0("2020-02-", 12:29), paste0("2020-03-0", 1:9), paste0("2020-03-1", 1:9), paste0("2020-03-2", 0:4))
img <- image_graph(1200, 700, res = 96)
out <- lapply(d, function(date){
 p <- plot(y, date=date,
 label=FALSE, continuous_scale=TRUE)
 print(p)
})
dev.off()
animation <- image_animate(img, fps = 2)
print(animation)


d <- y['global'] #extract global data
usdata <- d %>%
filter(d$country == "United States" & d$cum_confirm>100) %>%
select(time,cum_confirm)

ggplot(usdata,
 aes(time, cum_confirm)) +
 geom_line()


library(forecast)
case <- ts(usdata[,2], start=1,frequency = 1)
fit <- tslm(log(case) ~ trend)
fc <- forecast(fit, h=10)
plot(fc)


usdata$new_cases <- usdata$cum_confirm - lag(usdata$cum_confirm)
usdata$R <- usdata$new_cases / lag(usdata$new_cases)

plot(R~time, data=usdata, type='l')

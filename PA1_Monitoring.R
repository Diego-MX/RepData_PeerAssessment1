## ----packages, message=F-------------------------------------------------
library(knitr);  library(magrittr);  library(dplyr);
library(data.table);  library(lubridate);  library(ggplot2);

inline_hook <- function(x)
{ if(is.numeric(x))
  { digits_ <- getOption("digits")
    x %<>% signif(digits=digits_) %>%
      format(digits=digits_, format='fg')
  }
  paste(as.character(x), collapse=", ")
}

options(digits=3)
opts_knit$set(fig.path="./figures")
knit_hooks$set(inline=inline_hook)

## ----loading-------------------------------------------------------------
act_file <- "./activity.csv"
act_zip  <- "./activity.zip"
act_url  <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"

if(!file.exists(act_file))
{
  if(!file.exists(act_zip))
  { download.file(act_url, act_zip, method='curl')
  }
  unzip(act_zip)
}

stepsData <- read.csv(act_file) %>% as.data.table %>%
  mutate(date = ymd(date))

## ----stepsMean, fig.align='center', fig.width=6, fig.height=3------------
byDay <- group_by(stepsData, date) %>%
  summarise(steps = sum(steps, na.rm=T))

stepsHist <- ggplot(byDay, aes(steps)) +
  geom_histogram(binwidth=1500)
print(stepsHist)

## ----average, fig.align='center', fig.width=6, fig.height=4--------------
alongDay <- group_by(stepsData, interval) %>%
  summarise(steps = mean(steps, na.rm=T)) %>%
  mutate(hour = floor(interval/100),
       minute = interval %% 100,
      daytime = hm(paste(hour, minute)))

day_plot <- ggplot(alongDay, aes(interval, steps)) +
  geom_line()
print(day_plot)

## ------------------------------------------------------------------------
stepsFill <- ungroup(alongDay)$steps

byDayNA %>% stepsData %>%
  mutate(steps_ = ifelse(is.na(steps), stepsFill, steps)) %>%
  group_by(date) %>%
  summarise(stepsNA = sum(steps))

byDay %<>% join(byDayNA, date) %>%
  melt(id.vars = "date")

stepsHists <- ggplot(byDay, aes(steps, stepsNA))



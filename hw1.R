library(dplyr)
library(readr)
library(ggplot2)

# Reads in datafile, and creates column corresponding to
# year of project deadline
ks <- read_csv("./data/Kickstarter Projects.csv") %>%
  mutate(year=format(as.Date(deadline, format="%d-%m-%y"), "%Y"))

ks_states <- ks %>% 
  # We don't want to pass judgement about currently live projects
  filter(state != "live") %>%
  mutate(success = (state=="successful") ) %>%
  group_by(year) %>%
  summarise(n = mean(success))

ggplot(ks_states, aes(x=year, y=n, fill=year)) + geom_bar(stat="identity") +
  theme(legend.position = "none") +
  scale_y_continuous(labels= scales::percent) +
  labs(title="Successful Kickstarter Projects by Year", x="Year", 
       y="Percentage of Projects Reaching Goal")

ggplot(data=ks, aes(x=year, fill=state)) + geom_bar(position = "fill") +
  scale_y_continuous(labels= scales::percent)

ggplot(data=ks, aes(x=state, fill=year)) + geom_bar(position = "fill") +
  scale_y_continuous(labels= scales::percent)

ggplot(data=ks, aes(x=currency)) + geom_bar()

tbl <- data.frame(ks$year, ks$state)
# tbl <- filter(tbl, ks.state != "undefined")
tbl <- table(tbl)
mosaicplot(tbl, type="pearson", shade=TRUE)
 
# No relation between currency and success
tbl2 <- table(ks$state, as.character(ks$currency))
tbl2 <- data.frame(tbl2)
tbl2 <- filter(tbl2, Var2 %in% c("USD","AUD", "CAD", "EUR", "GBP")) %>%
  mutate(Var2 = factor(Var2))
mosaicplot(table(tbl2), shade=TRUE)

ks_backers <- ks %>%
  group_by(backers) %>%
  summarise(count = n())

# Here, we see that the number of backers follows a power law relationship
ggplot(ks_backers, aes(x=backers, y=count)) + geom_line() +
  scale_x_continuous(trans="log2") +
  scale_y_continuous(trans="log2")

ks_currency <- ks %>%
  mutate(pledged_usd = round(pledged_usd/10)*10) %>%
  group_by(pledged_usd) %>%
  summarise(count = n())

ggplot(ks_currency, aes(x=pledged_usd, y=count)) + geom_line() +
  scale_x_continuous(trans="log2") +
  scale_y_continuous(trans="log2") + 
  labs(x="Pledged USD (rounded to nearest $10)")

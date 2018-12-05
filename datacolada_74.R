# Analysis of weight study data reported in Datacolada 74 http://datacolada.org/74
#
# Disclaimer: This is just a demonstration & test, I make no claims about who is wrong or right.
#
# Fred Hasselman


library(plyr)
library(tidyverse)
require(rio)
library(benford.analysis)

data.study1 <- rio::import("http://datacolada.org/appendix/74/Study%201%20-%20Decoy%20Effect.csv")
data.study2 <- rio::import("http://datacolada.org/appendix/74/Study%202%20-%20Decoy%20Effect.csv")
data.study3 <- rio::import("http://datacolada.org/appendix/74/Study%203%20-%20Decoy%20Effect.csv")



# Study 1 ----

data.study1.long <- data.study1 %>%
  select(starts_with("Day")) %>%
  gather(key = Day, value = weight) %>%
  mutate(daynr = as.numeric(gsub("Day|\\s\\(Beginning of intervention\\)","",Day)),
         center = weight - round(mean(weight,na.rm = TRUE),2))

data.study1.long$last.ori <- laply(data.study2.long$weight,function(n)  as.numeric(substring(paste(n),nchar(paste(n)))))
data.study1.long$last <- laply(data.study2.long$center,function(n)  as.numeric(substring(paste(n),nchar(paste(n)))))

# 1st digit looks normally distributed
(cv1 <- benford(data.study1.long$weight, sign = "positive", number.of.digits = 1, discrete = FALSE))
plot(cv1)

# Last digit looks uniform-ish but not quite right 
(cv1lo <- benford(data.study1.long$last.ori, sign = "positive", number.of.digits = 1, discrete = TRUE))
plot(cv2lo)



# Positive centered values
(cv1p <- benford(data.study1.long$center, sign = "positive", number.of.digits = 1, discrete = FALSE))
plot(cv1p)

# Negative centered values
(cv1n <- benford(data.study1.long$center, sign = "negative", number.of.digits = 1, discrete = FALSE))
plot(cv1n)

# Last digit of centered values
(cv1l <- benford(data.study1.long$last, sign = "positive", number.of.digits = 1, discrete = TRUE))
plot(cv1l)

# Dataset for ggplot
dfcv1 <- ldply(list(positive.1st=cv1p$bfd,negative.1st=cv1n$bfd,last=cv1l$bfd))
dfcv1$pUnif <- c(rep((1/9)*NROW(cv1p$data),NROW(cv1p$bfd)), rep((1/9)*NROW(cv1n$data),NROW(cv1n$bfd)), rep((1/9)*NROW(cv1l$data),NROW(cv1l$bfd)))

ggplot(dfcv1, aes(x=digits, y=data.dist.freq)) +
  geom_col(fill="steelblue", colour="black") +
  geom_label(aes(label = paste0(format(data.dist*100, digits=2,scientific=FALSE),"%"), y=4), size=3, colour="steelblue") +
  facet_grid(.id ~., scales = "free_y") +
  geom_hline(aes(yintercept = pUnif), colour = "grey60", size=1) +
  geom_point(aes(y= pUnif, x=digits), colour = "grey60", size=2) +
  geom_line(aes(y=benford.dist.freq, x=digits), colour = "red3", size=1) +
  geom_label(aes(label = paste0(format(benford.dist*100, digits=2,scientific=FALSE),"%"), y=benford.dist.freq), colour="red3", size=3) +
  ggtitle(label = "Benford's Law (red) | Uniform (grey) | Observed (blue)",
          subtitle = paste0("Centered weight values in Study 1 (N=",NROW(cv1l$data),")")) +
  scale_x_continuous("Digits of centered weight",breaks = 1:9) +
  scale_y_continuous("Count") +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank())



# Study 2 ----

data.study2.long <- data.study2 %>%
  select(starts_with("Day")) %>%
  gather(key = Day, value = weight) %>%
  mutate(daynr = as.numeric(gsub("Day|\\s\\(Beginning of intervention\\)","",Day)),
         center = weight - round(mean(weight,na.rm = TRUE),2))

data.study2.long$last.ori <- laply(data.study2.long$weight,function(n)  as.numeric(substring(paste(n),nchar(paste(n)))))
data.study2.long$last <- laply(data.study2.long$center,function(n)  as.numeric(substring(paste(n),nchar(paste(n)))))

# 1st digit looks normally distributed
(cv2 <- benford(data.study2.long$weight, sign = "positive", number.of.digits = 1, discrete = FALSE))
plot(cv2)

# Last digit looks uniform-ish but not quite right 
(cv2lo <- benford(data.study2.long$last.ori, sign = "positive", number.of.digits = 1, discrete = TRUE))
plot(cv2lo)



# Positive centered values
(cv2p <- benford(data.study2.long$center, sign = "positive", number.of.digits = 1, discrete = FALSE))
plot(cv2p)

# Negative centered values
(cv2n <- benford(data.study2.long$center, sign = "negative", number.of.digits = 1, discrete = FALSE))
plot(cv2n)

# Last digit of centered values
(cv2l <- benford(data.study2.long$last, sign = "positive", number.of.digits = 1, discrete = TRUE))
plot(cv2l)

# Dataset for ggplot
dfcv2 <- ldply(list(positive.1st=cv2p$bfd,negative.1st=cv2n$bfd,last=cv2l$bfd))
dfcv2$pUnif <- c(rep((1/9)*NROW(cv2p$data),NROW(cv2p$bfd)), rep((1/9)*NROW(cv2n$data),NROW(cv2n$bfd)), rep((1/9)*NROW(cv2l$data),NROW(cv2l$bfd)))

ggplot(dfcv2, aes(x=digits, y=data.dist.freq)) +
  geom_col(fill="steelblue", colour="black") +
  geom_label(aes(label = paste0(format(data.dist*100, digits=2,scientific=FALSE),"%"), y=4), size=3, colour="steelblue") +
  facet_grid(.id ~., scales = "free_y") +
  geom_hline(aes(yintercept = pUnif), colour = "grey60", size=1) +
  geom_point(aes(y= pUnif, x=digits), colour = "grey60", size=2) +
  geom_line(aes(y=benford.dist.freq, x=digits), colour = "red3", size=1) +
  geom_label(aes(label = paste0(format(benford.dist*100, digits=2,scientific=FALSE),"%"), y=benford.dist.freq), colour="red3", size=3) +
  ggtitle(label = "Benford's Law (red) | Uniform (grey) | Observed (blue)",
          subtitle = paste0("Centered weight values in Study 2 (N=",NROW(cv2l$data),")")) +
  scale_x_continuous("Digits of centered weight",breaks = 1:9) +
  scale_y_continuous("Count") +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank())



# Study 3 ----

data.study3.long <- data.study3 %>%
  select(starts_with("Day")) %>%
  gather(key = Day, value = weight) %>%
  mutate(daynr = as.numeric(gsub("Day|\\s\\(Beginning of intervention\\)","",Day)),
         center = weight - round(mean(weight,na.rm = TRUE),2))

data.study3.long$last.ori <- laply(data.study3.long$weight,function(n)  as.numeric(substring(paste(n),nchar(paste(n)))))
data.study3.long$last <- laply(data.study3.long$center,function(n)  as.numeric(substring(paste(n),nchar(paste(n)))))

# 1st digit looks normally distributed
(cv3 <- benford(data.study3.long$weight, sign = "positive", number.of.digits = 1, discrete = FALSE))
plot(cv3)

# Last digit looks uniform-ish but not quite right 
(cv3lo <- benford(data.study3.long$last.ori, sign = "positive", number.of.digits = 1, discrete = TRUE))
plot(cv3lo)



# Positive centered values
(cv3p <- benford(data.study3.long$center, sign = "positive", number.of.digits = 1, discrete = FALSE))
plot(cv3p)

# Negative centered values
(cv3n <- benford(data.study3.long$center, sign = "negative", number.of.digits = 1, discrete = FALSE))
plot(cv3n)

# Last digit of centered values
(cv3l <- benford(data.study3.long$last, sign = "positive", number.of.digits = 1, discrete = TRUE))
plot(cv3l)

# Dataset for ggplot
dfcv3 <- ldply(list(positive.1st=cv3p$bfd,negative.1st=cv3n$bfd,last=cv3l$bfd))
dfcv3$pUnif <- c(rep((1/9)*NROW(cv3p$data),NROW(cv3p$bfd)), rep((1/9)*NROW(cv3n$data),NROW(cv3n$bfd)), rep((1/9)*NROW(cv3l$data),NROW(cv3l$bfd)))

ggplot(dfcv3, aes(x=digits, y=data.dist.freq)) +
  geom_col(fill="steelblue", colour="black") +
  geom_label(aes(label = paste0(format(data.dist*100, digits=2,scientific=FALSE),"%"), y=4), size=3, colour="steelblue") +
  facet_grid(.id ~., scales = "free_y") +
  geom_hline(aes(yintercept = pUnif), colour = "grey60", size=1) +
  geom_point(aes(y= pUnif, x=digits), colour = "grey60", size=2) +
  geom_line(aes(y=benford.dist.freq, x=digits), colour = "red3", size=1) +
  geom_label(aes(label = paste0(format(benford.dist*100, digits=2,scientific=FALSE),"%"), y=benford.dist.freq), colour="red3", size=3) +
  ggtitle(label = "Benford's Law (red) | Uniform (grey) | Observed (blue)",
          subtitle = paste0("Centered weight values in Study 3 (N=",NROW(cv3l$data),")")) +
  scale_x_continuous("Digits of centered weight",breaks = 1:9) +
  scale_y_continuous("Count") +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank())

# All together ----

df.all <- ldply(list(study1=data.study1.long,study2=data.study2.long, study3=data.study3.long), .id = "Study")

# 1st digit looks normally distributed
(cvA <- benford(df.all$weight, sign = "positive", number.of.digits = 1, discrete = FALSE))
plot(cvA)

# Last digit looks uniform-ish but not quite right 
(cvAlo <- benford(df.all$last.ori, sign = "positive", number.of.digits = 1, discrete = TRUE))
plot(cvAlo)


# Positive centered values
(cvAp <- benford(df.all$center, sign = "positive", number.of.digits = 1, discrete = FALSE))
plot(cvAp)

# Negative centered values
(cvAn <- benford(df.all$center, sign = "negative", number.of.digits = 1, discrete = FALSE))
plot(cvAn)

# Last digit of centered values
(cvAl <- benford(df.all$last, sign = "positive", number.of.digits = 1, discrete = TRUE))
plot(cvAl)


# Dataset for ggplot
dfcvA <- ldply(list(positive.1st=cvAp$bfd,negative.1st=cvAn$bfd,last=cvAl$bfd))
dfcvA$pUnif <- c(rep((1/9)*NROW(cvAp$data),NROW(cvAp$bfd)), rep((1/9)*NROW(cvAn$data),NROW(cvAn$bfd)), rep((1/9)*NROW(cvAl$data),NROW(cvAl$bfd)))

ggplot(dfcvA, aes(x=digits, y=data.dist.freq)) +
  geom_col(fill="steelblue", colour="black") +
  geom_label(aes(label = paste0(format(data.dist*100, digits=2,scientific=FALSE),"%"), y=4), size=3, colour="steelblue") +
  facet_grid(.id ~., scales = "free_y") +
  geom_hline(aes(yintercept = pUnif), colour = "grey60", size=1) +
  geom_point(aes(y= pUnif, x=digits), colour = "grey60", size=2) +
  geom_line(aes(y=benford.dist.freq, x=digits), colour = "red3", size=1) +
  geom_label(aes(label = paste0(format(benford.dist*100, digits=2,scientific=FALSE),"%"), y=benford.dist.freq), colour="red3", size=3) +
  ggtitle(label = "Benford's Law (red) | Uniform (grey) | Observed (blue)",
          subtitle = paste0("Centered weight values in Study 1 & 2 & 3 (N=",NROW(cvAl$data),")")) +
  scale_x_continuous("Digits of centered weight",breaks = 1:9) +
  scale_y_continuous("Count") +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank())


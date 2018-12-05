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

# 1st digit looks normal
(cv2 <- benford(data.study2.long$weight, sign = "positive", number.of.digits = 1, discrete = FALSE))
plot(cv2)


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




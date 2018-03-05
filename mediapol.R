# Before running the code make sure you have all libraries listed below installed to your machine

library(ggplot2)
library(haven)
library(extrafont)
library(survey)
library(dplyr)
library(stringr)
library(purrr)
library(highcharter)
library(htmlwidgets)
library(htmltools)
library(plotly)

# setwd() to your directory

# Define chart theme (custom font faces, etc.)
theme_plot <- theme(
  axis.text.y = element_text(colour="black", size = 16, family = "Gill Sans MT"),
  axis.text.x = element_text(colour="black", size = 16, family="Gill Sans MT"),
  axis.title.x = element_text(size=16, family = "Futura Hv BT"),
  axis.title.y = element_text(size=16, family = "Futura Hv BT"),
  strip.text  = element_text(size=16, family = "Futura Hv BT"),
  panel.border = element_rect(fill=NA, linetype = "solid", colour = "black"),
  panel.background = element_rect(fill = NA),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  plot.title = element_text(hjust = 0.5, colour = "Black", size=18, family = "Futura Hv BT"),
  plot.subtitle = element_text(hjust = 0.5, colour = "Black", size=14, family = "Futura Hv BT"),
  legend.title = element_text(size=12, family = "Futura Hv BT"),
  legend.text = element_text(size=14, family = "Gill Sans MT"),
  plot.caption = element_text(size=12, family = "Gill Sans MT"),
  panel.grid.major.y = element_line(colour = "grey80")
)

# Download public NDI data from Caucasusbarometer.org

ndipublic <- download.file("http://caucasusbarometer.org/downloads/NDI_2017_Dec_25_12_17_public.dta",
                           "NDI_2017_Dec_25_12_17_public.dta", mode = "wb")


# Read data

ndidec <- read_dta("NDI_2017_Dec_25_12_17_public.dta")

# Create variables

ndidec$rutv <- 0
ndidec$rutv[ndidec$TRURUTO==1] <- 1
ndidec$rutv[ndidec$TRURCH1==1] <- 1
ndidec$rutv[ndidec$TRURTR==1] <- 1
ndidec$rutv[ndidec$TRUREN==1] <- 1
ndidec$rutv[ndidec$TRURU1==1] <- 1
ndidec$rutv[ndidec$TRURU24==1] <- 1
ndidec$rutv[ndidec$TRUTVCEN==1] <- 1

ndidec$imedi <- 0
ndidec$imedi[ndidec$TRUIMEDI==1] <- 1
ndidec$r2 <- 0
ndidec$r2[ndidec$TRUR2==1] <- 1

ndidec$stratum <- ndidec$STRATUM
ndidec$stratum[ndidec$SUBSTRATUM==51] <- 3
ndidec$stratum[ndidec$SUBSTRATUM==52] <- 4
table(ndidec$stratum)

ndidec$gd <- 0
ndidec$gd[ndidec$PARTYSUPP1==8] <- 1

ndidec$unm <- 0
ndidec$unm[ndidec$PARTYSUPP1==6] <- 1

ndidec$none <- 0
ndidec$none[ndidec$PARTYSUPP1==-2 | ndidec$PARTYSUPP1==-1 | ndidec$PARTYSUPP1==26] <- 1


ndidec$eu <- 0
ndidec$eu[ndidec$EUVSEURU==3 | ndidec$EUVSEURU==4] <- 1

ndidec$tv <- 0
ndidec$tv[ndidec$INFSOUF1==7] <- 1

ndidec$edu <- 0
ndidec$edu[ndidec$RESPEDU<1] <- NA
ndidec$edu[ndidec$RESPEDU>0 & ndidec$RESPEDU<4] <- 1
ndidec$edu[ndidec$RESPEDU==4] <- 2
ndidec$edu[ndidec$RESPEDU>4] <- 3

ndidec$ethnocode <- ndidec$ETHNOCODE

ndidec$age <- ndidec$RESPAGE

table(ndidec$edu)

# Run models


mod1 <- glm(imedi~gd+unm+none+r2+factor(ethnocode)+factor(stratum),
            data=ndidec, family="binomial")
summary(mod1)

mod2 <- glm(r2~gd+unm+imedi+none+factor(ethnocode)+factor(stratum),
            data=ndidec, family="binomial")
summary(mod2)

mod3 <- glm(imedi~gd+unm+r2+factor(ethnocode)+factor(stratum)+factor(edu),
            data=ndidec, family="binomial")
summary(mod3)

mod4 <- glm(r2~gd+unm+imedi+factor(ethnocode)+factor(stratum)+factor(edu),
            data=ndidec, family="binomial")
summary(mod4)

### Compare AIC. Although the introduction of education improves the quality of model, the improvement is slight. So for the sake of simplicity I leave it out
cbind(mod1$aic, mod3$aic)
cbind(mod2$aic, mod4$aic)


### Stick to M1 and M3. Calculate predicted probabilities

imd1 <- data.frame(gd = rep(c(0, 1), 2),
                  unm = mean(ndidec$unm),
                  none = mean(ndidec$none),
                  ethnocode=1,
                  r2=mean(ndidec$r2),
                  age=mean(ndidec$age),
                  stratum=rep(c(1, 2, 3, 4), each=4))

imd1.write <- cbind(imd1, predict(mod1, newdata = 
                                    imd1, type = "response", 
                                  se.fit = TRUE))
imd1.write$fitlab <- round(imd1.write$fit, 2)
imd1.write$group <- "gd"
imd1.write$model <- "Imedi"

imd2 <- data.frame(unm = rep(c(0, 1), 2),
                   gd = mean(ndidec$gd),
                   none = mean(ndidec$none),
                   ethnocode=1,
                   r2=mean(ndidec$r2),
                   age=mean(ndidec$age),
                   stratum=rep(c(1, 2, 3, 4), each=4))

imd2.write <- cbind(imd2, predict(mod1, newdata = 
                                   imd2, type = "response", 
                                 se.fit = TRUE))
imd2.write$fitlab <- round(imd2.write$fit, 2)
imd2.write$group <- "unm"
imd2.write$model <- "Imedi"


imd3 <- data.frame(none = rep(c(0, 1), 2),
                   gd = mean(ndidec$gd),
                   unm = mean(ndidec$unm),
                   ethnocode=1,
                   r2=mean(ndidec$r2),
                   age=mean(ndidec$age),
                   stratum=rep(c(1, 2, 3, 4), each=4))

imd3.write <- cbind(imd3, predict(mod1, newdata = 
                                   imd3, type = "response", 
                                 se.fit = TRUE))
imd3.write$fitlab <- round(imd3.write$fit, 2)
imd3.write$group <- "none"
imd3.write$model <- "Imedi"



imd <- rbind(imd1.write, imd2.write, imd3.write)

r21 <- data.frame(gd = rep(c(0, 1), 2),
                  unm = mean(ndidec$unm),
				  none = mean(ndidec$none),
                  ethnocode=1,
                  imedi=mean(ndidec$imedi),
                  age=mean(ndidec$age),
                  stratum=rep(c(1, 2, 3, 4), each=4))

r21.write <- cbind(r21, predict(mod2, newdata = 
                                    r21, type = "response", 
                                  se.fit = TRUE))
r21.write$fitlab <- round(r21.write$fit, 2)
r21.write$group <- "gd"
r21.write$model <- "Rustavi 2"

r22 <- data.frame(unm = rep(c(0, 1), 2),
                   gd = mean(ndidec$gd),
				   none = mean(ndidec$none),
                   ethnocode=1,
                   imedi=mean(ndidec$imedi),
                   age=mean(ndidec$age),
                   stratum=rep(c(1, 2, 3, 4), each=4))

r22.write <- cbind(r22, predict(mod2, newdata = 
                                   r22, type = "response", 
                                 se.fit = TRUE))
r22.write$fitlab <- round(r22.write$fit, 2)
r22.write$group <- "unm"
r22.write$model <- "Rustavi 2"

r23 <- data.frame(none = rep(c(0, 1), 2),
                   gd = mean(ndidec$gd),
                   unm = mean(ndidec$unm),
                   ethnocode=1,
                    imedi=mean(ndidec$r2),
                   age=mean(ndidec$age),
                   stratum=rep(c(1, 2, 3, 4), each=4))

r23.write <- cbind(r23, predict(mod2, newdata = 
                                   r23, type = "response", 
                                 se.fit = TRUE))
r23.write$fitlab <- round(r23.write$fit, 2)
r23.write$group <- "none"
r23.write$model <- "Rustavi 2"

r2 <- rbind(r21.write, r22.write, r23.write)

imd <- subset(imd, select=c("none", "gd", "unm", "fit", "se.fit",  "stratum", "group", "model"))
r2 <- subset(r2, select=c("none","gd", "unm", "fit", "se.fit", "stratum", "group", "model"))

write <- rbind(imd, r2)
write$fitlab <- round(write$fit, 2)

write$group[write$gd==1] <- "GD"
# write$group[write$gd==0] <- "Non-GD supporters"
write$group[write$unm==1] <- "UNM"
# write$group[write$unm==0] <- "Non-UNM supporters"
write$group[write$none==1] <- "No party"
# write$group[write$none==0] <- "Affiliated"

write$group <- factor(write$group, levels=c("GD",
                                            "Non-GD supporters",
                                            "UNM",
                                            "Non-UNM supporters",
                                            "No party",
                                            "Affiliated"))

write$stratum <- factor(write$stratum, levels=c(1, 2, 3, 4),
                                      labels=c("Tbilisi",
                                            "Large urban",
                                            "Other urban",
                                            "Rural"))
write <- write %>%
    subset(., group %in% c("GD", "UNM", "No party"))

write <- unique(write)

# Make static chart

gpl <- ggplot(write, aes(factor(model), fit, group=group, color=group))+
  geom_point(stat="identity", aes(group=group, shape=group),
             position=position_dodge(width=0.9),
             size=3)+
  geom_errorbar(aes(ymax = fit+1.96*se.fit,
                    ymin = fit-1.96*se.fit),
                width=0.05, size=0.2,
                position=position_dodge(width=0.9), show.legend = FALSE)+
  scale_colour_manual(values=c("#377eb8", "#e41a1c", "#4daf4a"))+
  scale_shape_manual(values=c(15, 17, 19))+
  labs(title="Which TV channels do you trust the most for accurate information\non politics and current affairs?",
		subtitle="By Which party is closest to you And Settlement type",
		caption="Predicted probabilities with 95% confidence intervals",
       x="TV channels",
       y="Probability")+
  scale_y_continuous(labels=function(x)x*100, limits=c(0, 1))+
  facet_wrap(~stratum)+
  theme_plot+
  theme(
    strip.background = element_blank(),
    legend.position="top",
    legend.title = element_blank()
  )

ggsave("tv_models.png", gpl, height=7, width=13.5, dpi=300, device="png")

# Make interactive chart

plgpl <- ggplotly(gpl) %>%
		layout(legend = list(x = 0.35, y = 100, orientation = 'h'))

plgpl$x$data[[13]]$showlegend <- FALSE
plgpl$x$data[[14]]$showlegend <- FALSE
plgpl$x$data[[15]]$showlegend <- FALSE
plgpl$x$data[[16]]$showlegend <- FALSE
plgpl$x$data[[17]]$showlegend <- FALSE
plgpl$x$data[[18]]$showlegend <- FALSE
plgpl$x$data[[19]]$showlegend <- FALSE
plgpl$x$data[[20]]$showlegend <- FALSE
plgpl$x$data[[21]]$showlegend <- FALSE
plgpl$x$data[[22]]$showlegend <- FALSE
plgpl$x$data[[23]]$showlegend <- FALSE
plgpl$x$data[[24]]$showlegend <- FALSE


saveWidget(plgpl, "index.html")

library(dplyr)
library(ggplot2); theme_set(theme_bw())
library(tikzDevice)
library(egg)
source("rRfun.R")

ww <- c(1, 6, 11, 16)

rRdata <- list(
  data.frame(
    r=seq(0, 0.2, by=0.01)[ww+4],
    R=sapply(seq(0, 0.2, by=0.01)[ww+4], function(x) serialR(x, rho=0.75)$R),
    type="Forward serial interval ($\\rho=0.75$)"
  ),
  data.frame(
    r=seq(0, 0.2, by=0.01)[ww+3],
    R=sapply(seq(0, 0.2, by=0.01)[ww+3], function(x) serialR(x, rho=0.5)$R),
    type="Forward serial interval ($\\rho=0.5$)"
  ),
  data.frame(
    r=seq(0, 0.2, by=0.01)[ww+2],
    R=sapply(seq(0, 0.2, by=0.01)[ww+2], function(x) serialR(x, rho=0.25)$R),
    type="Forward serial interval ($\\rho=0.25$)"
  ),
  data.frame(
    r=seq(0, 0.2, by=0.01)[ww+1],
    R=sapply(seq(0, 0.2, by=0.01)[ww+1], function(x) serialR(x, rho=0)$R),
    type="Forward serial interval ($\\rho=0$)"
  ),
  data.frame(
    r=seq(0, 0.2, by=0.01)[ww],
    R=sapply(seq(0, 0.2, by=0.01)[ww], function(x) generationR(x)$R),
    type="Intrinsic generation interval"
  )
) %>%
  bind_rows %>%
  mutate(
    type=factor(type, level=c("Intrinsic generation interval", "Forward serial interval ($\\rho=0.75$)",
                              "Forward serial interval ($\\rho=0.5$)",
                              "Forward serial interval ($\\rho=0.25$)", 
                              "Forward serial interval ($\\rho=0$)"))
  )

## viridis/C colours; manually extracted (see commented code below)
vcpal <- c("#7E03A8FF", "#CC4678FF", "#F89441FF", "#F0F921FF", "#0D0887FF")
## reduce luminanance of too-light yellow
vcpal[4] <- adjustcolor(vcpal[4], red.f = 0.9, green.f = 0.9, blue.f = 0.9)
vcpal <- vcpal[c(5,1:4)] ## shuffle

g1 <- ggplot(rRdata) +
  geom_line(aes(r, R, col=type, lty=type)) +
  geom_point(aes(r, R, col=type, shape=type), size=2) +
  scale_x_continuous("Exponential growth rate $r$ (1/day)", limits=c(0, 0.2), expand=c(0, 0)) +
  scale_y_continuous("Basic reproduction number $\\mathcal R_0$", limits=c(1, 5.5), expand=c(0, 0)) +
  scale_colour_manual(values=vcpal) +
  ggtitle("A") +
  theme(
    panel.grid = element_blank(),
    legend.position = c(0.33, 0.75),
    legend.title = element_blank(),
    legend.background = element_blank()
  )

rGdata <- list(
  data.frame(
    r=seq(0, 0.2, by=0.01),
    mean=sapply(seq(0, 0.2, by=0.01), function(x) serialR(x, rho=0.75)$mean),
    type="Serial interval ($\\rho=0.75$)"
  ),
  data.frame(
    r=seq(0, 0.2, by=0.01),
    mean=sapply(seq(0, 0.2, by=0.01), function(x) serialR(x, rho=0.5)$mean),
    type="Serial interval ($\\rho=0.5$)"
  ),
  data.frame(
    r=seq(0, 0.2, by=0.01),
    mean=sapply(seq(0, 0.2, by=0.01), function(x) serialR(x, rho=0.25)$mean),
    type="Serial interval ($\\rho=0.25$)"
  ),
  data.frame(
    r=seq(0, 0.2, by=0.01),
    mean=sapply(seq(0, 0.2, by=0.01), function(x) serialR(x, rho=0)$mean),
    type="Serial interval ($\\rho=0$)"
  ),
  data.frame(
    r=seq(0, 0.2, by=0.01),
    mean=sapply(seq(0, 0.2, by=0.01), function(x) generationR(x)$mean),
    type="Generation interval"
  )
) %>%
  bind_rows %>%
  mutate(
    type=factor(type, level=c("Generation interval", "Serial interval ($\\rho=0.75$)",
                              "Serial interval ($\\rho=0.5$)", 
                              "Serial interval ($\\rho=0.25$)", 
                              "Serial interval ($\\rho=0$)"))
  )

g2 <- ggplot(rGdata) +
  geom_smooth(aes(r, mean, col=type, lty=type), se=FALSE) +
  geom_point(aes(r, mean, col=type, shape=type), size=2) +
  scale_x_continuous("Exponential growth rate $r$ (1/day)", limits=c(0, 0.2), expand=c(0, 0)) +
  scale_y_continuous("Mean interval (days)", expand=c(0, 0), limits=c(7.9, 10.5),
                     breaks=c(7:11)) +
  scale_colour_manual(values=vcpal) +
  ggtitle("B") +
  theme(
    panel.grid = element_blank(),
    legend.position = "none",
    legend.title = element_blank()
  )

rkappadata <- list(
  data.frame(
    r=seq(0, 0.2, by=0.01),
    mean=sapply(seq(0, 0.2, by=0.01), function(x) serialR(x, rho=0.75)$kappa),
    type="Serial interval ($\\rho=0.75$)"
  ),
  data.frame(
    r=seq(0, 0.2, by=0.01),
    mean=sapply(seq(0, 0.2, by=0.01), function(x) serialR(x, rho=0.5)$kappa),
    type="Serial interval ($\\rho=0.5$)"
  ),
  data.frame(
    r=seq(0, 0.2, by=0.01),
    mean=sapply(seq(0, 0.2, by=0.01), function(x) serialR(x, rho=0.25)$kappa),
    type="Serial interval ($\\rho=0.25$)"
  ),
  data.frame(
    r=seq(0, 0.2, by=0.01),
    mean=sapply(seq(0, 0.2, by=0.01), function(x) serialR(x, rho=0)$kappa),
    type="Serial interval ($\\rho=0$)"
  ),
  data.frame(
    r=seq(0, 0.2, by=0.01),
    mean=sapply(seq(0, 0.2, by=0.01), function(x) generationR(x)$kappa),
    type="Generation interval"
  )
) %>%
  bind_rows %>%
  mutate(
    type=factor(type, level=c("Generation interval", "Serial interval ($\\rho=0.75$)",
                              "Serial interval ($\\rho=0.5$)", 
                              "Serial interval ($\\rho=0.25$)", 
                              "Serial interval ($\\rho=0$)"))
  )

g3 <- ggplot(rkappadata) +
  geom_smooth(aes(r, mean, col=type, lty=type), se=FALSE) +
  geom_point(aes(r, mean, col=type, shape=type), size=2) +
  scale_x_continuous("Exponential growth rate $r$ (1/day)", limits=c(0, 0.2), expand=c(0, 0)) +
  scale_y_continuous("Squared coefficient of variation", expand=c(0, 0), limits=c(0, 0.84)) +
  scale_colour_manual(values=vcpal) +
  ggtitle("C") +
  theme(
    panel.grid = element_blank(),
    legend.position = "none",
    legend.title = element_blank()
  )

tikz(file = "rR.tex", width = 12, height = 4, standAlone = T)
ggarrange(g1, g2, g3, nrow=1)
dev.off()
tools::texi2dvi('rR.tex', pdf = T, clean = T)

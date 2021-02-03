## Ecosystem pink figure for Greg R synthesis paper

## housekeeping ---------------------------------
source("functions.r")
library(rstan)
library(plyr)
library(chroma) 
library(codatools)
library(ggpubr)

load_rdata(path = "./data/output/", verbose = TRUE)

## Data
sock <- read.csv("./data/master_brood_table_covar.csv",
                 stringsAsFactors = FALSE)
sock$Stock <- factor(sock$Stock, levels = unique(sock$Stock))

sock.info <- read.csv("./data/master_stock_info.csv",
                      stringsAsFactors = FALSE)
sock.info$Stock <- factor(sock.info$Stock, levels = unique(sock.info$Stock))

bienniel <- read.csv("./data/odd_even.csv",
                 stringsAsFactors = FALSE)

## Set bayesplot theme
bayesplot::bayesplot_theme_set(new = theme_sleek())

## Define colors
col.stock  <- rev(chroma::qpal(7, alpha = 0.4)[c(1, 4, 6)])
col.region <- rev(chroma::qpal(7, luminance = 40)[c(1, 4, 6)])
col.lt <- rev(chroma::qpal(7)[c(1, 4, 6)])
col.dk <- rev(chroma::qpal(7, luminance = 30)[c(1, 4, 6)])

## ------------------------------------------------


## Fig: Posterior percent change density; main model (post '77 BYs) -------------------
lst <- hb07_density_df(hb07r2)
s.df <- lst$stock
s.df <- subset(s.df, var != "SST x Comp")
m.df <- lst$region
m.df <- subset(m.df, var != "SST x Comp")

## Covariate labels
vars <- data.frame(var = levels(m.df$var))
vars$lab <- paste0("(", letters[1:nrow(vars)], ") ", vars$var)
vars <- vars[-3,]
vars$lab <- c("SST", "Pink","SST + Pink + SST x Pink")

a <- ggplot(m.df) +
    geom_vline(xintercept = 0, color = "grey50", linetype = 2, size = 0.25) +
    geom_path(data = s.df[s.df$region == "West Coast", ],
              aes(x = x, y = y, group = stock), color = col.stock[1],
              na.rm = TRUE) +
    geom_path(data = s.df[s.df$region == "Gulf of Alaska", ],
              aes(x = x, y = y, group = stock), color = col.stock[2],
              na.rm = TRUE) +
    geom_path(data = s.df[s.df$region == "Bering Sea", ],
              aes(x = x, y = y, group = stock), color = col.stock[3],
              na.rm = TRUE) +
    geom_path(aes(x = x, y = y, color = region), size = 1,
              na.rm = TRUE) +
    scale_color_manual(values = rev(col.region)) +
    labs(x = "Percent change in sockeye returns per spawner (R/S)",
         y = "",
         color = "") +
    scale_x_continuous(limits = c(-50, 52), expand = c(0, 0)) +
    scale_y_continuous(breaks=NULL) +
    geom_text(data = vars,
              aes(x = -48.1,
                  y = max(m.df$y) - 0.008,
                  label = lab),
              hjust = 0,
              size = 2.7,
              color = "grey30") +
    facet_wrap( ~ var, ncol = 1) +
    theme_sleek(base_size = 9) +
    theme(legend.justification = c(0, 0),
          legend.position = c(0.7, 0.9),
          legend.key.size = unit(9, "pt"),
          legend.background = element_blank(),
          legend.text = element_text(size = 7),
          panel.spacing.y = unit(-0.5, "pt"),
          strip.background = element_blank(),
          strip.text.x = element_blank(),
          plot.margin = unit(c(0.23,0,1.9,0.4), "lines")) 


## Fig: biennial patterns

bienniel$metric_f = factor(bienniel$metric, levels=c('survival','age','length'))

b <- ggplot(bienniel, aes(x = stock, y = value, fill = even_odd)) +
  geom_bar(position="dodge", stat="identity") +
  facet_wrap( ~ metric_f, ncol = 1, scales = "free_y",
              strip.position = "right", 
              labeller = as_labeller(c(survival = "Survival (ln [R/S])", age = "Ocean age 3 (z)", length = "Length (z)" ))) +
  scale_fill_manual(values = c("light grey", "black"), labels = c("Even-year broods", "Odd-year broods")) +
  geom_hline(yintercept = 0, color = "grey50", linetype = 2, size = 0.25) +
  labs(y = "", x = "Population") +
  theme_sleek(base_size = 9) +
  scale_y_continuous(position = "right") +
  theme(strip.placement = "outside",
        legend.justification = c(0, 0),
        legend.position = c(0.01, 0.9),
        legend.key.size = unit(9, "pt"),
        legend.background = element_blank(),
        legend.text = element_text(size = 7),
        panel.spacing.x = unit(-0.5, "pt"),
        axis.text.x = element_text(angle = 45, 
                                   vjust = 1,
                                   hjust = 1,
                                   size=7),
        legend.title = element_blank(),
        plot.margin = unit(c(0.23,0,0,1.1), "lines"),
        panel.spacing.y = unit(-0.5, "pt")) 

g <- ggarrange(a, b, 
          labels = c("A", "B"),
          ncol = 2,
          widths = c(0.9,1))

jpeg("pink-sockeye-multi-panel.v2.jpeg", width = 8, height = 5.5, units = "in", res = 400)
print(g)
dev.off()


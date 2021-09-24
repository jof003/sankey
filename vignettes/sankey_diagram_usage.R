## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(ggplot2)
library(ggalluvial)
library(reshape2)
library(readxl)

df <- read_excel("../inst/extdata/Stacked_column_stroke_blacks_noAF.xlsx", sheet = "Data", range = "A1:F6")
print.data.frame(df)

## ----transform----------------------------------------------------------------
df_v2 <-melt(df, id = "Risk Factors for Stroke in Blacks")
names(df_v2) <- c("risk_factor", "year", "value")

## ----border-------------------------------------------------------------------
borders <- 1

## ----diagram------------------------------------------------------------------
ggplot(df_v2, aes(x = year, 
                  y = value, 
                  alluvium = risk_factor,
                  stratum = risk_factor)) +
geom_alluvium(aes(fill = risk_factor), 
                  alpha = 1, 
                  width = 1/2,  
                  size = 1, 
                  color = "white", 
                  decreasing = FALSE) +
geom_stratum(aes(fill = risk_factor), 
                  alpha = 1, 
                  width = 2/3,  
                  size = 1, 
                  color = "white", 
                  decreasing = FALSE) + 
scale_fill_manual(values = c("#CB6C56", "#CD925E", "#94B993", "#546C91", "#88ABC2")) +
geom_text(stat = "stratum", aes(label = value), colour = "white", size = 3, fontface = "bold", decreasing = FALSE) +
ggtitle("Risk Factor for Stroke in Blacks") + 
theme(plot.title = element_text(face = "bold", size = (10), hjust = 0.5, margin=margin(0,0,20,0))) +
scale_x_discrete(position = "top") + 
theme(axis.text.x = element_text(face = "bold", color = "black"), axis.title.x = element_blank()) +
geom_text(stat = "stratum",aes(label = ifelse(year == "1990", risk_factor," ")), hjust = 0, nudge_x = -1.75, color = "black", size = 3,  decreasing = FALSE) +
theme(axis.title.y = element_blank()) +
theme(legend.position = "none",
           panel.background = element_blank(),
           panel.grid.major = element_blank(),
           panel.grid.minor = element_blank(),
           axis.text.y = element_blank(),
           axis.ticks = element_blank())


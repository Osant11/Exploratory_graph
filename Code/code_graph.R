library(haven)
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(summarytools)
library(knitr)
library(scales)
library(ggpubr)
library(grid)
library(gridExtra)
library(cowplot)

### Please load base_data ###

base_data_1 <- base_data %>% 
  mutate(Count_Var = ifelse(Var_A_chg >= 0, rnbinom(906, size = 0.2, mu = 4), rnbinom(906, size = 0.2, mu = 1.5)), 
         Count_Var = ifelse(prob <= 0.03, NA, Count_Var), 
         Change_Var_B = ifelse(Var_A_chg >= 0, rnorm(906, 0.5, 2), rnorm(906, -0.5, 2)), 
         Var_C = case_when(Count_Var == 0 ~ rnorm(906, 0, 4), 
                           Count_Var == 1 ~ rnorm(906, 1, 4), 
                           Count_Var == 2 ~ rnorm(906, 2, 5),
                           Count_Var == 3 ~ rnorm(906, 3, 3),
                           Count_Var == 4 ~ rnorm(906, 4, 6),
                           Count_Var == 5 ~ rnorm(906, 5, 8),
                           Count_Var == 6 ~ rnorm(906, 6, 2),
                           Count_Var == 7 ~ rnorm(906, 7, 3), 
                           TRUE ~ NA_real_), 
         Count_Var2 = ifelse(Count_Var <= 7, Count_Var, NA))

#####################    CHARACTERISTICS    #####################


# Distribution of Fatigue - AVAL 

titles <- c("Histogram of Variable A at EOT")
footnotes <- c(paste(" Black line: Mean - Dashed green line : Median - Red line: Empirical Cumulative Distribution Function", 
                     "\n* Number of observations / Percentage"))

q <- ggplot(base_data_1, aes(x = Var_A)) + 
  geom_histogram(fill = "blue", col = "blue", alpha = 0.4, boundary = -2, binwidth = 0.5) 

p <- q + 
  stat_ecdf(aes(y = ..y..  * max(layer_data(q, i = 1)[1])), color = alpha("red", 0.5)) +
  stat_bin(aes(y = ..count.. + 10, label = paste(..count.., " / \n", round(..count.. / nrow(base_data_1), 3) * 100, "%*", sep = "")), 
           geom = "text", binwidth = 0.5,  boundary = -2, size = 2.5) +
  geom_vline(aes(xintercept = mean(Var_B)), size = 0.7, alpha = 0.6) +
  geom_vline(aes(xintercept = median(Var_B)), size = 0.7, linetype = "dashed", col = "green4", alpha = 0.7) +
  scale_x_continuous(breaks = seq(floor(min(base_data_1$Var_A)), ceiling(max(base_data_1$Var_A)), 0.5), 
                     limits = c(floor(min(base_data_1$Var_A)), ceiling(max(base_data_1$Var_A)))) + 
  # scale_y_continuous(breaks = seq(0, max(layer_data(q, i = 1)[1]) + 20, 10), 
  #                    sec.axis = sec_axis(~./max(layer_data(q, i = 1)[1]), breaks = seq(0,1,0.1), name = "ECDF")) + 
  xlab("Values at EOT") +
  ylab("Count") + 
  theme_bw() +
  theme(plot.caption = element_text(hjust = 0.01), 
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.background = element_blank())

#Table
p.data <- layer_data(p, i = 1) %>% 
  mutate(Dens = paste(format(round(cumsum(count/sum(count) * 100), 1), nsmall = 1), "%", sep = ""))

t <- ggplot(p.data, aes(x = x, y = "ECDF", label = Dens)) + 
  geom_text(size = 2.8) + 
  scale_x_continuous(breaks = seq(floor(min(base_data_1$Var_B)), ceiling(max(base_data_1$Var_B)) , 1), 
                     limits = c(floor(min(base_data_1$Var_A)), ceiling(max(base_data_1$Var_A)))) + 
  theme_void() +
  theme(axis.text.y = element_text(size = 9, hjust = 0.95))
  

c <- ggarrange(p, t, ncol = 1, nrow = 2, heights = c(27, 3), align = "v") 


grid.arrange(c, 
             bottom = textGrob(footnotes,
                               x = 0.01, hjust = 0, gp = gpar(fontface = 3L, fontsize = 10)),
             top = textGrob(titles,
                            x = 0.01, hjust = 0, gp = gpar(fontface = 3L, fontsize = 13))
) 



##### Dif_EOT_BSL_1: Baseline Value aginst EOT #####
##### Dif_EOT_BSL_1: Baseline Value aginst EOT ##### 
##### Dif_EOT_BSL_1: Baseline Value aginst EOT ##### 
ggplot(base_data_1, aes(x = Var_A , y = Var_A_end, col = Var_A_chg, shape = Treatment)) + 
  geom_point(position = position_jitter(), size = 1) + 
  geom_abline(slope = 1, intercept = 0, col = "wheat3") + 
  geom_abline(slope = 1, intercept = 3, col = "red2", alpha = 0.6) +
  geom_abline(slope = 1, intercept = -3, col = "green3") +
  geom_text(aes(x = 6, y = -1, label = "Treatment A = 8 \nTreatment B = 8"), col = "green4", size = 3) +
  geom_text(aes(x = -1, y = 7, label = "Treatment A = 10 \nTreatment B = 13"), col = "red4", size = 3) +
  scale_y_continuous(breaks = seq(-2, 10, 1), limits = c(-2, 8)) + 
  scale_x_continuous(breaks = seq(-2, 10, 1), limits = c(-2, 8)) +
  scale_color_gradient2(midpoint = 0, low = "green4", mid = "wheat",
                        high = "red4", space = "Lab", 
                        guide = guide_colorbar(title.position = "top", barheight = 0.5, 
                                               title.theme = element_text(size = 8))) +
  scale_shape(guide = guide_legend(order = 1)) +
  xlab("Value at Baseline") +
  ylab("Value at EOT") + 
  labs(color = "Change from Baseline", 
       shape = "", 
       caption = NULL, 
       title = "Variable A at EOT against Baseline",
       subtitle = "Beige line: No Improvement; Green line = Improvement of 3 pts; Red line = Worsen of 3 pts") + 
  theme_bw() +
  theme(legend.position="bottom", 
        legend.justification = "left", 
        plot.caption = element_text(hjust = 0),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())




##### Dif_EOT_BSL_2: Baseline Value aginst EOT and highlighted subjects with events before EOT ##### 
##### Dif_EOT_BSL_2: Baseline Value aginst EOT and highlighted subjects with events before EOT ##### 
##### Dif_EOT_BSL_2: Baseline Value aginst EOT and highlighted subjects with events before EOT ##### 
ggplot(base_data_1, aes(x = Var_A , y = Var_A_end, shape = Treatment, col = Var_A_chg)) + 
  geom_point(position = position_jitter(), size = 1) + 
  geom_abline(slope = 1, intercept = 0, col = "wheat3") + 
  geom_abline(slope = 1, intercept = 3, col = "red2", alpha = 0.6) +
  geom_abline(slope = 1, intercept = -3, col = "green3") +
  geom_text(aes(x = Var_A, y = Var_A_end + 0.17, label = Event_A), size = 3) +
  geom_text(aes(x = 6, y = -1, label = "Treatment A = 8 \nTreatment B = 8"), col = "green4", size = 3) +
  geom_text(aes(x = -1, y = 7, label = "Treatment A = 10 \nTreatment B = 13"), col = "red4", size = 3) +
  scale_y_continuous(breaks = seq(-2, 10, 1), limits = c(-2, 8)) + 
  scale_x_continuous(breaks = seq(-2, 10, 1), limits = c(-2, 8)) +
  scale_color_gradient2(midpoint = 0, low = "green4", mid = "wheat",
                        high = "red4", space = "Lab", 
                        guide = guide_colorbar(title.position = "top", barheight = 0.5, 
                                               title.theme = element_text(size = 8))) +
  scale_shape(guide = guide_legend(order = 1)) +
  xlab("Value at Baseline") +
  ylab("Value at EOT") + 
  labs(shape = "", 
       color = "Change from Baseline", 
       caption = "Red: Subject with event 15 days before EOT", 
       title = "Variable A at EOT against Baseline",
       subtitle = "Beige line: No Improvement; Green line = Improvement of 3 pts; Red line = Worsen of 3 pts") + 
  theme_bw() +
  theme(legend.position="bottom", 
        legend.justification = "left",
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        plot.caption = element_text(hjust = 0.02))



##### Dif_EOT_BSL_event: Baseline Value aginst EOT and highlighted subjects with events before EOT ##### 
##### Dif_EOT_BSL_event: Baseline Value aginst EOT and highlighted subjects with events before EOT ##### 
##### Dif_EOT_BSL_event: Baseline Value aginst EOT and highlighted subjects with events before EOT #####
ggplot(base_data_1, aes(x = Var_A , y = Var_A_end, col = Event_B, shape = Treatment)) + 
  geom_point(position = position_jitter(), size = 1) + 
  geom_abline(slope = 1, intercept = 0, col = "wheat3") + 
  geom_abline(slope = 1, intercept = 3, col = "red2", alpha = 0.6) +
  geom_abline(slope = 1, intercept = -3, col = "green3") +
  geom_text(aes(x = 6, y = -1, label = "Treatment A = 8 \nTreatment B = 8"), col = "green4", size = 3) +
  geom_text(aes(x = -1, y = 7, label = "Treatment A = 10 \nTreatment B = 13"), col = "red4", size = 3) +
  scale_y_continuous(breaks = seq(-2, 10, 1), limits = c(-2, 8)) + 
  scale_x_continuous(breaks = seq(-2, 10, 1), limits = c(-2, 8)) +
  scale_color_manual(values=c(alpha("deepskyblue1",0.6), "red2")) +
  xlab("Value at Baseline") +
  ylab("Value at EOT") + 
  labs(color = "SAE", 
       shape = "", 
       caption = "Red dot: Serious adverse event occuring 30 days before EOT", 
       title = "Variable A at EOT against Baseline",
       subtitle = "Beige line: No Improvement; Green line = Improvement of 3 pts; Red line = Worsen of 3 pts")  + 
  theme_bw() +
  theme(legend.position="bottom", 
        legend.justification = "left",
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        plot.caption = element_text(hjust = 0.02))



##### Dif_EOT_BSL_3: Baseline Value aginst EOT and filled by specific continuous variable ##### 
##### Dif_EOT_BSL_3: Baseline Value aginst EOT and filled by specific continuous variable ##### 
##### Dif_EOT_BSL_3: Baseline Value aginst EOT and filled by specific continuous variable ##### 
ggplot(base_data_1, aes(x = Var_A , y = Var_A_end, col = Change_Var_B, shape = Treatment)) + 
  geom_point(position = position_jitter(), size = 1) + 
  geom_abline(slope = 1, intercept = 0, col = "wheat3") + 
  geom_abline(slope = 1, intercept = 3, col = "red2", alpha = 0.6) +
  geom_abline(slope = 1, intercept = -3, col = "green3") +
  geom_text(aes(x = 6, y = -1, label = "Treatment A = 8 \nTreatment B = 8"), col = "green4", size = 3) +
  geom_text(aes(x = -1, y = 7, label = "Treatment A = 10 \nTreatment B = 13"), col = "red4", size = 3) +
  scale_y_continuous(breaks = seq(-2, 10, 1), limits = c(-2, 8)) + 
  scale_x_continuous(breaks = seq(-2, 10, 1), limits = c(-2, 8)) + 
  scale_color_gradientn(colors=c("green4", alpha("green3",0.8), alpha("white",0.6), alpha("black",0.8), "black"),
                        values=rescale(c(-6,-2,0,3,6)),
                        limits=c(-6,6), 
                        na.value = "tan2",
                        name = "CH. Var B",
                        guide = guide_colourbar(title.position = "top", barheight =  0.7, title.vjust = -0.5,
                                                title.theme = element_text(size = 9))) +
  scale_shape(guide = guide_legend(order = 1)) +
  xlab("Value at Baseline") +
  ylab("Value at EOT") + 
  labs(color = "SAE", 
       shape = "", 
       caption = "Red dot: Missing Change Var B", 
       title = "Variable A at EOT against Baseline",
       subtitle = "Beige line: No Improvement; Green line = Improvement of 3 pts; Red line = Worsen of 3 pts")  + 
  theme_bw() +
  theme(legend.position="bottom", 
        legend.justification = "left",
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        plot.caption = element_text(hjust = 0.02))




##### Dif_EOT_BSL_4: Baseline Value aginst EOT and filled by specific count variable ##### 
##### Dif_EOT_BSL_4: Baseline Value aginst EOT and filled by specific count variable ##### 
##### Dif_EOT_BSL_4: Baseline Value aginst EOT and filled by specific count variable ##### 
ggplot(base_data_1, aes(x = Var_A, y = Var_A_end, shape = Treatment, col = Count_Var)) + 
  geom_point(position = position_jitter(), size = 1.5) + 
  geom_abline(slope = 1, intercept = 0, col = "blue", alpha = 0.5) + 
  geom_abline(slope = 1, intercept = 3, col = "red2", alpha = 0.6) +
  geom_abline(slope = 1, intercept = -3, col = "green3") +
  geom_text(aes(x = 80, y = 4, label = "PONESIMOD = 13 \nTERIFLUNOMIDE = 22"), col = "red4", size = 3) +
  geom_text(aes(x = 15, y = 95, label = "PONESIMOD = 12 \nTERIFLUNOMIDE = 10"), col = "green4", size = 3)+
  scale_y_continuous(breaks = seq(-2, 10, 1), limits = c(-2, 8)) + 
  scale_x_continuous(breaks = seq(-2, 10, 1), limits = c(-2, 8)) +
  scale_color_gradientn(colours = c("white","dodgerblue","black"), na.value = "tan2",
                        values = c(0,1,35)/35,
                        name = "Number of events C",
                        guide = guide_colourbar(title.position = "top", barheight = 0.5,
                                                title.theme = element_text(size = 9))) +
  scale_shape(guide = guide_legend(order = 1)) +
  xlab("Value at Baseline") +
  ylab("Value at EOT") +  
  labs(shape = "", 
       title = "Variable A at EOT against Baseline colored by cumulative number of event C", 
       subtitle = "Blue line: No Improvement; Green line = Improvement of 3 pts; Red line = Worsen of 3 pts", 
       caption = "Orange dot: Missing Count assessment")  + 
  theme_bw() +
  theme(legend.position="bottom", 
        legend.justification = "left",
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        plot.caption = element_text(hjust = 0.02))




##### EVENT_VAR_REG: Continuous variable by count variables filled by specific variable #####
##### EVENT_VAR_REG: Continuous variable by count variables filled by specific variable #####
##### EVENT_VAR_REG: Continuous variable by count variables filled by specific variable #####
Var_c_descr <- base_data_1 %>% 
  group_by(Treatment, Count_Var2) %>% 
  summarise(n = n(), 
            mean = mean(Var_C)) %>% 
  group_by(Treatment) %>% 
  mutate(tot = sum(n), 
         prop = paste(round(n / tot, 3) * 100, "%", sep = ""), 
         pos = ifelse(Treatment == "Treatment A", Count_Var2 - 0.25, Count_Var2 + 0.25))

ggplot(base_data_1, aes(x = Count_Var2, y = Var_C, col = Treatment), drop=FALSE) + 
  geom_point(aes(fill = Var_A_chg), position = position_jitterdodge(0.20), stroke = 1, shape = 21) + 
  geom_point(data = Var_c_descr, aes(y = mean, x = pos), shape = 4, col = "black", size = 2) +
  geom_text(data = Var_c_descr, aes(y = -19, x = pos, label = paste(n, " /\n ", prop, "* ", sep = ""), col = Treatment), 
            size = 2.5) + 
  geom_vline(xintercept = seq(-0.5,7.5,1), linetype = "dashed", alpha = 0.5) + 
  stat_smooth(method = "lm", alpha = 0.4, geom = "line") +
  scale_x_continuous(breaks = seq(0,8,1)) +
  scale_y_continuous(breaks = seq(-15,25,5), minor_breaks = seq(-10,20,10), limits = c(-20, 25)) + 
  scale_fill_gradient2(low = "green4", mid = "white", high = "red4", midpoint = 0, 
                       limits = c(-4,4),
                       na.value = "black", name = "Change",
                       guide = guide_colourbar(title.position = "top", barheight = 0.5)) +
  scale_color_discrete(labels = c("Treatment A, n = 448", "Treatment B, n = 458"), name = " ") +
  labs(caption = paste(" * Number of observations / Percentage - x: Mean\n", 
                       "Blue and Red lines: Linear Regression Model"), 
       title = "Number of event at EOT against Variable C at EOT filled by Variable A Change") +
  ylab("Variable C") + 
  xlab("Number of events") +
  theme_bw() +
  theme(legend.position="bottom", 
        legend.justification = "left",
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        plot.caption = element_text(hjust = 0.02)) +
  guides(color = guide_legend(order = 1))

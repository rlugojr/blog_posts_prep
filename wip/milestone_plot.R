library(ggplot2)

dataset <- data.frame(id = 1:8,
                      x = c(0, 2, 4, 6, 1, 1, 6, 10),
                      xend = c(2, 4, 6, 9, 10, 4, 9, 12),
                      y = c(4, 4, 4, 3, 2, 1, 0, 0),
                      yend = c(4, 4, 4, 3, 2, 1, 0, 0),
                      label = c("parallelization", "feature\nselection", "feature\nengineering", "integrating data\nfrom literature",
                                "NGS patient data\n(when available)", "NGS\nin vitro data", "preliminary\nmodels", "final models"),
                      label_pos = c(1, 3, 5, 7.5, 5.5, 2.5, 7.5, 11))

ggplot(dataset) +
  #geom_segment(aes(x = x+0.1, xend = xend+0.1, y = (y-0.1), yend = (yend-0.1)), size = 22, alpha = 0.6, color = "lightgrey") +
  geom_segment(aes(x = x+0.05, xend = xend-0.05, y = y, yend = yend), size = 22, alpha = 0.6, color = "black") +
  geom_segment(aes(x = x+0.1, xend = xend-0.1, y = y, yend = yend), size = 20, alpha = 0.4, color = "grey") +
  guides(color = FALSE) +
  theme_minimal() +
  theme(
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_text(face = "bold", size = 16),
    axis.title.x = element_text(face = "bold", size = 16, hjust = 0),
    panel.grid.minor = element_blank()) +
  ylim(limits = c(-0.5, 4.4)) +
  scale_x_continuous(limits = c(0, 12), breaks = c(1:12)) +
  labs(x = "Month", y = "") +
  geom_text(aes(x = label_pos, y = y, label = label), color = "white", fontface = "bold")

ggsave("U:/DFG_Nachwuchsakademie/Antrag/images/timeline.pdf", width = 8, height = 4.4)

ggplot(dataset) +
  geom_segment(aes(x = x, xend = xend, y = y, yend = yend, color = factor(id)), size = 30, alpha = 0.7) +
  scale_color_brewer(palette = "Set1") +
  guides(color = FALSE) +
  theme_minimal() +
  theme(
    axis.text.y = element_blank(),
    axis.ticks = element_blank()) +
  ylim(limits = c(-0.5, 4.4)) +
  labs(x = "Month", y = "") +
  scale_x_discrete(limits = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12")) +
  geom_text(aes(x = label_pos, y = y, label = label), position = position_dodge(0.9))


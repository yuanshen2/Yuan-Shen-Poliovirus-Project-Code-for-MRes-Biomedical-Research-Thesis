library(ggplot2)

# Generate example data
x<-Outbreak_Duration_and_Campaign$Country

# Create histogram
ggplot(data.frame(x), aes(x)) +
  geom_bar(stat = "count") +
  xlab("Countries") +
  ylab("Number of Outbreaks") +
  ggtitle("Histogram of Global Polio Outbreaks") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#change colors of histogram
library(ggplot2)

x<-Outbreak_Duration_and_Campaign$Country

color_vector <- rep("deepskyblue", length(unique(Outbreak_Duration_and_Campaign$Country)))
names(color_vector) <- unique(Outbreak_Duration_and_Campaign$Country)

ggplot(data.frame(x), aes(x, fill = x)) +
  geom_bar(stat = "count") +
  xlab("Countries") +
  ylab("Number of Outbreaks") +
  ggtitle("Histogram of Global Polio Outbreaks") +
  scale_fill_manual(values = color_vector) +
  guides(fill=FALSE) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))



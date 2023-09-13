# Load necessary library
library(ggplot2)

# Create histogram
HT<-ggplot(SIAs, aes(x = `Targeted Population`)) +
  geom_histogram( aes(fill = `Vaccine Type`), alpha = 0.5) +
  facet_wrap(~ `Vaccine Type`, ncol = 2) +
  labs(title = "Histogram of Targeted Population by Vaccine Type", 
       x = "Targeted Population", y = "Number of Campaigns")
HT

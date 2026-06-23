#Testing significance between rates per year
#2019 vs.2023 --> SIGNIFICANCE (0.00036)
pop19 <- 46636+ 41531+ 36517+38458+51154+132001+92902+83701+77967+45714+22559+9182
od_19 <- 4+0+0+0+37+114+101+120+69+22+1+0

pop23 <- 44828+	38188+	36115+	39392+	52303+	143714+	102970+	78549+	76849+	58627+	26747+	9866
od_23 <- 5+0+0+11+21+120+152+154+110+30+5+0

##calculate proportions
p1 <- od_19/pop19
p2 <- od_23/pop23

##pool the proportions
p_pool <- (od_19+od_23)/(pop19+pop23)

#calculate SE
se <- sqrt(p_pool*(1-p_pool)*((1/pop19)+(1/pop23)))

#compute the z stats
z <- (p1-p2)/se

#calculate the p-value
p_value <- 2*pnorm(-abs(z))

cat("Proportion in Year 1:", p1, "\n")
cat("Proportion in Year 2:", p2, "\n")
cat("Z-statistic:", z, "\n")
cat("P-value:", p_value, "\n")

# Conclusion
if (p_value < 0.05) {
  cat("There is a significant difference between the two years.\n")
} else {
  cat("There is no significant difference between the two years.\n")
}



##2019vs.2024 --> no SIGNIFICANCE (0.271)
pop24 <- 44828+	38188+	36115+	39392+	52303+	143714+	102970+	78549+	76849+	58627+	26747+	9866
od_24 <- 5+0+0+7+15+99+132+123+101+39+1+2


p1 <- od_19/pop19
p2 <- od_24/pop24

##pool the proportions
p_pool <- (od_19+od_24)/(pop19+pop24)

#calculate SE
se <- sqrt(p_pool*(1-p_pool)*((1/pop19)+(1/pop24)))

#compute the z stats
z <- (p1-p2)/se

#calculate the p-value
p_value <- 2*pnorm(-abs(z))

cat("Proportion in Year 1:", p1, "\n")
cat("Proportion in Year 2:", p2, "\n")
cat("Z-statistic:", z, "\n")
cat("P-value:", p_value, "\n")

# Conclusion
if (p_value < 0.05) {
  cat("There is a significant difference between the two years.\n")
} else {
  cat("There is no significant difference between the two years.\n")
}



##2023vs.2024 -->SIGNIFICANCE (0.013)
p1 <- od_23/pop23
p2 <- od_24/pop24

##pool the proportions
p_pool <- (od_23+od_24)/(pop23+pop24)

#calculate SE
se <- sqrt(p_pool*(1-p_pool)*((1/pop23)+(1/pop24)))

#compute the z stats
z <- (p1-p2)/se

#calculate the p-value
p_value <- 2*pnorm(-abs(z))

cat("Proportion in Year 1:", p1, "\n")
cat("Proportion in Year 2:", p2, "\n")
cat("Z-statistic:", z, "\n")
cat("P-value:", p_value, "\n")

# Conclusion
if (p_value < 0.05) {
  cat("There is a significant difference between the two years.\n")
} else {
  cat("There is no significant difference between the two years.\n")
}

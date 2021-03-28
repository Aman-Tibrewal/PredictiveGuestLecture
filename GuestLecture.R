library(ggplot2)
library(mandelbrot)

mb <- mandelbrot(xlim = c(-0.8335, -0.8325),
                 ylim = c(0.205, 0.206),
                 resolution = 1200L,
                 iterations = 1000)
# vaccination heatmap palette
cols <- c(
  colorRampPalette(c("#e7f0fa", "#c9e2f6", "#95cbee",
                     "#0099dc", "#4ab04a", "#ffd73e"))(10),
  colorRampPalette(c("#eec73a", "#e29421", "#e29421",
                     "#f05336","#ce472e"), bias=2)(90),"black")
df <- as.data.frame(mb)
ggplot(df, aes(x = x, y = y, fill = value)) +
  geom_raster(interpolate = TRUE) + theme_void() +
  scale_fill_gradientn(colours = cols, guide = "none")


# Source blog:https://techvidvan.com/tutorials/decision-tree-in-r/
# Library & dataset: Tree and ISLR packages and car seats dataset.
# Pls install packages tree & ISLR
# Carseats dataset comes built in with ISLR

data <- Carseats

#Examine Data
str(data)

# Dataset consists of 400 observations of 11 different variables.
# Sales is a variable that shows the number of sales in a particular area.
# Sales is a suitable target variable for our decision tree.
# But for better analysis convert Sales into categorical variable Salecat.
# The value of Salecat will be Yes when Sales > 8, otherwise it will be No.

Salecat <- ifelse(data$Sales>=8,"Yes","No")
data <- data.frame(data,Salecat)
data$Salecat = as.factor(data$Salecat)


sale.tree <- tree(Salecat~.-Sales,data = data)
summary(sale.tree)

plot(sale.tree)
text(sale.tree,pretty = 0)

set.seed(40)
tree.train <- sample(1:nrow(data),250)
sale.tree <- tree(Salecat~.-Sales,data,subset=tree.train)
plot(sale.tree)
text(sale.tree, pretty=0)

sale.pred = predict(sale.tree, data[-tree.train,], type="class") 
with(data[-tree.train,], table(sale.pred, Salecat))

sale.cv = cv.tree(sale.tree, FUN = prune.misclass)
sale.cv

# Plot this mismatch curve to see
plot(sale.cv)

sale.prune = prune.misclass(sale.tree, best = 12)
plot(sale.prune)
text(sale.prune, pretty=0)

sale.pred = predict(sale.prune, data[-tree.train,], type="class")
with(data[-tree.train,], table(sale.pred, Salecat))

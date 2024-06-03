# Requires library factoextra
library("factoextra")
library("FactoMineR")

# read in Breast Cancer data set from UCI Machine Learning
wdbc <- read.csv("wdbc.data", header = F)

# assign feature names to data
features <- c("radius", "texture", "perimeter", "area",
              "smoothness", "compactness", "concavity", 
              "concave_points", "symmetry", "fractal_dimension")
names(wdbc) <- c("id", "diagnosis", paste0(features,"_mean"),
                 paste0(features,"_se"), paste0(features,"_worst"))
# exploratory data analysis
head(wdbc)
pairs(wdbc[,3:12], pch = 19)
my_cols <- c("#00AFBB", "#E7B800")
pairs(wdbc[,3:12], pch = 19,  cex = 0.5,
      col = my_cols[wdbc$diagnosis],
      lower.panel=NULL)
#Standardize the data (Center and scale).Set mean to zero and variance to 1

#Calculate the Eigenvectors and Eigenvalues from the covariance matrix or correlation matrix (One could also use Singular Vector Decomposition).

#Sort the Eigenvalues in descending order and choose the K largest Eigenvectors (Where K is the desired number of dimensions of the new feature subspace k ≤ d).

#Construct the projection matrix W from the selected K Eigenvectors.

#Transform the original dataset X via W to obtain a K-dimensional feature subspace Y.

wdbc.pr <- prcomp(wdbc[c(3:32)], center = TRUE, scale = TRUE)
summary(wdbc.pr)
res.pca <- PCA(wdbc[c(3:32)], graph = FALSE)
eig.val <- get_eigenvalue(res.pca)
eig.val

# screeplot
fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 50))


cumpro <- cumsum(wdbc.pr$sdev^2 / sum(wdbc.pr$sdev^2))
plot(cumpro[0:15], xlab = "PC #", ylab = "Amount of explained variance", main = "Cumulative variance plot")
abline(v = 6, col="blue", lty=5)
abline(h = 0.88759, col="blue", lty=5)
legend("topleft", legend=c("Cut-off @ PC6"),
       col=c("blue"), lty=5, cex=0.6)

# ELLIPSES ----------------------------------------------------------------


# plots PCs 1 and 2 labelled

plot(wdbc.pr$x[,1],wdbc.pr$x[,2], xlab="PC1 (44.3%)", ylab = "PC2 (19%)", main = "PC1 / PC2 - plot")
fviz_pca_ind(wdbc.pr, geom.ind = "point", pointshape = 21, 
             pointsize = 2, 
             fill.ind = wdbc$diagnosis, 
             col.ind = "black", 
             palette = "jco", 
             addEllipses = FALSE,
             label = "var",
             col.var = "black",
             repel = TRUE,
             legend.title = "Diagnosis") +
  ggtitle("2D PCA-plot from 30 feature dataset") +
  theme(plot.title = element_text(hjust = 0.5))

# plot with ellipses
fviz_pca_ind(wdbc.pr, geom.ind = "point", pointshape = 21, 
             pointsize = 2, 
             fill.ind = wdbc$diagnosis, 
             col.ind = "black", 
             palette = "jco", 
             addEllipses = TRUE,
             label = "var",
             col.var = "black",
             repel = TRUE,
             legend.title = "Diagnosis") +
  ggtitle("2D PCA-plot from 30 feature dataset") +
  theme(plot.title = element_text(hjust = 0.5))


# QUALITY OF REPRESENTATION -----------------------------------------------
# correlation circle
fviz_pca_var(res.pca, col.var = "black")

var <- get_pca_var(res.pca)
var
library("corrplot")
corrplot(var$cos2, is.corr=FALSE)
# Color by cos2 values: quality on the factor map
fviz_pca_var(res.pca, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE # Avoid text overlapping
)

corrplot(var$contrib, is.corr=FALSE)    
# Contributions of variables to PC1
fviz_contrib(res.pca, choice = "var", axes = 1, top = 10)
# Contributions of variables to PC2
fviz_contrib(res.pca, choice = "var", axes = 2, top = 10)



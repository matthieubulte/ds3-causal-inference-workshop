
#.libPaths("C:/ProgramData/Anaconda3/Lib/R/library");
#install.packages("AER",repos = "http://cran.us.r-project.org");
library(AER);

# load CollegeDistance data set
data("CollegeDistance")
head(CollegeDistance)
# read out relevant variables
Y <- CollegeDistance$score
X <- CollegeDistance$education
I <- CollegeDistance$distance

beta = cov(Y,I)/cov(X,I)
print(beta)

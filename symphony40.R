library(ggplot2)
library(reshape2)
library(animation)
library(gridExtra)

data <- read.table("./symphony40.txt", header=TRUE)
names(data)
attach(data)
summary(data)

z <- prcomp(data[c(6:25)])
summary(z)
z$rotation
pca <- as.data.frame(cbind(data, z$x))
names(pca)
sub.pca <- pca[,c(1,4,5,26:29)]
sub.pca$voice <- as.numeric(sub.pca$voice)

mpca <- melt(sub.pca, id=c("tone","voice","PC1","PC2","PC3","PC4"))
mpca$voice <- as.factor(mpca$voice)
mpca <- mpca[with(mpca, order(mpca$voice, mpca$value)),]
head(mpca)
max(mpca$value)

for (i in 1:4776) {
	print(i)
p <- ggplot(mpca, aes(PC1, PC2))
w <- p + geom_point(alpha=0.01, colour="grey", size=5) + geom_path(aes(colour=voice, alpha=value, size=value)) + geom_point(aes(colour=voice, alpha=value, size=value)) + scale_alpha(range=c(0.25, 1), limits=c(i-5, i)) + scale_size(range=c(1, 5), limits=c(i-5, i)) + theme_bw()  + scale_x_continuous(breaks=NULL) + scale_y_continuous(breaks=NULL) + scale_colour_manual(values=c("darkolivegreen3", "darkolivegreen4","deepskyblue3","deepskyblue4","black","goldenrod1","darkorange","darkorange","darkorange3","darkorange3","firebrick1","firebrick1","firebrick4","firebrick4")) + theme(legend.position="none", axis.title.x=element_blank(), axis.title.y=element_blank(), panel.border=element_blank(), panel.background=element_blank())

p <- ggplot(mpca, aes(PC3, PC4))
x <- p + geom_point(alpha=0.01, colour="grey", size=5) + geom_path(aes(colour=voice, alpha=value, size=value)) + geom_point(aes(colour=voice, alpha=value, size=value)) + scale_alpha(range=c(0.25, 1), limits=c(i-5, i)) + scale_size(range=c(1, 5), limits=c(i-5, i)) + theme_bw()  + scale_x_continuous(breaks=NULL) + scale_y_continuous(breaks=NULL) + scale_colour_manual(values=c("darkolivegreen3", "darkolivegreen4","deepskyblue3","deepskyblue4","black","goldenrod1","darkorange","darkorange","darkorange3","darkorange3","firebrick1","firebrick1","firebrick4","firebrick4")) + theme(legend.position="none", axis.title.x=element_blank(), axis.title.y=element_blank(), panel.border=element_blank(), panel.background=element_blank())

tiff(file=paste(i, "A.tiff", sep=""), width=1400, height=800, res=100)

grid.arrange(w,x,ncol=2)
dev.off()

}

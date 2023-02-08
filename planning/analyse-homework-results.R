

in.df <- readxl::read_xlsx("./Ageing-workshop-regional-homework-all-regions.xlsx", skip=3)
in.df$ageing.structure <- in.df$`Ageing structure`
in.df$n.years <- in.df$End.year - in.df$Start.year + 1

in.df$stock <- paste0(in.df$Region.short, "-", substring(sapply(strsplit(in.df$`Scientific name`," "), "[", 1),1,1), ". ",  sapply(strsplit(in.df$`Scientific name`," "), "[", 2))

xl <- range(in.df$Start.year, na.rm=TRUE)

keep <- which(in.df$Start.year>=1900)
nn <- length(keep)

yl <- c(0,nn+1)

use.df <- in.df[keep,]

oo <- order(use.df$Start.year, decreasing=FALSE)
use.df <- use.df[oo,]

png.fn <- "orca-plot.png"
png(png.fn, width=1200, height=1800, res=150)
par(mar=c(5,6,1,1))
plot(1970, 1, type='n', xlim=xl, ylim=yl, xlab="Year", ylab="",  yaxs="i", axes=FALSE)
grid()


for(ii in 1:nrow(use.df)){
this.df <- use.df[ii, ]
segments(this.df$Start.year, ii, this.df$End.year, ii, lty=1, lwd=5, col=grey(0.7))
}

axis(side=1, padj=-0.5)
axis(side=2, at=1:nrow(use.df), labels=use.df$stock, las=1, cex.axis=0.35)

box()
dev.off()


hist(use.df$n.years, breaks=15)


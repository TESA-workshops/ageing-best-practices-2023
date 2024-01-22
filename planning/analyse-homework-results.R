## rm(list=ls())

in.df <- readxl::read_xlsx("./Ageing-workshop-regional-homework-all-regions.xlsx", skip=3)
in.df$ageing.structure <- in.df$`Ageing structure`
in.df$n.years <- in.df$End.year - in.df$Start.year + 1

in.df$stock <- paste0(in.df$Region.short, "-", substring(sapply(strsplit(in.df$`Scientific name`," "), "[", 1),1,1), ". ",  sapply(strsplit(in.df$`Scientific name`," "), "[", 2))
in.df$sci.name <- in.df$`Scientific name`
in.df$Structure.type <- in.df$`Structure type`
in.df$taxonomic.category <- in.df$'Taxonomic category'

xl <- range(in.df$Start.year, na.rm=TRUE)

keep <- which(in.df$Start.year>=1900) ## remove rows where no start year appears
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


## break down by other variables
long.df <- in.df
long.df$Structure.type <- factor(long.df$Structure.type, levels=c("Scale","Otolith","Other"), ordered=TRUE)
long.df$taxonomic.category <- factor(long.df$taxonomic.category, levels=c("Diadromous","Marine fish","Freshwater fish","Mollusc","Marine mammal"), ordered=TRUE)

## use species and region for uniqueness, to deflate the Newfoundland case where the same species appear for each survey
long.df$uid <- paste0(long.df$Region.short, "-", long.df$`Scientific name`)
long.df$scientific.name <- long.df$`Scientific name`

library(ggplot2)

g <- ggplot(data=long.df, aes(x=Start.year, xend=End.year, y=stock, yend=stock)) +
  geom_segment() + xlab("Year") + ylab("Stock name")
g


g <- ggplot(data=long.df, aes(x=Start.year, xend=End.year, y=sci.name, yend=sci.name)) +
  geom_segment() + xlab("Year") + ylab("Stock name") +
  facet_grid(cols=vars(Structure.type), rows=vars(taxonomic.category), scales="free")
g


## use a colour palette that matches the DFO regions map
my.cols <- c("GLF"="#8eb945", "MAR"="#f0f6ca", "NL"="#93f8e6", "OP/ARC"="#b1cae0", "PAC"="#f1b99e", "QUE"="#d4b3c8")

g <- ggplot(data=long.df, aes(Region.short, fill=Region.short)) +
  geom_histogram(stat="count", colour=grey(0.5)) +
  facet_grid(cols=vars(Structure.type), rows=vars(taxonomic.category)) +
  theme_bw() +  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  xlab("DFO Region") + ylab("Number of collections") +
  scale_color_manual(name="DFO Region", values = my.cols,
                     aesthetics = c("fill"))
g

pdf.fn <- "number-of-stocks-by-structure-and-taxonomic-group.pdf"
ggsave(g, file=pdf.fn, width=6, height=8)



## histogram showing the number of years
g <- ggplot(data=long.df, aes(x=Region.short, y=n.years)) +
  geom_boxplot() +
  facet_grid(cols=vars(Structure.type), rows=vars(taxonomic.category), scales = "free") +
  theme_bw() +
  xlab("DFO Region") + ylab("Number of years of available age structures")
g
pdf.fn <- "number-of-years-by-structure-and-taxonomic-group.pdf"
ggsave(g, file=pdf.fn, width=8.5, height=11)


g <- ggplot(data=long.df, aes(x=uid, y=Start.year, colour=Region.short)) +
  geom_segment(aes(xend=uid, yend=End.year)) +
  facet_grid(cols=vars(Structure.type), rows=vars(taxonomic.category), scales = "free") +
  theme_bw() + ylab("Time span of age structure availability") + xlab("Stock") +
  theme(axis.text.x=element_blank())

g

pdf.fn <- "timespan-by-structure-and-taxonomic-group.pdf"
ggsave(g, file=pdf.fn, width=8.5, height=11)


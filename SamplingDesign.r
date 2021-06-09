setwd("C:/Users/nhy577/Documents/_toma/Rmisc/Reproducibility/ReprothonSampling")

po<- read.delim("Posts_reduced_2021-03.txt")

po$CDate<- strptime(po$CreationDate, format= "%Y-%m-%dT%H:%M:%OS")
po$Year<- factor(po$CDate[, "year"] + 1900)
str(po)

table(is.na(po$Tags))

tags<- na.exclude(po$Tags)

# convert to a vector for frequency calculation
tags.list<- as.relistable(strsplit(tags, split= c("><", "<", ">")))
tags.vec<- unlist(tags.list)
# tidy up
tags.vec<- gsub(pattern= "<", replacement= "", tags.vec)
tags.vec<- gsub(pattern= ">", replacement= "", tags.vec)

# make a list of tags for each StackOverflow question
tags.list<- relist(tags.vec)
	# check all is okay
	length(tags.list) == nrow(po)
	str(strsplit(tags, split= c("><", "<", ">"))[100000:100010])
	str(tags.list[100000:100010]) # same structure, so fine

## Tag frequency calculation, excluding "r" and empty
tags.vec<- tags.vec[!tags.vec %in% c("", "r")]
str(tags.vec)

tagfreq<- sort(table(tags.vec), decreasing= T)

head(tagfreq, 100)

pdf("TagFreq.pdf", height= 14)
par(mar= c(3.1, 10.1, 2.1, 2.1))
barplot(tagfreq[50:1], horiz= T, las= 1)
dev.off()

# (manually) creating a list of topics of interest among the top 100 tags in popularity 
dput(tagfreq[1:100])

topics<- list(data.frame= c("dataframe", "csv"),
	data.table= c("data.table"),
	tidy= c("tidyverse", "dplyr", "plyr", "tidyr", "mutate"),
	basedata= c("apply", "lapply", "merge", "aggregate", "replace", "sapply", "match"),  
	reshape= c("reshape", "reshape2"),
	baseplot= c("plot", "legend", "boxplot", "graphics", "colors"),
	lattice= c("lattice", "histogram", "heatmap"),
	ggplot2= c("ggplot2"),
	plotly= c("plotly"),
	baseobjects= c("matrix", "list", "vector", "subset", "arrays"),
	stringr= c("stringr"),
	basestring= c("regex", "split", "string", "gsub"),  
	baseprog= c("loops", "for-loop", "if-statement"),
	purr= c("purrr"),
	basetime= c("time", "date"),
	datetime= c("datetime"),
	lubridate= c("lubridate"),
	xts= c("xts"),
	igraph= c("igraph"),
	raster= c("raster"),
	zoo= c("zoo"),
	caret= c("r-caret"),
	basestats= c("lm", "mean", "filter", "sum"))


# Compute topic frequencies per year:
	# for each topic, 
		# find the subset of questions that belong to it
		# tabulate the years for that subset
TopicYear.Freq<- sapply(topics, function(x){
	po.sub<- sapply(tags.list, function(z){
		any(z %in% x)
	})
	table(po$Year[po.sub])
})

TopicYear.Freq

# how do frequencies of each topic change over time?
matplot(x= as.numeric(row.names(TopicYear.Freq)), 
		y= TopicYear.Freq+1,
		xlab= "Year",
		type= "l", log= "y")

legend(x= 2015, y= 15, legend= names(topics),
		lty= 1:length(topics),
		col= 1:length(topics),
		ncol= 3)


# Add indicator variables in `po` for topic membership
Topic.Memb<- sapply(topics, function(x){
	sapply(tags.list, function(z){
		any(z %in% x)
	})
})
colnames(Topic.Memb)<- paste("Topic", 1:length(topics), sep= "")
po<- cbind(po, Topic.Memb)

# topic membership columns index
TopMemb.Cols<- grep("Topic", names(po))

# Topic*Year combinations in long format
TopYear<- expand.grid(Year= row.names(TopicYear.Freq), Topic= colnames(Topic.Memb), stringsAsFactors= F)

# sampling
	# [1] compute number of questions already sampled in topic*year
	# [2] compute number of questions *not* yet sampled in topic*year
	# sample topic*year weighted by ((1 - [1]/max([1])) * ([2] > 0))^2 in order to even out sample size between topic*year until questions are exhausted
	# for selected topic*year, draw one out of yet unsampled questions
	# mark question as sampled



# [1] function to compute number of questions already sampled in each topic*year
TopicYear.Sampled<- function(){
	apply(po[, TopMemb.Cols], 2, FUN= function(x){
		tapply(po$Sampled[x], list(po$Year[x]), sum)
	})
}

# function to convert NA into 0
NA_into_0<- function(x){x[is.na(x)]<- 0; x}

#### export questions that belong to a topic of interest 
#### (for the Shiny sample server)
belong2topics<- apply(po[, TopMemb.Cols], 1, function(x){any(x)})

po<- po[belong2topics, ]

save(TopicYear.Freq, Topic.Memb, po, file= "shinyReprothon.RData")

# when was question last sampled (assume 1970-01-01 00:00:00 UTC if not)
Sampled<- rep(0, nrow(po))

zzs <- file("Sampled.txt", "wb")
writeBin(as.integer(Sampled), zzs, size = 4) # faster way of writing to file
close(zzs)

# tmpp<- readBin("Sampled.txt", what= integer(), size= 4, n= file.info("Sampled.txt")$size)


#### Now, create a backup file of 5000 random samples in case the server fails when needed

# Row identifier
po$Row<- 1:nrow(po)

# Indicator of whether question has yet been sampled or not
po$Sampled<- FALSE

# seed the process with one random question marked as sampled
po$Sampled[5]<- TRUE

new.seed<- 543

zz<- file("BackupRandomSampling.csv", "w")

# draw 
for(i in 1:5000){
TY.S<- as.vector(NA_into_0(TopicYear.Sampled()))
# [2] compute number of questions *not* yet sampled in topic*year
TY.NS<- as.vector(TopicYear.Freq) - TY.S

set.seed(new.seed)
# sample topic*year weighted by inverse of relative frequency
Topic.Selected<- sample((1:length(TopicYear.Freq)), size= 1,
				prob= ((1- (TY.S) / (max(TY.S))) * (TY.NS > 0))^2)

# for the selected topic*year, draw one out of the yet unsampled questions
Questions2Sample<- po$Year == TopYear$Year[Topic.Selected] & 
					po[, TopYear$Topic[Topic.Selected]] &
					!po$Sampled
New.Sample<- sample(po$Row[Questions2Sample], size= 1)

po$Sampled[New.Sample]<- TRUE

Output<- po$Id[New.Sample]

new.seed<- Output

barplot(NA_into_0(TopicYear.Sampled()), ylim= c(0, 100))

cat(paste0("https://stackoverflow.com/questions/", Output, "/"), file = zz, sep = "\n") # add the new sample to the backup file

print(i)
}

close(zz)

test.dat<- read.csv("BackupRandomSampling.csv")
str(test.dat)

# For reference, the frequency in the data
dev.new()
barplot(NA_into_0(TopicYear.Freq))

str(po)







# Tags intentionally left out because they are unspecific or refer to resources outside R:
# "shiny", "shinydashboard", 
# "python", 
# "knitr", 

# "sql",
# "data-visualization",
# "excel", 
# "machine-learning",
# "parallel-processing",
# "graph",
# "rstudio", 
# "function"
# "r-markdown", "html", "xml", 
# "rcpp", 
# "latex",
# "rvest", 


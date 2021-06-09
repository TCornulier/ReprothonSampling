lpath <- '/srv/R_library'
.libPaths(c(lpath, .libPaths()))

load("shinyReprothon.RData")

# Row identifier
po$Row<- 1:nrow(po)

# topic membership columns index
TopMemb.Cols<- grep("Topic", names(po))

# Topic*Year combinations in long format
TopYear<- expand.grid(Year= row.names(TopicYear.Freq), Topic= colnames(Topic.Memb), stringsAsFactors= F)

# sampling
	# [1] compute number of questions already completed in topic*year
	# [2] compute number of questions *not* yet completed in topic*year
	# sample topic*year weighted by ((1 - [1]/max([1])) * ([2] > 0))^2 in order to even out sample size between topic*year until questions are exhausted
	# for selected topic*year, draw one out of yet uncompleted questions
	# mark question as sampled



# [1] function to compute number of questions already answered in each topic*year
TopicYear.Avail<- function(Avail){
	apply(po[, TopMemb.Cols], 2, FUN= function(x){
		tapply(Avail[x], list(po$Year[x]), sum)
	})
}

# function to convert NA into 0
NA_into_0<- function(x){x[is.na(x)]<- 0; x}

update.Completed<- function(){
	# read the data from the Reprothon's Google form
	answ<- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vSoOTSgChKKe6A7oeS5KCgHu3BagshoWO_LhsNlaAA4K-5TNqYTllU_AqKzl0pHvLIIdbek4EkS-vy-/pub?gid=1025370078&single=true&output=csv")
	# identify completed samples
	which.done<- answ[, "Was.it.reproduced.successfully."] %in% 
					c("Pass", "Fail", "No reproducible example provided")
	Q.links<- answ[which.done, "The.link.to.your.StackOverflow.question"]
	# get list of completed question IDs
	completed.ID<- as.numeric(sapply(strsplit(Q.links, "/"), function(x){x[5]}))
	# return update of 'Completed' vector
	po$Id %in% completed.ID
}

read.Sampled<- function(){
	readBin("Sampled.txt", what= integer(), size= 4, n= file.info("Sampled.txt")$size)
}

write.Sampled<- function(x){
	zzs <- file("Sampled.txt", "wb")
	writeBin(as.integer(x), zzs, size = 4)
	close(zzs)
}


fn.sample<- function(inputData){
	# update 'Completed' and 'Sampled' vectors
	Completed<- update.Completed() # 0/1 indicator
	Sampled<- read.Sampled() # time last sampled (in seconds)

	## check which SO questions are available
	## (= yet untested and not sampled within last 15 min)
	current.time<- unclass(Sys.time()) # store current time in seconds
	# test availability of SO question
	Avail.Q<- (!Completed) & ((current.time - Sampled) > 900)

	# [1] compute number of questions already completed/sampled in each topic*year
	TY.S<- as.vector(NA_into_0(TopicYear.Avail(Avail= Avail.Q)))
	# [2] compute number of questions *not* yet completed/sampled in topic*year
	TY.NS<- as.vector(TopicYear.Freq) - TY.S

	# sample topic*year weighted by inverse of relative frequency
	Topic.Selected<- sample((1:length(TopicYear.Freq)), size= 1,
					prob= ((1- (TY.S) / (max(TY.S))) * (TY.NS > 0))^2)
	
	# for the selected topic*year, draw one out of the available SO questions
	Questions2Sample<- po$Year == TopYear$Year[Topic.Selected] & 
						po[, TopYear$Topic[Topic.Selected]] &
						Avail.Q
	New.Sample<- sample(po$Row[Questions2Sample], size= 1)

	Sampled[New.Sample]<- current.time

	write.Sampled(Sampled) # write the new 'Sampled' in file on server

	Output<- po$Id[New.Sample]

    return(paste0("https://stackoverflow.com/questions/", Output, "/"))
}


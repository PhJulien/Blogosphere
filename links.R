




library(XML)
library(RCurl)
library(stringr)

xml.url <- "http://feeds2.feedburner.com/Biopsci"
script  <- getURL(xml.url)


script  <- getURL(xml.url)
doc     <- xmlParse(script)
titles    <- xpathSApply(doc,'//item/title',xmlValue)
descriptions    <- xpathSApply(doc,'//item/description',xmlValue)
pubdates <- xpathSApply(doc,'//item/pubDate',xmlValue)
content <- xpathSApply(doc,'//item/content:encoded',xmlValue)


matched <- str_match_all(content[1], "<a href=\"(.*?)\"")
# From: http://stackoverflow.com/questions/3746256/extract-links-from-webpage-using-r

domain <- function(x) strsplit(gsub("http://|https://|www\\.", "", x), "/")[[c(1, 1)]]
# From http://stackoverflow.com/questions/19020749/function-to-extract-domain-name-from-url-in-r

grep("http[\\:/a-zA-Z0-9\\.\\?\\=&]*", content[1], ignore.case=T, value=T)

# Using igraph: http://stackoverflow.com/questions/13432444/plot-nodes-and-edges-in-a-coordinate-system-using-r
# And qgraph: http://stackoverflow.com/questions/4975681/r-creating-graphs-where-the-nodes-are-images


# The igraph book : http://igraph.sourceforge.net/igraphbook/igraphbook-creating.html
# igraph plotting example: http://stackoverflow.com/questions/13432444/plot-nodes-and-edges-in-a-coordinate-system-using-r
########
########## Whole script
#######




library(igraph)
library(XML)
library(RCurl)
library(stringr)

col_scheme <- c("tomato1", "skyblue", "wheat3", "grey80", "orange")
names(col_scheme) <- c("scilogs", "Cafe", "Media", "Libre", "ASP")


domain <- function(x) strsplit(gsub("http://|https://|www\\.", "", x), "/")[[c(1, 1)]]


RSS <- read.delim("/Users/pjulien/Code/RSS.tsv", stringsAsFactors=F)
Domain2Network <- RSS$Network
names(Domain2Network) <- RSS$Domaine
Domain2Network <- Domain2Network[!duplicated(names(Domain2Network))]

list <- list()
#g <- graph.empty(n=length(names(RSS)), directed=TRUE)
#g <- new("graphNEL", nodes, edges, "directed")

dat <- data.frame(From=c(), To=c(), weight=c(), name=c())

for (n in 1:dim(RSS)[1]) {	
	
	thisName <- as.character(RSS[n,"Nom"])
	cat(thisName <- RSS[n,"Nom"], " --- ", sep="")
	thisDomain <- RSS[n,"Domaine"]
	thisNetwork <- RSS[n,"Network"]
	xml.url <- RSS[n,"RSS"]
	
	script  <- getURL(xml.url)
	doc     <- xmlParse(script)
	
	if (thisNetwork=="ASP") {
		all_list <- xmlToList(doc)
		content <- sapply(all_list$channel[which(names(all_list$channel)=="item")], function(v) {return(v$description)})
	} else {
	
	if (length(getNodeSet(doc, "//item/content:encoded")) > 0) {
		content <- xpathSApply(doc,'//item/content:encoded',xmlValue)
		} else if (length(getNodeSet(doc, "//item/description")) > 0) {
		content <- xpathSApply(doc,'//item/description',xmlValue)
	}
	
	}
  
  list[[thisName]] <- content
	
	matched <- str_match_all(content, "<a href=\"(.*?)\"")
	all_URLs <- unlist(sapply(matched, function(e) {return(e[,2])}))
	all_domains <- tapply(all_URLs, 1:length(all_URLs), domain)
	all_domains_count <- tapply(all_domains, all_domains, length)
	
	subdat <- data.frame(From=rep(thisDomain, length(all_domains_count)), To=names(all_domains_count), weight=all_domains_count/length(content), name=thisName)
	dat <- rbind(dat, subdat)
	
}



# Graph with all nodes
g <- graph.data.frame(dat, directed=TRUE)

# Graphs with only blogs
dat2 <- dat[dat$To %in% dat$From,]

g2 <- graph.data.frame(dat2, directed=TRUE)
V(g2)$colouring <- col_scheme[Domain2Network[V(g2)$name]]
E(g2)$sizes <- log2(E(g2)$weight + 1)


plot(g2, vertex.color=V(g2)$colouring , edge.width=E(g2)$sizes, layout=layout.circle)
plot(g2, vertex.color=V(g2)$colouring , edge.width=E(g2)$sizes, layout=layout.fruchterman.reingold) # Not bad
legend("topleft", fill=col_scheme, legend=names(col_scheme), bty="n")

## Removing self citations
dat2_noself <- dat2[which(as.character(dat2$From) != as.character(dat2$To)),]

g2_noself <- graph.data.frame(dat2_noself, directed=TRUE)
V(g2_noself)$colouring <- col_scheme[Domain2Network[V(g2_noself)$name]]
# E(g2_noself)$sizes <- log2(E(g2_noself)$weight + 1)
E(g2_noself)$sizes <- E(g2_noself)$weight


plot(g2_noself, vertex.color=V(g2_noself)$colouring , edge.width=E(g2_noself)$sizes, layout=layout.circle, vertex.label=V(g2_noself)$name, vertex.label.dist=1)
plot(g2_noself, vertex.color=V(g2_noself)$colouring , edge.width=E(g2_noself)$sizes, layout=layout.fruchterman.reingold) # Not bad
legend("topleft", fill=col_scheme, legend=names(col_scheme), bty="n")


plot(g2_noself, vertex.color=V(g2_noself)$colouring , edge.width=log2(E(g2_noself)$sizes*20), layout=layout.fruchterman.reingold) # Not bad
legend("topleft", fill=col_scheme, legend=names(col_scheme), bty="n")

# Extract Names

legend_df <- as.data.frame(V(g2_noself)$name)
rownames(legend_df) <- as.character(0:(dim(legend_df) - 1))
print(xtable(legend_df) ,type="html")

# As an ordered table

dat2_noself[order(dat2_noself$weight),1:3]

##### Extracting some stats
dat3 <- dat[which(as.character(dat$From) != as.character(dat$To)),]

all_tos_count <- tapply(dat$weight, dat$To, sum)

all_tos_count_noself <- tapply(dat3$weight, dat3$To, sum)

print(xtable(as.data.frame(sort(all_tos_count_noself))), type="html")

## Nb of links per domain # Same thing as below actually...

all_froms_count <- tapply(dat$weight, dat$From, sum)

all_froms_count_noself <- tapply(dat3$weight, dat3$From, sum) # Check how this makes sense...

print(xtable(as.data.frame(sort(all_froms_count_noself))), type="html")

# Ranking nb liens / article

nb_link_per_article <- tapply(dat$weight, dat$From, sum)

dat_no_self <- dat[which(as.character(dat$From) != as.character(dat$To)),]
nb_link_per_article_no_self <- tapply(dat_no_self$weight, dat_no_self$From, sum)

print(xtable(as.data.frame(sort(nb_link_per_article_no_self))), type="html")

nb_link_per_article_name <- tapply(dat3$weight, dat3$name, sum)
print(xtable(as.data.frame(sort(nb_link_per_article_name))), type="html")


####### Most Cited blogs
# Nb link pointing to each domain
nb_link_to_domain_no_self <- tapply(dat_no_self$weight, dat_no_self$To, sum)
# Filtering for blogs only
print(xtable(as.data.frame(sort(nb_link_to_domain_no_self[names(nb_link_to_domain_no_self) %in% dat$From]))), type="html")


########## TO DO
#### Interactions between and within network
#### Include orphan nodes in the displayed graph 
#### Link to original articles X
### Most cited blogs? X





#### Link to original articles 

journals <- c("sciencemag.org", "nature.com", "plosbiology.org", "plosone.org", "pnas.org", "plosgenetics.org", "ploscompbiol.org", "rsbl.royalsocietypublishing.org", "rspb.royalsocietypublishing.org", "cell.com", "sciencedirect.com", "arxiv.org", "biorxiv.org", "frontiersin.org", "frontiersinzoology.com", "mbe.oxfordjournals.org", "ncbi.nlm.nih.gov")


dat_to_journal <- dat[dat$To %in% journals,]

sort(tapply(dat_to_journal$weight, dat_to_journal$name, sum))
as.data.frame(sort(tapply(dat_to_journal$weight, dat_to_journal$name, sum)))

print(xtable(as.data.frame(sort(tapply(dat_to_journal$weight, dat_to_journal$name, sum)))), type="html")

## Adding orphan vertices

# plot(g2_noself, vertex.color=V(g2_noself)$colouring , edge.width=E(g2_noself)$sizes, layout=layout.fruchterman.reingold) # Not bad
# legend("topleft", fill=col_scheme, legend=names(col_scheme), bty="n")
# 
# 
# all_from_domains <- as.character(unique(dat$From))
# 
# unique(as.character(dat[!(as.character(dat$From) %in% dat2$From), "From"]))
# as.data.frame(sort(tapply(dat_to_journal$weight, dat_to_journal$name, sum)))


### Interaction entre reseaux

agg <- aggregate(dat2_noself$weight, by=list(FROM=Domain2Network[as.character(dat2_noself$From)], TO=Domain2Network[as.character(dat2_noself$To)]), median)
agg <- agg[order(agg$FROM),]

par(mar=c(8, 4, 4, 2) + 0.1)
barplot(agg$x, names.arg=paste(agg$FROM, ">", agg$TO, sep=""), las=3, col=col_scheme[agg$TO], density=20, border=col_scheme[agg$FROM], lwd=2)


### Liens vers et depuis reseaux
#### Check how rliable are those numbers

agg2 <- aggregate(dat2_noself$weight, by=list(FROM=Domain2Network[as.character(dat2_noself$From)]), median) #Median, sum?
agg3 <- aggregate(dat2_noself$weight, by=list(TO=Domain2Network[as.character(dat2_noself$To)]), median)


####### Saving some objects

save(list, file="/Users/pjulien/Code/LinksResults/Liste_tous_contenus.RData")
write.table(dat, file="/Users/pjulien/Code/LinksResults/Tous_liens.txt", quote=F, row.names=F)
write.table(dat2_noself[order(dat2_noself$weight),1:3], file="/Users/pjulien/Code/LinksResults/Liens_graphique_blog.txt", quote=F, row.names=F)
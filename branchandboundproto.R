#Criteria
#Distance, Cost, Crowdedness, Alcohol Choice, PageRank

#Assume 10 bars. 
#Cost is a relative 1-10, Crowdedness likewise, Alcohol Choice is Liquor Preference 1-10 (Beer preference is then 10-alcohol choice), Pagerank is also 1-10.
#Distances is an nxn matrix 

#User Defined Parameters
Nobot=12  #max number of bars on tour
#Preferences 1-10
costpref=8
crowdpref=4
alcpref=2
#Pagerank is outside of user control.

#Lets generate sample data matrices:
bars=c('Allen St. Grill','Bar Bleu','The Brewery','CafÃ© 210','Chilis','Chrome','Chumleys','Darkhorse Tavern','Gingerbread Man','Indigo','Inferno','Kildares', 'Levels','Lions Den','Local Whiskey',
'Mad Mex','The Phyrst','Bill Pickles Tap Room', 'The Rathskeller','Rotellis','Rumors Lounge','The Saloon','The Shandygaff','The Tavern Restaurant','Z Bar @ The Deli','Zenos') 

Costvalues=rpois(length(bars),4);names(Costvalues)=bars #want to minimize this always; 
Crowdvalues=rpois(length(bars),4);names(Crowdvalues)=bars
Alcvalues=rpois(length(bars),4);names(Alcvalues)=bars
Pagerankvalues=rpois(length(bars),1);names(Pagerankvalues)=bars

#Making distances:
dvals=matrix( rnorm(length(bars)*length(bars),mean=.2,sd=.4), length(bars), length(bars)) #helps create distances
distances=as.data.frame(as.matrix(dist(dvals,upper=T))) #Keep the upper because we dont have to worry about switching i and j.
names(distances)=bars
row.names(distances)=bars
# Lets Sort the user preference.
prefrank=cbind(c(costpref,alcpref,crowdpref),c("Cost","Alc","Crowd"))
rankedpreferences=as.character(as.data.frame(prefrank)[order(as.data.frame(prefrank)$V1),]$V2)


two_bar_bakeoff=function(userprefs,bar){ 
#find the nearest two bars
firstbar=names(sort(distances[bar,])[2]) #first is the bar itself
secondbar=names(sort(distances[bar,])[3]) #first is the bar itself
firstbarcost=Costvalues[[firstbar]]; secondbarcost=Costvalues[[secondbar]]
firstbarcrowd=Crowdvalues[[firstbar]]; secondbarcrowd=Crowdvalues[[secondbar]]
firstbaralc=Alcvalues[[firstbar]]; secondbaralc=Alcvalues[[secondbar]]
firstbarpagerank=Pagerankvalues[[firstbar]]; secondbarpagerank=Pagerankvalues[[secondbar]]

#lets make it really greedy by minimizing the weighted sum of the objectives at the branch.
#If not, rankedpreferences is stored as a list, and userprefs is a dummy input to adjust this
costfactor=-1.2 #20% percent more important
pagerankfactor=-3
firstbarscore=firstbarcrowd*crowdpref+firstbaralc*alcpref+firstbarcost*costfactor*costpref+pagerankfactor*firstbarpagerank  #assume higher cost rank means more expensive
secondbarscore=secondbarcrowd*crowdpref+secondbaralc*alcpref+secondbarcost*costfactor*costpref+pagerankfactor*secondbarpagerank
if(min(firstbarscore,secondbarscore)==firstbarscore){return(firstbar)}else
return(secondbar)
}
 #Main Function
#starting bar
startingbar="Bar Bleu" #set the starting bar
bartour<-c()
for(i in 1:Nobot){
if(i==1){bartour[1]=two_bar_bakeoff(rankedpreferences,startingbar); 
columntoremove=which(names(distances)==bartour[1])
distances=as.data.frame(distances[,-columntoremove])
}
else {
bartour[i]=two_bar_bakeoff(rankedpreferences,bartour[i-1]);
columntoremove=which(names(distances)==bartour[i])
distances=distances[,-columntoremove]
}
}

print(bartour)




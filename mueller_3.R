library(magrittr)
library(dplyr)
library(ggpubr)
library(Rtsne)
library(ggplot2)
library(reshape)
library(ggrepel)

groupby_mat_names = function(mat, names, new_name){
  new_name = mat[,names]
  mat[,new_name] = new_name
  mat = mat[,-which(colnames(mat) %in% names)]
  return(mat)
}

build_graph = function(data){
  laplacian = ((as.matrix(t((data)))%*%as.matrix((data))))
  
  laplacian = laplacian/rowSums(laplacian)
  if(sum(diag(laplacian) ==1) >0){laplacian = laplacian[-which(diag(laplacian)==1),-which(diag(laplacian)==1)]}
  
  laplace=diag(rowSums(abs(laplacian)))-laplacian
  eig.laplace=eigen(laplace)
  lenght_mat = dim(eig.laplace[[2]])[[1]]
  
  
  get_vectors = eig.laplace[[2]][,(lenght_mat-1):(lenght_mat-2)]
  
  colnames(get_vectors) = c('vect1','vect2')
  get_vectors = data.frame(get_vectors)
  
  get_vectors[,'vect1'] = Re(get_vectors[,'vect1'])
  get_vectors[,'vect2'] = Re(get_vectors[,'vect2'])
  get_vectors[,'name']= colnames(laplacian)
  
  get_weights = as.matrix(t(data))%*%as.matrix(data)
  edge_list = melt(get_weights*upper.tri(get_weights))
  edge_list = subset(edge_list, value != 0)
  edge_list_1 = merge(edge_list, get_vectors, by.x = 'X1', by.y = 'name' , all.x = TRUE)
  edge_list_1 = merge(edge_list_1, get_vectors, by.x = 'X2', by.y = 'name', all.x = TRUE )
  temp = ggplot(get_vectors, aes(x = vect1, y = vect2))+geom_segment(aes(x = vect1.x, y = vect2.x, xend = vect1.y, yend = vect2.y), data = edge_list_1,alpha = .1)+
    geom_point()
  temp = temp + geom_text_repel(aes(x = vect1, y = vect2, label = name), data = get_vectors)+
    theme_minimal()+labs(x = 'n-1 Laplace Eigenvector', y = 'n-2 Laplace Eigenvector')
  plot(temp)
  return(list(temp, get_vectors))
  
}

find_words = function(x, text){
  temp_vector = rep(0, length(text))
  text_locs = grep(x, text)
  temp_vector[text_locs] = 1
  return(temp_vector)
  
}

remove_words = function(names, data){
  temp_data = data[,-which(colnames(data) %in% names)]
  return(temp_data)
}

new_df = read.csv('/users/sweiss/downloads/mueller_report_by_topic.csv')


names = c('clinton','trump','flynn','stone','podesta','netyksho','sessions','guccifer','page','uhder','assange',
          'cohen','gates','manafort','clovis','smith','ledeen','oknyansky','corsi','bannon','prince','crocus','kaveladze',
          'goldstone','agalarov','sater','putin','klokov','lewandowski','peskov','miller','papadopoulos',
          'mifsud',"polonskaya","timofeev", 'gordon','millian', 'carter','klimentov','kislyak','simes','burt',
          'vargas','veselnitskaya', "magnitsky","samochornov","denman","deripaska","kilimnik", 
          "yanukovych","mcfarland","aven","dmitriev","nader","seychelles",
          "gerson","berkowitz","gorkov", 'djtjr', 'glassner','landrum', 'malloch','obama','Podobnyy', 'Poliakova', 'rozov','supra','weber','Buryakov',
          'Dvorkovich', 'Hicks', 'ivanka','mcgee','mey','skiber','ward','DonaldJTrumpJr','Boyarkin','Patten', 'Preteetee', 'Phares','Akhmetshin',
          'Erchova','flaherty','garten','Khalilzad','Klein', 'Konstantin', 'Papadopoulo', 'Szobocsan', 'Mashburn', 'pinedo','Zakharova','jr', 'donald trump jr', 'ivanka trump', 'kushner','candidate trump', "GRU", "IRA",
          'prigozhin','rasin', 'prikhodko', 'foresman', 'kobyakov', 'rtskhiladze','oganov')





library(tokenizers)
new_df[,3]= as.character(new_df[,3])
split_text = tokenize_sentences(as.character(new_df[,3]))
length_of_headers = sapply(split_text, length)
sentence_labels = rep(new_df[,2], length_of_headers)

split_text = unlist(split_text)
split_text = tolower(split_text)

mat_1 = do.call(cbind,lapply(tolower(names), function(x) find_words(x,split_text)))
colnames(mat_1) = tolower(names)

mat_1[which(mat_1[,'donald trump jr'] >0 ), 'jr'] = 1
mat_1[which(mat_1[,'djtjr'] >0 ), 'jr'] = 1
mat_1[which(mat_1[,'donaldjtrumpjr'] >0 ), 'jr'] = 1
mat_1[which(mat_1[,'papadopoulo'] >0 ), 'papadopoulos'] = 1
mat_1[which(mat_1[,'ivanka trump'] >0 ), 'ivanka'] = 1

mat_1[,'trump'] = (mat_1[,'candidate trump'])>0
mat_1[,'page'] =0
mat_1[grep(' Page',unlist(tokenize_sentences(new_df[,3]))),'page'] = 1
mat_1[,'sessions'] =0
mat_1[grep(' Sessions',unlist(tokenize_sentences(new_df[,3]))),'sessions'] = 1
mat_1[,'ward'] = 0
mat_1[grep(' Ward',unlist(tokenize_sentences(new_df[,3]))),'ward'] = 1
mat_1[,'aven'] = 0
mat_1[grep(' Aven',unlist(tokenize_sentences(new_df[,3]))),'aven'] = 1
mat_1[,'simes'] = 0
mat_1[grep(' Simes',unlist(tokenize_sentences(new_df[,3]))),'simes'] = 1




to_remove_duplicates = c('')
to_remove = c('uhder', 'podesta', 'corsi', 'donald trump jr', 'candidate trump', 'papadopoulo',
              'konstantin', 'seychelles', 'obama', 'magnitsky', 'president trump', 'yanukovych', 'carter',
              'preteetee', 'mey',  'crocus','supra', 'ira','gru', 'donald trump jr', 'djtjr','donaldjtrumpjr',
              'guccifer','netyksho','clinton','donald trump jr', 'putin')



mat_2 = remove_words(to_remove, mat_1)

num_occurances_names = data.frame(names = colnames(mat_2), counts = colSums(mat_2))
num_occurances_names[,'names'] = factor(num_occurances_names[,'names'], num_occurances_names[order(colSums(mat_2),decreasing=TRUE),'names'])
#keep top 20 names
num_occurances_names = num_occurances_names[order(num_occurances_names[,'counts'],decreasing=TRUE)[1:20],]
freq_occurances_plot = ggplot(num_occurances_names, aes(x = names, y = counts))+
  geom_bar(stat = 'identity') +theme_minimal()+ theme(axis.text.x = element_text(angle = 90, hjust = 1))




mueller_network = build_graph((mat_2[,-which(colSums(mat_2)<20)]))[[1]]
mueller_network = mueller_network+ggtitle('Graph Laplacian for Mueller Report Names (>30 occurances)')

#agg_mat_1 = aggregate(mat_2 ~ sentence_labels, FUN = sum)
#agg_mat_1_melt = melt(agg_mat_1, id.vars = 'sentence_labels')

#get num times name appears in subtext. not that informative 
#ggplot(agg_mat_1_melt, aes(x = variable, y = log(value+1))) + 
#  geom_bar(stat = "identity")+facet_grid(sentence_labels~.)+ 
#  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
#  theme(strip.text.y = element_text(angle = 360))

#agg_mat_1 = aggregate(mat_2 ~ sentence_labels, FUN = sum)
#agg_mat_1_melt = melt(agg_mat_1, id.vars = 'sentence_labels')


##


russians = c("oknyansky", 'klokov', "kaveladze", "goldstone", 'agalarov', 'aras','peskov','klimentov',
             "mifsud"  , "polonskaya"   , "timofeev","millian" , "kislyak" ,"veselnitskaya", "samochornov",
             'deripaska','kilimnik','dmitriev','aven', 'gorkov', 'netyksho',"Podobnyy","Poliakova","rozov",
             "Buryakov","Dvorkovich","Boyarkin" ,"Phares","Akhmetshin","Erchova", "Khalilzad" ,'prigozhin', 'rasin',
             'rtskhiladze'
)
russian_mat = (mat_2[,which(colnames(mat_2) %in% russians)])
russian_mat = russian_mat[,-which(colSums(russian_mat)<3)]
russian_network = build_graph(russian_mat)[[1]]
russian_network = russian_network +ggtitle('Graph Laplacian for Russian Names (>3 occurances)')

#

#trump business
group_3 = c("agalarov"  , "aras"   , "goldstone","samochornov", "veselnitskaya",'kaveladze','akhmetshin' ) # trump moscow project / trump tower meeting
group_4 = c('klokov','erchova') # trump moscow w/ cohen  - klokov wanted to meet with cohen but cohen wanted it professional 
group_7 = c('rtskhiladze', 'rozov') # trump moscow w/ choen

#the oppurtunists
group_2 = c("mifsud"  , "polonskaya"   , "timofeev","millian" ) # papadope 
group_5 = c('klimentov','poliakova','peskov','dvorkovich') # mostly page although cohen did email 

#the 'professionals' 
group_8 = c('oknyansky','rasin') # tried to sell to cohen / stone
group_1 = c('kilimnik','deripaska', 'boyarkin', 'oganov') # manafort 

#russian officiials and lackies 
group_6 = c('kislyak', 'gorkov') # ambassador 
group_9 = c('aven','dmitriev') #putin's oligarchs /seychelles 
new_numbers = data.frame(old = c(3,4,7,2,5,8,1,6,9),
                         new = c(1,2,3,4,5,6,7,8,9),
                         cat_names = c('Trump Business','Trump Business','Trump Business',
                                       'The Opportunists', 'The Opportunists', 'The Professionals',
                                       'The Professionals', "Russian Officials and Lackies", "Russian Officials and Lackies"))


#trump business
group_1 = c("agalarov"  , "aras"   , "goldstone","samochornov", "veselnitskaya",'kaveladze','akhmetshin' ) # trump moscow project / trump tower meeting
group_2 = c('klokov','erchova') # trump moscow w/ cohen  - klokov wanted to meet with cohen but cohen wanted it professional 
group_3 = c('rtskhiladze', 'rozov') # trump moscow w/ choen

#the oppurtunists
group_4 = c("mifsud"  , "polonskaya"   , "timofeev","millian" ) # papadope 
group_5 = c('klimentov','poliakova','peskov','dvorkovich') # mostly page although cohen did email 

#the 'professionals' 
group_6 = c('oknyansky','rasin') # tried to sell to cohen / stone
group_7 = c('kilimnik','deripaska', 'boyarkin', 'oganov') # manafort 

#russian officiials and lackies 
group_8 = c('kislyak', 'gorkov') # ambassador 
group_9 = c('aven','dmitriev') #putin's oligarchs /seychelles 


mat_renamed = as.data.frame(mat_2)
mat_renamed[,'group_1'] = rowSums(mat_renamed[,which(colnames(mat_renamed) %in% group_1)])
mat_renamed[,'group_2'] = rowSums(mat_renamed[,which(colnames(mat_renamed) %in% group_2)])
mat_renamed[,'group_3'] = rowSums(mat_renamed[,which(colnames(mat_renamed) %in% group_3)])
mat_renamed[,'group_4'] = rowSums(mat_renamed[,which(colnames(mat_renamed) %in% group_4)])
mat_renamed[,'group_5'] = rowSums(mat_renamed[,which(colnames(mat_renamed) %in% group_5)])
mat_renamed[,'group_6'] = rowSums(mat_renamed[,which(colnames(mat_renamed) %in% group_6)])
mat_renamed[,'group_7'] = rowSums(mat_renamed[,which(colnames(mat_renamed) %in% group_7)])
mat_renamed[,'group_8'] = rowSums(mat_renamed[,which(colnames(mat_renamed) %in% group_8)])
mat_renamed[,'group_9'] = rowSums(mat_renamed[,which(colnames(mat_renamed) %in% group_9)])


mat_renamed = mat_renamed[,-which(colnames(mat_renamed) %in% c(group_1, group_2, group_3, group_4, group_5, group_6,group_7,group_8,group_9))]
build_graph((mat_renamed[,-which(colSums(mat_renamed)<20)]))


manual_relations = read.csv('/users/sweiss/downloads/mueller_network_data_1.csv')

new_numbers = data.frame(old = c(3,4,7,2,5,8,1,6,9),
                         new = c(1,2,3,4,5,6,7,8,9),
                         cat_names = c('Trump Business','Trump Business','Trump Business',
                                       'The Opportunists', 'The Opportunists', 'The Professionals',
                                       'The Professionals', "Russian Officials and Lackies", "Russian Officials and Lackies"))


manual_relations[,'categories'] = new_numbers[,3][match(manual_relations[,'group'], new_numbers[,1])]

manual_relations[,'group'] = new_numbers[,2][match(manual_relations[,'group'], new_numbers[,1])]
manual_relations[71,'categories'] = 'Trump Business'
manual_relations[,'date'] = as.Date(manual_relations[,'date'], '%m/%d/%Y')
manual_relations[,'degree'] = as.factor(manual_relations[,'degree'])
manual_relations[,'group'] = as.factor(manual_relations[,'group'])

manual_relations[,'person'] = factor(manual_relations[,'person'], c('papadopolous','clovis','lewandowski','gates','manafort','cohen','ivanka','jr','miller','page',
                                                                    'phares','kushner','sessions','gordon','prince','bannon','flynn','mcfarland', 'caputo','stone' ))

manual_relations$value = 1
manual_relations_adjacency = reshape((manual_relations[,c(1:2,6)]), idvar = "person", timevar = "group", direction = "wide")
manual_relations_adjacency[is.na(manual_relations_adjacency)] = 0
manual_relations_adjacency = (manual_relations_adjacency>0)*1
temp_names = manual_relations_adjacency[,1]

#laplace_vectors = build_graph(new_manual_relations_adjacency[,1:18])[[2]]
#manual_relations[,'person'] = factor(manual_relations[,'person'], c('caputo','stone',laplace_vectors[order(laplace_vectors[,'vect2']),'name']))

test_ordering = c('caputo','stone','ivanka','jr','cohen','gates','manafort',
                  'clovis','phares','papadopolous','miller','lewandowski','kushner','prince',
                  'page','bannon','mcfarland','sessions','gordon','flynn')
manual_relations[,'person'] = factor(manual_relations[,'person'], test_ordering)

library(RColorBrewer)
myColors <- brewer.pal(9,"Set3")
myColors <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7","#000000")
names(myColors) <- levels(unique(manual_relations[,'group']))
colScale <- scale_colour_manual(name = "grp",values = myColors)



ts_relations_plot =ggplot((unique(manual_relations)), aes(x = as.Date(date), y = person, colour = group, shape = as.factor(degree)))+
  #geom_jitter(height = 0)+
  geom_text_repel(aes(label=paste(group, "^(", degree, ")", sep = "")),parse = TRUE)+labs(x = 'Date', y = 'Person Name', title = paste("Person-Russian Group TS Graph"))+
  theme_minimal()+colScale+theme(legend.position = 'none')

manual_relations[,'group_1'] = as.character(manual_relations[,'group'])

ts_plots_by_cat = lapply(unique(manual_relations[,'categories']), function(x)
  ggplot(subset(unique(manual_relations), categories == x), aes(x = as.Date(date), y = person, colour = group, shape = as.factor(degree)))+
    #geom_jitter(height = 0)+
    geom_text_repel(aes(label=paste(group, "^(", degree, ")", sep = "")),parse = TRUE, nudge_x= .1)+labs(x = 'Date', y = 'Person Name', title = paste(as.character(x)))+
    theme_minimal()+colScale+
    theme(legend.position = 'none')
  
  )
names(ts_plots_by_cat) = unique(manual_relations[,'categories'])

name_locs = data.frame(test_ordering, name_locs= (1:length(test_ordering))/length(test_ordering) )
slope_graph = merge(manual_relations, name_locs, by.y = 'test_ordering', by.x = 'person')
slope_graph$ones = 1
slope_graph$two = 2

slope_graph[,'group_names'] = sapply(list(c('1',group_1), c('2',group_2), c('3',group_3), c('4',group_4), c('5',group_5), c('6',group_6),c('7',group_7),c('8',group_8),c('9',group_9)), function(x) paste(x, collapse = ' '))[slope_graph[,'group']]

slope_graph_g = ggplot(unique(slope_graph[,c('ones','two','name_locs','group','person','group_names')]))+
  geom_segment(aes(x = ones, y = name_locs, xend = two, yend = (as.numeric(group))/9,colour = as.factor(group)),
               data = unique(slope_graph[,c('group_names','ones','two','name_locs','group','person')]), alpha = .2)+
  geom_text( aes(x = ones, y= name_locs, label = person,angle = 45))+geom_text( aes(x = two, y= (as.numeric(group))/9, 
                                                                                    label = stringr::str_wrap(group_names,4), colour = as.factor(group) ))+
  theme_minimal()+
  theme(axis.text.x=element_blank(),axis.text.y=element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position = 'none')+xlim(.95,2.1)+
  labs(x = '',y='') + ggtitle('Trump Campaign Officials Relations With Russian "Groups"')+colScale
  

#new_manual_relations_adjacency = as.data.frame(t(manual_relations_adjacency[,-1]))
#colnames(new_manual_relations_adjacency) = temp_names
#new_manual_relations_adjacency[,'degree'] = row.names(new_manual_relations_adjacency)
#new_manual_relations_adjacency_melt = melt(new_manual_relations_adjacency, id.vars = 'degree')
#new_manual_relations_adjacency_melt = subset(new_manual_relations_adjacency_melt, value > 0)

#new_manual_relations_adjacency_melt[,1] = gsub('degree','group',new_manual_relations_adjacency_melt[,1])
#campaign_members_by_group = ggplot(new_manual_relations_adjacency_melt, aes(x = variable, y = degree))+geom_point()+ 
#  theme_minimal()+
#  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
#  theme(strip.text.y = element_text(angle = 360))+
#  labs(x = 'Campaign Member', y = 'Group ID',title = 'Contact or Knowledge of Group by Person')

save(slope_graph_g,ts_plots_by_cat,ts_relations_plot,  file = '/users/sweiss/src/mueller_report/data/mueller_plots_slope_graph.rdata')
save(freq_occurances_plot,mueller_network,russian_network,ts_relations_plot, file = '/users/sweiss/src/mueller_report/data/mueller_plots.rdata')

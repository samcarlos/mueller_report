library(dplyr)

data = read.csv('/data/mueller_report.csv')
test = str_replace_all(test, "[[:punct:]]", " ")
test = lapply(strsplit(test, '    '), function(x) x[1])
first_space = lapply(strsplit(unlist(test), NULL), function(x) x[1])
headers = unlist(test)[which(is.na(as.numeric(first_space)) == FALSE)]
headers_2 = unlist(lapply(strsplit(unlist(headers), ' '), function(x) paste(unlist(x[-c(1:2)]), collapse = ' ')))
headers_2 = gsub('U S ', "U.S.",headers_2)
headers_2 = gsub('IRA Controlled', "IRA-Controlled",headers_2)
headers_2 = gsub(' Guccifer 2 0', "Guccifer 2.0",headers_2)
headers_2 = headers_2[-which(headers_2 == "")]

headers_locations = sapply(headers_2, function(x) print(agrep(x, data[,3])[2]))

header_locs = agrep(headers_2[1], data[,3])[2]

for(x in headers_2[-1]){
  proposed_locs = agrep(x, data[,3])
  prev_loc = header_locs[length(header_locs)]
  diff_locs = proposed_locs - prev_loc
  proposed_locs = proposed_locs[-which(diff_locs<0)]
  new_loc = proposed_locs[which.min(proposed_locs)]
  header_locs = c(header_locs, new_loc) 
}


text = c()
for(x in 1:(length(header_locs))){
  text = c(text, paste0(data[header_locs[x]:(header_locs[x+1]-1),3], collapse = ' '))
}

new_df = data.frame(headers = headers_2[-length(headers_2)], text = text)
write.csv(new_df, 'data/mueller_report_by_topic.csv')

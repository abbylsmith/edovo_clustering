all.files <- map_chr(1:9, function(x) paste0("data_", "C", str_pad(x, 3, pad = "0"), "_Dedup.RData"))
mylist<- lapply(all.files, function(x) {
  load(file = x)
  get(ls()[ls()!= "filename"])
})

names(mylist) <- all.files #Note, the names here don't have to match the filenames
all_blocks <- do.call("rbind", mylist)


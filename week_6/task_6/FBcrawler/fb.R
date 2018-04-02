library(httr)

name = "chenchimai2017"
token = ""

url = paste0("https://graph.facebook.com/v2.12/",
             name,
             "?fields=posts&access_token=",
             token)

res = httr::GET(url)
post = content(res)

while( !is.null(url) )
{
  if( is.null(post$posts$data) )
  {
    data = post$data  
  }
  else
  {
    data = post$posts$data
  }
  pageNo = length(data)
  from = 1
  date = data[[from]]$created_time
  currentY = strsplit(date, '-')[[1]][1]
  currentM = strsplit(date, '-')[[1]][2]
  currentM = paste0(currentY,'_',currentM)
  saveData = list(data[[from]])

  for( id in c(2:pageNo) )
  {
    date = data[[id]]$created_time
    year = strsplit(date, '-')[[1]][1]
    month = strsplit(date, '-')[[1]][2]
    month = paste0(year,'_',month)    
    if( currentM == month )
    {
      to = id
      saveData = append(saveData, data[[to]])
    }
    else
    {
      filename = paste0("./",name,"/",currentM,".txt")
      currentM = month
      write(unlist(saveData), filename, append = TRUE)
      from = to
      saveData = list(data[[from]])
    }
  }
  
  url = paste0(post$paging$`next`,
               post$posts$paging$`next`)
  res = httr::GET(url)
  post = content(res)
}

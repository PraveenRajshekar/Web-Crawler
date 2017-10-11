df=data.frame()
for(j in 1:20){
  uuu="https://www.zillow.com/homes/for_sale/Philadelphia-PA/13271_rid/50000-100000_price/189-378_mp/globalrelevanceex_sort
  /40.1529,-74.907188,39.852039,-75.331535_rect/10_zm/0_mmm/"
  
  idirect =gregexpr("zm(.*?)",uuu)
  idirect=unlist(idirect)+3
  idirect123=gregexpr("mmm(.*?)",uuu)
  idirect123=unlist(idirect123)-2
  newurl=paste(substr(uuu, 1, idirect), j,"_p/", substr(uuu, idirect123, nchar(uuu)), sep = "")
  url = newurl
  if(j==1)
    url=uuu
  html <- paste(readLines(url), collapse="\n")
  match <- str_match_all(html, "<a href=\"/homedetails(.*?)\"")
  match1=as.data.frame(match)
  mainurl="https://www.zillow.com/"
  
  for (i in 1:26) {
    u= paste(mainurl,substr(match1$X1[i],11,stri_length(match1$X1[i])-1),sep='')
    zz= read_html(u)
    
    
  
    #url<-("https://www.zillow.com/homedetails/2214-N-Bancroft-St-Philadelphia-PA-19132/10245336_zpid/")
    url1 =u
    connection="zillow_home.js"
    
    Hmid= zz %>%  html_nodes(".z-moreless-content") %>% html_text
    #urll link of each house
    hmid=substr(u,65,(stri_length(u)-6))
    idirect =gregexpr("/",hmid)
    hmid1=substr(hmid,idirect,(stri_length(hmid)))
    hmid2=substr(hmid1,2,(stri_length(hmid1)))
    HomeID=as.numeric(hmid2)
    # HomeID of
    ## Home ID extracted
    
    
    skl= zz %>%  html_nodes(".clearfix") %>% html_text
    f1=gsub(" {1-10}","",skl[4])
    f2=gsub("\n{2}","",f1)
    f3=gsub("\n","_",f2)
    school1=f3
    f1=gsub(" {1-10}","",skl[5])
    f2=gsub("\n{2}","",f1)
    f3=gsub("\n","_",f2)
    school2=f3
    f1=gsub(" {1-10}","",skl[6])
    f2=gsub("\n{2}","",f1)
    f3=gsub("\n","_",f2)
    school3=f3
    school=paste(school1,school2,school3,sep=';')
    
    n= zz %>%  html_nodes("h2") %>% html_text
    n=n[grep("Neigh(.*?)",n)]
    Neighborhood=gsub("Neighborhood: ","",n)
    
    writeLines(sprintf("var webPage = require('webpage');
                       var page = webPage.create();
                       
                       var fs = require('fs');
                       var path = './rproject/%s.html'
                       
                       page.open('%s', function () {
                       window.setTimeout(function () {
                       var content = page.content;
                       fs.write(path,content)
                       
                       phantom.exit();
                       },2000)
                       });",HomeID, url1), con=connection)
  
    
    system("./phantomjs zillow_home.js")
    
    
    table_Data <- paste("./rproject/",HomeID,".html",sep="") %>% read_html() %>% html_nodes(xpath = "//table") %>% html_text()
    
    Price_history <- table_Data[gregexpr("^DateEvent[aA-zZ | 0-9]*",table_Data[1:length(table_Data)]) != -1]
    datetemp <- unlist(gregexpr("[0-9]{2}/[0-9]+/[0-9]{2}",Price_history))
    r <- 1
    row_details<- character(0)
    rowlength <- length(datetemp)
    
    
    
    if(rowlength > 1){
      for(i in 2 : rowlength){
        row_details[r] <- substr(Price_history,datetemp[i-1],datetemp[i]-1)
        r <- r+1
        if(i == rowlength){
          row_details[r] <- substring(Price_history,datetemp[i])
          row_details[r] <- sub("\\.{3}.*","",row_details[r])
        }
      }
    }else{
      if(rowlength < 1){
        row_details <- NA
      }else{
        rtemp <- substring(Price_history,datetemp)
        row_details <- sub("\\.{3}.*","",rtemp)
      }
    }
    row_details <- gsub("([\\.]|Report).*[\\.$]","",row_details)
    
    x=""
    for(i in 1:length(row_details))
      x=paste(x,row_details[i],";")
    price_hist=x
    
    
    Tax_history <- table_Data[gregexpr("^YearProperty[aA-zZ | 0-9]*",table_Data[1:length(table_Data)]) != -1]
    datetemp <- unlist(gregexpr("[0-9]{4}",Tax_history))
    r <- 1
    tax_details<- character(0)
    rowlength <- length(datetemp)
    
    
    
    if(rowlength > 1){
      for(i in 2 : rowlength){
        tax_details[r] <- substr(Price_history,datetemp[i-1],datetemp[i]-1)
        r <- r+1
        if(i == rowlength){
          tax_details[r] <- substring(Price_history,datetemp[i])
          tax_details[r] <- sub("\\.{3}.*","",tax_details[r])
        }
      }
    }else{
      if(rowlength < 1){
        tax_details <- NA
      }else{
        rtemp <- substring(Price_history,datetemp)
        tax_details <- sub("\\.{3}.*","",rtemp)
      }
    }
    tax_details <- gsub("([\\.]|Report).*[\\.$]","",tax_details)
    
    
    x=""
    for(i in 1:length(tax_details))
      x=paste(x,tax_details[i],";")
    tax_hist=x
    
    
    
    #address
    ad=substr(u,37,(stri_length(u)-11))
    idirect =gregexpr("/",ad)
    ad=substr(ad,1,idirect)
    ad=substr(ad,1,(stri_length(ad)-1))
    ad=gsub("-"," ",ad)
    
    #ZIP Code
    zipb=substr(ad,stri_length(ad)-5,stri_length(ad))
    zipb=as.numeric(zipb)
    
    ##Parcel Id
    idirect =gregexpr("Parcel #: (.*)Zill",Hmid[2])
    page0=regmatches(Hmid[2],idirect)
    Parcel_ID=as.numeric(substr(page0,11,(stri_length(page0)-4)))
    
    
    ##Price/sqft
    Facts= zz %>%  html_nodes(".top-facts") %>% html_text
    Facts[1]
    idirect =gregexpr("Price.sqft: (.*?)[0-9]",Facts[1])
    page0=regmatches(Facts[1],idirect)
    Price_Sqft=substr(page0,12,(stri_length(page0)))
    
    ##Last_Remodel_Year
    idirect =gregexpr("Last_Remodel_Year: (.*?)[0-9]",Facts[1])
    page0=regmatches(Facts[1],idirect)
    Last_Remodel_Year=substr(page0,18,(stri_length(page0)))
    
    
    
    
    ##House_Type
    idirect =gregexpr("House_Type: (.*?)[0-9]",Facts[1])
    page0=regmatches(Facts[1],idirect)
    House_Type=substr(page0,11,(stri_length(page0)))
    
    
    ##Type
    idirect =gregexpr("Townhouse|Condo",Facts[1])
    page0=regmatches(Facts[1],idirect)
    Type=page0
    
    #Lot_size
    idirect =gregexpr("(Facts)Lot: (.*?)[0-9] ",Facts[1])
    page0=regmatches(Facts[1],idirect)
    Facts_lot=substr(page0,10,(stri_length(page0)))
    
    #MLS
    idirect =gregexpr("MLS #: (.*?)[A-za-z]",Facts[1])
    page0=regmatches(Facts[1],idirect)
    MLS=substr(page0,7,(stri_length(page0)-2))
    MLS=as.numeric(MLS)
    
    ##Builtin
    idirect =gregexpr("Built in [0-9]{4}",Facts[1])
    page0=regmatches(Facts[1],idirect)
    Builtin=substr(page0,10,(stri_length(page0)))
    Builtin=as.numeric(Builtin)
    
    
    
    ##Room count
    Ex=zz %>%  html_nodes(".top-facts+ .z-moreless-content") %>% html_text
    idirect =gregexpr("Room count: (.*?)Stories",Ex)
    page0=regmatches(Ex,idirect)
    Room_Count=substr(page0,12,(stri_length(page0)-7))
    Room_Count= as.numeric(Room_Count)
    
    ##Stories
    idirect =gregexpr("Stories: (.*?)Unit",Ex)
    page0=regmatches(Ex,idirect)
    Stories=substr(page0,9,(stri_length(page0)-4))
    Stories= as.numeric(Stories)
    
    ##Beds
    Beds= zz %>%  html_nodes(".addr_bbs") %>% html_text
    b=strsplit(Beds," \"")
    idirect =gregexpr("[0-9]{1-3} ",as.character(b[1]))
    page0=regmatches(as.character(b[1]),idirect)
    bedss=as.numeric(page0)
    
    
    
    #Baths
    idirect =gregexpr("[0-9]{1-3} ",as.character(b[2]))
    page0=regmatches(as.character(b[2]),idirect)
    Baths=as.numeric(page0)
    
    
    
    ##Floors
    idirect =gregexpr("[0-9]{1-3} ",as.character(b[3]))
    page0=regmatches(as.character(b[3]),idirect)
    Floor_size=as.numeric(page0)
    
    ##
    Floor_size=substr(b[3],1,6)
    
    
    Current_Price= zz %>%  html_nodes(".main-row span") %>% html_text
    Current_Price=trimws(Current_Price[1])
    
    
    df=rbind.data.frame(df, data.frame(HomeID,school,URL=u,Current_Price,tax_History=tax_hist,Last_Remodel_Year,House_Type,Price_history=price_hist,Neighborhood,MLS,address=ad,zipb,Parcel_ID,Price_Sqft,Lot_size=Facts_lot,Builtin,Type,Room_Count,Stories,bedss,Baths,Floor_size))
    
  }
  
  
  Sys.sleep(7)
}

write.table(df,"example.txt",append =TRUE, sep="\t")
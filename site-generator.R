
#setwd("/var/www/static_site.com")
source("./libs/plugin-rmarkdown.R")
library(configr)
library(whisker)
library(markdown)
library(xml2)
library(yaml)
#get configration from config
tomlConfig.list <- read.config("config.toml")
structureChanged <- FALSE
#argument for rebuild
#args = commandArgs(trailingOnly=TRUE)
args <- list("build-clean")
if (length(args) > 0) {
  
  if(args[1] == "build-clean"){
    if(dir.exists("./public")){
      buildClean <- TRUE
      unlink("./public/content", recursive=TRUE)
      unlink("./public/static", recursive=TRUE)
      file.remove("./public/index.html")
    }
  }else{
    stop("Argument for regenerate everything not valid use 'build-clean' insted.", call.=FALSE)
  }
}

# path to theme statis files
theme <- paste("./themes/",tomlConfig.list$theme, sep = "")
theme.static.folder <- paste(theme, "/static", sep = "")
#print(theme.static.folder)
#path public site 
public.folder <- "./public"

# create public directory if not exist
dir.create(public.folder, showWarnings = FALSE)

# copy themes static file in public directory
if(!dir.exists(paste(public.folder,"/static"))){
  file.copy(theme.static.folder, public.folder, overwrite = TRUE, recursive=TRUE)
}

if (!file.exists(paste(public.folder,"/index.html", sep = ""))){
  structureChanged <- TRUE
  #create Index file
  markdownIndexOutput <- markDownReader("index.Rmd", page = FALSE, post = FALSE, index = TRUE, blogs = FALSE)
  pageTemplate <- readLines(paste(theme,"/templates/index.mustache", sep = ""))
  
  
  data <- list( siteTitle = tomlConfig.list$title
                , socialMedia = tomlConfig.list$socialMedia
                , dropDownMenu = tomlConfig.list$dropDownMenu
                , logo = tomlConfig.list$params$logo
                , headers = markdownIndexOutput$header
                , content = markdownIndexOutput$body
                , title  = markdownIndexOutput$ptitle
  )
  print("Creating index.html")
  
  writeLines(
    whisker.render(pageTemplate, data),
    paste(public.folder,"/index.html", sep = "")
  )
  
}

# create static pages
pages <- list.files("./content/pages")

# create directory for pages if not exist
dir.create(paste(public.folder,"/content", sep = ""), showWarnings = FALSE)
dir.create(paste(public.folder,"/content/pages", sep = ""), showWarnings = FALSE)

for(i in 1:length(pages)) {
  rawFileName <- strsplit(pages[[i]], "[.]")[[1]][[1]]
  if (!file.exists(paste(public.folder,"/content/pages/",rawFileName, "/index.html", sep = ""))){
    structureChanged <- TRUE
    output <- markDownReader(pages[[i]], page = TRUE, post = FALSE,index = FALSE, blogs=FALSE)
    
    pageTemplate <- readLines(paste(theme,"/templates/page.mustache", sep = ""))
    
    data <- list( siteTitle = tomlConfig.list$title
                  , socialMedia = tomlConfig.list$socialMedia
                  , dropDownMenu = tomlConfig.list$dropDownMenu
                  , logo = tomlConfig.list$params$logo
                  , headers = output$header
                  , content = output$body
                  , title = output$ptitle
    )
    print(paste("Creating page ",rawFileName, sep = ""))
    writeLines(
      whisker.render(pageTemplate, data),
      paste(public.folder,"/content/pages/",rawFileName,"/index.html", sep = ""))
  }
  
}


# create static posts
posts <- list.files("./content/posts")

# create directory for posts if not exist
dir.create(paste(public.folder,"/content", sep = ""), showWarnings = FALSE)
dir.create(paste(public.folder,"/content/posts", sep = ""), showWarnings = FALSE)

for(i in 1:length(posts)) {
  rawFileName <- strsplit(posts[[i]], "[.]")[[1]][[1]]
  if (!file.exists(paste(public.folder,"/content/posts/",rawFileName, "/index.html", sep = ""))){
    structureChanged <- TRUE
    output <- markDownReader(posts[[i]], page = FALSE, post = TRUE,index = FALSE, blogs = FALSE)
    postTemplate <- readLines(paste(theme,"/templates/post.mustache", sep = ""))
    postsYamlHeader <- read_RMD_yaml_headers(paste("./content/posts/",posts[[i]],sep = ""),rawFileName)
    
    data <- list( siteTitle = tomlConfig.list$title
                  , socialMedia = tomlConfig.list$socialMedia
                  , dropDownMenu = tomlConfig.list$dropDownMenu
                  , logo = tomlConfig.list$params$logo
                  , headers = output$header
                  , content = output$body
                  , title = output$ptitle
                  , commentEnable = postsYamlHeader$commentEnable
                  , tags = strsplit(postsYamlHeader$tags,",")[[1]]
    )
    print(paste("Creaing post ", rawFileName, sep = ""))
    writeLines(
      whisker.render(postTemplate, data),
      paste(public.folder,"/content/posts/",rawFileName,"/index.html", sep = ""))
  } 
}

if(structureChanged){
  
  # all posts by title and date
  print("Creating blogs page")
  
  postsRMDs <- list.files("./content/posts")
  allPosts <- data.frame(
    file=character(),
    title=character(),
    date=character(), 
    rawFileName=character(),
    teaser = character(),
    pinned = character(), 
    stringsAsFactors=FALSE,
    tags = character()
  )
  for(i in 1:length(postsRMDs)) {
    rawFileName <- strsplit(postsRMDs[[i]], "[.]")[[1]][[1]]
    postConfig <- read_RMD_yaml_headers(paste("./content/posts/",postsRMDs[[i]], sep = ""), rawFileName)
    #allPosts <- c(allPosts,postConfig )
    allPosts <- rbind(allPosts, as.data.frame( postConfig))
  }
  
  # pinned posts
  pinnedPosts <- allPosts[ which(allPosts$pinned==TRUE), ]
  
  # unpinned posts
  unpinnedPosts <- allPosts[ which(allPosts$pinned==FALSE), ]
  
  #sort by date
  pinnedPosts[] <- lapply(pinnedPosts, as.character)
  pinnedPosts <- pinnedPosts[order(pinnedPosts$date,decreasing = TRUE),]
  tmp.pinned <- split(pinnedPosts, seq(nrow(pinnedPosts)))
  finalPinnedPosts <- list()
  for(s in 1:length(tmp.pinned)){
    finalPinnedPosts[[s]] <- as.list(tmp.pinned[[s]])
    finalPinnedPosts[[s]]$tags <- strsplit(finalPinnedPosts[[s]]$tags, ",")[[1]] 
  }
  
  unpinnedPosts[] <- lapply(unpinnedPosts, as.character)
  
  
  itemPerpage <- 10
  if (file.exists("./content/blogs_list/blogs.Rmd")){
    blogsOutput <- markDownReader("blogs.Rmd", page = FALSE, post = FALSE,index = FALSE, blogs = TRUE)
    blogYaml <- read_RMD_yaml_headers("./content/blogs_list/blogs.Rmd","blogs")
    if(blogYaml$sortby == "title"){
      unpinnedPosts <- unpinnedPosts[order(unpinnedPosts$title,decreasing = FALSE),]
    }else{
      unpinnedPosts <- unpinnedPosts[order(unpinnedPosts$date,decreasing = TRUE),]
    }
    itemPerpage <- blogYaml$perpage_item
  }else{
    blogOutput <- list()
    
  }
  
  
  tmp.unpinned <- split(unpinnedPosts, seq(nrow(unpinnedPosts)))
  finalUnPinnedPosts <- list()
  for(s in 1:length(tmp.unpinned)){
    finalUnPinnedPosts[[s]] <- as.list(tmp.unpinned[[s]])
    finalUnPinnedPosts[[s]]$tags <- strsplit(finalUnPinnedPosts[[s]]$tags, ",")[[1]]
  }
  pager <- FALSE
  if(length(finalUnPinnedPosts) > itemPerpage){
    pager <- TRUE
  }
  
  postTemplate <- readLines(paste(theme,"/templates/posts_list.mustache", sep = ""))
  
  data <- list( siteTitle = tomlConfig.list$title
                , socialMedia = tomlConfig.list$socialMedia
                , dropDownMenu = tomlConfig.list$dropDownMenu
                , logo = tomlConfig.list$params$logo
                , headers = blogsOutput$header
                , content = blogsOutput$body
                , title =   blogsOutput$ptitle
                , perPageItem = itemPerpage
                , pinnedPost = finalPinnedPosts
                , upinnedPost = finalUnPinnedPosts
                , pager = pager
  )
  # create blogs directory if not exist
  dir.create(paste(public.folder,"/content/pages/blogs",sep = ""), showWarnings = FALSE)
  
  writeLines(
    whisker.render(postTemplate, data),
    paste(public.folder,"/content/pages/blogs/index.html", sep = ""))
  
}

if(!structureChanged){
  print("No thing to update.")
}

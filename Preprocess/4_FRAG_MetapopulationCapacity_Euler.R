rm(list=ls())
# library(tidyverse)
library(tictoc)
library(raster)
library(landscapemetrics)
library(parallel)
library(foreach)
library(doParallel)


library(RSpectra)
library(dismo)
library(readxl)
library(sf)
library(stars)
library(terra)

library(Reconnect)


raster0 <- raster("/nfs/nas22.ethz.ch/fs2201/usys_ibz_cr_lab/Yibiao/FRAG/Forest_Extent/2000/00N_060W.tif")

extent_Ras <- extent(raster0)

LON <- extent_Ras[1]
LAT <- extent_Ras[3]

Res <- 1/24
N <- 10/Res

vector1 <- rep(1:N, each = N)
vector2 <- rep(1:N, N)

# Combine into a list of pairs
input_list <- mapply(list, vector1, vector2, SIMPLIFY = FALSE)

num_cores <- 100  # Leave one core free for system processes

# Set up parallel backend to use multicore, if available
cl <- makeCluster(num_cores)
doParallel::registerDoParallel(cl)


setwd("/nfs/nas22.ethz.ch/fs2201/usys_ibz_cr_lab/Yibiao/FRAG/Forest_Extent/2000")
fileList <- list.files()

for(s in 1:length(fileList)){
  rs <- fileList[s]
  

  tic()
  
  results <- foreach(pair = iter(input_list),.packages=(.packages()), .combine = 'rbind') %dopar% {
    
    ###############################
    ### Define functions in use----
    rstosf = function(rs=NULL) {
      ## prepare data: make objects pa (numeric vector of patch areas) and mdist (square matrix of interpatch distances)
      if(class(rs)[1]!="NULL"){
        ## if rs is a raster layer, make an sf object out of it..
        if(class(rs)[1]=="RasterLayer") {
          # either make clumps or be aware that it works only if the raster has 1 and NA..clumps depend on landscapemetrixs, but advantage of unique IDs...
          if(max(values(rs),na.rm=TRUE)==1){
            rs  = rs
            rs[rs<1|!is.finite(rs)] = NA
            rs   = landscapemetrics::get_patches(rs, directions=4, class=1)[[1]][[1]]  ## the landscapemetrics function works only when non-habitat cells are set to NA...
          } # making sure to get habitat raster clumped into different patches
          rs   = stars::st_as_stars(rs) %>% sf::st_as_sf(merge = TRUE)    # if it is a raster, make a shapefile out of it
          rs   = sf::st_make_valid(rs)                        # make sure polygons are correctly extracted
        } # if raster
        names(rs)[1] = "pid"
        return(rs)
      } # !is.null rs
    }
    
    
    costdist_pm = function(x=NULL,resi=NULL,id=NULL,areafun=sf::st_area,
                           trans_fun=function(x) 1/mean(x),neighb=8,
                           dist_type="edge",cost_type="least-cost") {
      
      # x=habitat_raster_crop
      # id="ID"
      # resi=resistance_raster_hr 
      # dist_type = "edge"
      # areafun=sf::st_area
      # trans_fun=function(x) 1/mean(x)
      # neighb=8
      # cost_type="least-cost"
      
      
      ## change resitance raster values depending on the distance type!
      if(dist_type=="edge") {habval = 0} else if(dist_type=="centroid") { habval = 1 }
      
      ## change resistance layer where habitat patches are, use a habitat raster or shapefile - depending on what is provided!
      if(class(x)[1]=="RasterLayer") {
        ## set habitat cells zero in the resistance layer! (Assuming habitat cells have a value >0)
        resi[x>0] = habval
        ## generate an sf object
        sfp   = rstosf(rs=x)
        rm(list="x")
      } else if(class(x)[1]=="sf") {
        sfp = x
        rm(list="x")
        ## extract cells of habitat and set these cells zero in the resistance raster!
        resi2   = resi
        resi2[] = NA
        cells   = raster::extract(resi2, as_Spatial(sfp$geometry),method="simple",cellnumbers=TRUE) %>% unlist
        if(length(cells)>0){
          resi[cells[is.finite(cells)]] = habval
        }
        rm(list="resi2")
      }
      
      ## in case no id is provided by the user, just use the name of the first column
      if(class(id)[1]!="character") {
        if(length(names(sfp)[names(sfp)!="geometry"])>0) {id = names(sfp)[names(sfp)!="geometry"][1]
        } else {sfp = sf::st_sf(pid=1:length(sfp$geometry),geometry=sfp$geometry); id = "pid"}
      } # if class(id)
      
      ## only conduct the following if sfp has more than 1 geometry!
      if(length(sfp$geometry)>1) {
        ## apply gdistance functions
        ## create transition matrix
        tr  = gdistance::transition(x=resi,transitionFunction=trans_fun,directions=neighb)
        ## apply different geocorrection for projected or lat lon rasters
        if(!raster::isLonLat(resi)) {
          trC  = gdistance::geoCorrection(tr, type="c", multpl=FALSE, scl=TRUE)
        } else if(raster::isLonLat(resi)) {
          trC  = gdistance::geoCorrection(tr, type="r", multpl=FALSE, scl=TRUE)
        }
        
        ## gives a sparse matrix (same as distr object above...)
        ## the accuracty of the resistance distance depends on the number of neighbours, the further away the more accurate get measurements...
        if(cost_type=="least-cost"){
          cosdi =  tryCatch({
            result = gdistance::costDistance(trC,as_Spatial(sf::st_centroid(sfp$geometry)))
          },
          error = function(e) {
            message('trycatch-error cosdi')
            print(e)
          }
          )
        } else if(cost_type=="commute-time") {
          cosdi =  tryCatch({
            result = gdistance::commuteDistance(trC,as_Spatial(sf::st_centroid(sfp$geometry)))
          },
          error = function(e) {
            message('trycatch-error cosdi')
            print(e)
          }
          )
        }
        
        ## if gdistance does not work, return NA
        if(class(cosdi)[1]!="simpleError") {
          ## turn into a matrix and add correct row and column names
          cosdi = as.matrix(cosdi)
        } else { cosdi = matrix(NA,nrow=length(sfp[[id]]),ncol=length(sfp[[id]]))}
        # sf has more than one patch
      } else { cosdi = as.matrix(0) }
      ## give names to distance matrix
      colnames(cosdi) = rownames(cosdi) = sfp[[id]]
      ## extract patch area vector
      sfpa = as.numeric(areafun(sfp))
      return(list(pa=sfpa,mdist=cosdi))
    } # function end
    
    
    centr_igraph = function(x=NULL,id=NULL,resi=NULL,pa=NULL,mdist=NULL,
                            weighted=TRUE,cutoffpr=0.01,MST=FALSE,normalized=TRUE,mode=c("undirected"),
                            areafun=sf::st_area,dist_type="edge",
                            cost_type="euclidean", trans_fun=function(x) 1/mean(x),neighb=8,
                            alpha=500,dispfop="negex",savememory=FALSE) {
      
      # x=habitat_raster_crop
      # id="ID"
      # resi=resistance_raster_hr
      # dist_type = "edge"
      # areafun=sf::st_area
      # trans_fun=function(x) 1/mean(x)
      # neighb=8
      # cost_type="least-cost"
      # pa=NULL
      # mdist=NULL
      # weighted=TRUE
      # cutoffpr=0.01
      # MST=FALSE
      # normalized=TRUE
      # mode=c("undirected")
      # alpha=500
      # dispfop="negex"
      # savememory=FALSE
      
      ## in case x is provided: prepare data: make objects pa (numeric vector of patch areas) and mdist (square matrix of interpatch distances)
      if(class(x)[1]!="NULL"){
        
        if(cost_type  %in% c("least-cost","commute-time")) {
          ## get the patch area and cost distance matrix object
          pamd = costdist_pm(x=x,resi=resi,id=id,areafun=areafun,
                             trans_fun=trans_fun,neighb=neighb,
                             dist_type=dist_type,cost_type=cost_type)
          pa    = pamd$pa
          mdist = pamd$mdist
          
        } else if (cost_type == "euclidean") {
          ## get the patch area and euclidean distance matrix object
          pamd = eucdist_pm(x=x,areafun=areafun,dist_type=dist_type)
          pa    = pamd$pa
          mdist = pamd$mdist
        }
        
        ## remove to save memory
        rm(list="pamd")
        
      } # if a habitat raster or shapefile is provided
      
      ## if x is not specified but mdist and pa are, we can directly go ahead:
      if(class(pa)[1]=="numeric" & class(mdist)[1]=="matrix"& length(pa)==dim(mdist)[1] & dim(mdist)[1]==dim(mdist)[2]) {
        
        ## Igraph: specify the graph
        graph = igraph::graph_from_adjacency_matrix(
          adjmatrix= mdist,
          mode = mode,
          weighted = weighted,
          diag = TRUE,
          add.colnames = NULL,
          add.rownames = NA)
        
        ## if the minimum spanning tree argument is on, derive the minimum spanning tree
        ## this can save computing time and highlight central nodes better
        if(MST) {
          ## define weights
          if(weighted){  graph = igraph::mst(graph=graph,weights=edge_attr(graph, 'weight'))
          } else { weights = igraph::mst(graph=graph) }
        }
        
        ## if the cutoff probability is given, remove corresponding edges
        if(!is.null(cutoffpr)) {
          ## define the dispersal function
          dispfun = mkdispfun(option=dispfop)
          ## define cutoff distance for given alpha and dispfun
          cutoffd = bufferdist(alpha=alpha,dispfun=dispfun,prob=cutoffpr)["dist"] %>% as.numeric
          ## remove "isolated" patches
          iso = which(edge_attr(graph, 'weight')>=cutoffd)
          if(length(iso)>0) {graph = delete_edges(graph, iso)}
        } else if(is.null(cutoffpr)){
          cutoffd = -1
        }
        
        ## define weights
        if(weighted){ weights = edge_attr(graph, 'weight') } else { weights = NULL }
        
        ## calculate metrics..
        ### normalized: Logical scalar, whether to calculate the normalized closeness, i.e. the inverse average distance to all reachable vertices.
        ## igraph does have specific functions for large graphs - estimate_betweenness and estimate_closeness, which the manual says are not quadratic in runtime. You define a cutoff, which is the largest path length that will be included in the calculation. Traditionally, betweenness considers paths of any length. Defining a cutoff substantially cuts down the runtime:
        ## https://stackoverflow.com/questions/41753929/how-long-does-it-take-for-igraph-r-to-estimate-network-centrality-measures-for-a
        
        BC = igraph::betweenness(
          graph,
          v = V(graph),
          weights  = weights,
          cutoff   = cutoffd
          #,normalized=normalized
        )
        
        CC = igraph::closeness(
          graph,
          v = V(graph),
          weights = weights,
          cutoff=cutoffd
          #,normalized=normalized
        )
        
        ###JO: multiply by 100 to get larger values..
        CC = CC*100
        
        ### degreee
        ND = igraph::degree(graph=graph,v = V(graph), mode="all", normalized = FALSE)
        
        ### return the graph as well for potential further analyses?
        
        ## return PCnum, PC, and ECA, with total area in m^2..,
        # return(list(BC=BC,CC=CC,ND=ND,graph=graph)) # function end
        return(list(BC=BC,CC=CC,ND=ND,graph=graph)) # function end
        
      } else {
        print("--- error --- pa or mdist do not conform to one of the following ---")
        print('class(pa)[1]=="numeric" & class(mdist)[1]=="matrix"& length(pa)==dim(mdist)[1] & dim(mdist)[1]==dim(mdist)[2]')
      } # else
    }
    
    ########################################
    ### Main code for parallel computing----
    i <- pair[[1]]
    j <- pair[[2]]
    
    setwd("/nfs/nas22.ethz.ch/fs2201/usys_ibz_cr_lab/Yibiao/FRAG/Forest_Extent/2000")
    
    raster0 <- raster(rs)
    
    Resistance <- raster("/cluster/home/yibzou/FGFC/hfp2000_reprojected.tif")
    
    # raster0 <- raster("/cluster/home/yibzou/FRAG/Forest_Extent/2020/00N_060W.tif")
    
    extent_Ras <- extent(raster0)
    
    LON <- extent_Ras[1]
    LAT <- extent_Ras[3]
    
    Res <- 1/24
    N <- 10/Res
    
    lon <- LON + (i-1)*Res
    lat <- LAT + (j-1)*Res
    new_extent <- extent(lon, lon+Res, lat, lat+Res)
    
    MPC_Df_Seg <- data.frame(cbind(longitude=lon, latitude=lat, mpc=0,
                                   mpcdens=0))
    
    tryCatch({
    habitat_raster_crop <- crop(raster0, new_extent)
    
    if(sum(as.matrix(habitat_raster_crop))>0){
    resistance_raster <- crop(Resistance, new_extent)
    
    # Create a high-resolution raster template
    resistance_raster_hr <- raster(new_extent, 
                                   res=0.00025,
                                   crs=projection(resistance_raster))
    resistance_raster_hr <- resample(resistance_raster, resistance_raster_hr, method='bilinear')
    
    # if(j==1){
    #   saveRDS(MPC_Df_Seg, file=paste0("/cluster/home/yibzou/FRAG/Outcome/2024_10_30/",i,"_",j,"_",s,".rds"))
    # }

    # tryCatch({
      costpm = costdist_pm(x=habitat_raster_crop, id="ID", resi=resistance_raster_hr, dist_type = "edge")
      
      if(length(costpm)>0){
        mpcres = MPC_fun(pa=costpm$pa,mdist=costpm$mdist,alpha=300)
        
        if(length(mpcres)>0){
          MPC_Df_Seg <- data.frame(cbind(longitude=lon, latitude=lat, mpc=mpcres$mpc,
                                         mpcdens=mpcres$mpcdens))
        }
      }
    # }, error = function(e){
    #   MPC_Df_Seg <- data.frame(cbind(longitude=lon, latitude=lat, mpc=NA,
    #                                  mpcdens=NA))
    # })
    
    }
    
    }, error = function(e){
      MPC_Df_Seg <- data.frame(cbind(longitude=lon, latitude=lat, mpc=NA,
                                     mpcdens=NA))
    })
  }
  toc()
  
  rs_name <- strsplit(rs, split = "\\.")[[1]][1]
  
  # saveRDS(results, file=paste0("C:/Zeus/ETHz/ETHz_S7/Forest_GFC/Outcome/2024_10_29/frag_df_",rs_name,".rds"))
  
  saveRDS(results, file=paste0("/cluster/home/yibzou/FRAG/Outcome/2024_10_29/mpc_df_Y2000_",rs_name,"_5km.rds"))
  
}

# Stop the parallel cluster
stopCluster(cl)




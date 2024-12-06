#' a data.frame with information about available resources
#' @docType data
#' @note This information was scraped from [scverse spatialdata](https://spatialdata.scverse.org/en/latest/tutorials/notebooks/datasets/README.html) download site on 5 Dec 2024.
"demodata_df"


#' this function consolidates the retrieval and caching and
#' transformation of scverse-curated Zarr archives and 10x-curated
#' Xenium archives
#' @param patt character(1) sufficient to identify a resource in OSN
#' @param cache like BiocFileCache
#' @examples
#' (br2fov <- get_demo_SD("Breast_2fov"))
#' @export
get_demo_SD = function(patt, cache=BiocFileCache::BiocFileCache()) {
#
# Bioconductor's OSN bucket
#
  buckprefix = "https://mghp.osn.xsede.org/bir190004-bucket01"
#
# work on zipped Zarr archives from scverse SpatialData datasets page
#  
  sdfold = "BiocSpatialData"
  
  sdzips = c("mcmicro_io.zip", "merfish.zarr.zip", 
  "mibitof.zip", "steinbock_io.zip", 
  "visium_associated_xenium_io_aligned.zip", "visium_hd_3.0.0_io.zip",
  "xenium_rep1_io_aligned.zip",
  "xenium_rep2_io_aligned.zip")
  
  sdurls = paste(buckprefix, sdfold, sdzips, sep="/")
#
# work on zipped Xenium minimal outputs, retrieved and zipped in OSN
# these must be expanded and processed with use_sdio
#  
  xdfold = "BiocXenDemo"
  
  xdzips = c("Xenium_V1_human_Breast_2fov_outs.zip",
      "Xenium_V1_human_Lung_2fov_outs.zip")
#
# collect names of all zip files
#
  allz = c(sdzips, xdzips)
# 
# build a tibble with all relevant information
#
  xdurls = paste(buckprefix, xdfold, xdzips, sep="/")
  
  allurls = c(sdurls, xdurls)
  
  ca = BiocFileCache::BiocFileCache()
  chk = lapply(allurls, function(x) BiocFileCache::bfcquery(ca, x))
  chkdf = do.call(rbind, chk)
  ind = grep(patt, chkdf$rname)
  nupatt = "pattern does not uniquely identify a resource, please be more specific"
  if (length(ind)>1) stop(nupatt)
  if (length(ind) == 0) {
    chkxen = grep(patt, xdzips)
    if (length(chkxen)>1) stop(nupatt)
    if (length(chkxen)==0) {   # add a zipped zarr, cache, and parse
     zipind = grep(patt, sdzips)
     zipname = sdzips[zipind]
     message(sprintf("caching %s", zipname))
     fpath = sdurls[zipind]
     loc = BiocFileCache::bfcadd(cache, rname=zipname, 
         fpath=fpath, rtype="web")
     td = tempfile()
     dir.create(td)
     unzip(loc, exdir=td)
     return(SpatialData::readSpatialData(dir(td, full.names=TRUE)))
     }   # end zipped zarr, now retrieve Xenium, run use_sdio, then read
    zipname = xdzips[chkxen]
    message(sprintf("caching %s", zipname))
    fpath = xdurls[chkxen]
    preloc = BiocFileCache::bfcadd(cache, rname=zipname, 
         fpath=fpath, rtype="web")
    td = tempfile()
    dir.create(td)
    unzip(preloc, exdir=td)  # manufacturer output
    target = tempfile()
    SpatialData::use_sdio("xenium", srcdir=td, dest=target) # zarr in target
    return(SpatialData::readSpatialData(target))
   }
# so a single pattern has hit, and it is in cache
   if (chkdf[ind,]$rname %in% xdzips) {  # it is a Xenium 10x output resource
     preloc = chkdf[ind,]$rpath
   td = tempfile()
   dir.create(td)
   unzip(preloc, exdir=td)  # manufacturer output
   target = tempfile()
   SpatialData::use_sdio("xenium", srcdir=td, dest=target) # zarr in target
   return(SpatialData::readSpatialData(target))
   }
   loc = chkdf[ind,]$rpath
   td = tempfile()
   dir.create(td)
   unzip(loc, exdir=td)
   SpatialData::readSpatialData(dir(td, full.names=TRUE))
}

       

  

  
  

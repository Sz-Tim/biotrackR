#' Generate properties for biotracker
#'
#' @param properties_file_path filename for properties file; if NULL, character vector is returned only
#' @param coordOS Use OSGB1936 (EPSG 27700)? 'false' = WGS84 (EPSG 4326)
#' @param hfDir0 Primary data directory
#' @param hfDirPrefix0 Primary data directory prefix
#' @param hfDirSuffix0 Primary data directory suffix
#' @param mesh0 Primary mesh file
#' @param meshType0 Primary mesh type
#' @param hfFilePrefix0 Primary hydrodynamic file prefix (e.g., 'westcoms2')
#' @param hfDir1 Secondary data directory
#' @param hfDirPrefix1 Secondary data directory prefix
#' @param hfDirSuffix1 Secondary data directory suffix
#' @param mesh1 Secondary mesh file
#' @param meshType1 Secondary mesh type
#' @param hfFilePrefix1 Secondary hydrodynamic file prefix (e.g., 'etive28')
#' @param hfDir2 SWAN wave model data directory
#' @param hfDirPrefix2 SWAN wave model data directory prefix
#' @param hfDirSuffix2 SWAN wave model data directory suffix
#' @param hfFilePrefix2 SWAN wave model file prefix
#' @param sitefile Site location file (csv with headers)
#' @param sitefileEnd Site end location file (csv with headers)
#' @param siteDensityPath Site lice density path (csv with headers; row = site, col = date)
#' @param daylightPath File with daylight hours (row = day, column 1 = sunrise hour, column 2 = sunset hour)
#' @param verboseSetUp Additional info
#' @param start_ymd Start date YYYYMMDD
#' @param numberOfDays Number of days to simulate
#' @param checkOpenBoundaries Check for exits through open boundaries?
#' @param openBoundaryThresh Threshold for boundary exits (m)
#' @param duplicateLastDay TRUE; duplicates final day of hydrodynamic files
#' @param recordsPerFile1 Records per hydrdynamic file
#' @param dt Time step
#' @param maxDepth Limit for particle depth (m, positive = deeper)
#' @param parallelThreads Number of threads
#' @param parallelThreadsHD Number of cores for reading HD .nc files
#' @param releaseScenario 1 = consistent release across time steps
#' @param releaseInterval Interval (hours) for releaseScenario==1
#' @param nparts Particles per site per release
#' @param startDepth Particle starting depth
#' @param stepsPerStep RK4 steps per timestep
#' @param variableDh Include variable horizontal diffusion? Uses WeStCOMS viscofh field
#' @param variableDhV Include variable vertical diffusion? Uses WeStCOMS kh field
#' @param D_h Horizontal diffusion coefficient
#' @param D_hVert Vertical diffusion coefficient
#' @param stokesDrift Include stokes drift? Requires setting \code{hf*2} arguments providing Hsig, Dir, and Tm01
#' @param salinityThreshCopepodidMin Lower salinity threshold for sinking (all sink)
#' @param salinityThreshCopepodidMax Upper salinity threshold for sinking (none sink)
#' @param salinityThreshNaupliusMin Lower salinity threshold for sinking (all sink)
#' @param salinityThreshNaupliusMax Upper salinity threshold for sinking (none sink)
#' @param swimLightLevel Should particles swim upward if light is sufficient
#' @param lightThreshCopepodid Light level (umol/m2/s) stimulating upward swimming
#' @param lightThreshNauplius Light level (umol/m2/s) stimulating upward swimming
#' @param swimUpSpeedMean Mean upward swim speed (m/s)
#' @param swimUpSpeedStd SD for upward swim speed (m/s)
#' @param swimUpSpeedCopepodidMean Mean upward swim speed (m/s); ignored if swimUpSpeedMean != NULL
#' @param swimUpSpeedCopepodidStd SD for upward swim speed (m/s); ignored if swimUpSpeedStd != NULL
#' @param swimUpSpeedNaupliusMean Mean upward swim speed (m/s); ignored if swimUpSpeedMean != NULL
#' @param swimUpSpeedNaupliusStd SD for upward swim speed (m/s); ignored if swimUpSpeedStd != NULL
#' @param swimDownSpeedMean Mean downward swimming speed (m/s)
#' @param swimDownSpeedStd SD for downward swimming speed (m/s)
#' @param swimDownSpeedCopepodidMean Mean downward swimming speed (m/s); ignored if swimDownSpeedMean != NULL
#' @param swimDownSpeedCopepodidStd SD for downward swimming speed (m/s); ignored if swimDownSpeedStd != NULL
#' @param swimDownSpeedNaupliusMean Mean downward swimming speed (m/s); ignored if swimDownSpeedMean != NULL
#' @param swimDownSpeedNaupliusStd SD for downward swimming speed (m/s); ignored if swimDownSpeedStd != NULL
#' @param passiveSinkingIntercept Intercept for passive sinking based on salinity
#' @param passiveSinkingSlope Slope for passive sinking based on salinity
#' @param eggTemp_fn Function for temperature-dependent egg production; 'constant', 'linear', 'quadratic', or 'logistic'
#' @param eggTemp_b Parameters for temperature-dependent egg production; comma-separated string
#' @param mortSal_fn Function for salinity-dependent mortality; 'constant', 'quadratic', or 'logistic'
#' @param mortSal_b Parameters for salinity-dependent mortality; comma-separated string
#' @param viabletime Time at which particles become viable (h)
#' @param maxParticleAge Time at which particles die (h)
#' @param viableDegreeDays Degree days at which particles become viable
#' @param maxDegreeDays Degree days at which particles die
#' @param recordImmature Record immature particles in addition to mature? Affects psteps & vertDistr
#' @param recordPsteps Record particle steps? Matrix with density/element
#' @param splitPsteps Split Psteps by release location?
#' @param pstepsInterval Psteps recording frequency (h)
#' @param pstepsMaxDepth Maximum depth for tallying a particle (m)
#' @param recordVertDistr Record vertical distribution of particles per element?
#' @param vertDistrInterval Vert distribution recording frequency (h)
#' @param vertDistrMax Maximum depth to record (\code{0:vertDistrMax}) with all deeper particles lumped into last group
#' @param recordMovement Record movement details for sample of particles?
#' @param recordElemActivity Record overall element/hourly particle activity?
#' @param recordConnectivity Record connectivity (mature particles only) between sites?
#' @param connectImmature Record connectivity for immature particles between sites?
#' @param connectDepth1_min Minimum depth (m) of particles connectivity (depth range 1); default = 0
#' @param connectDepth1_max Maximum depth (m) of particles connectivity (depth range 1); default = 10000
#' @param connectDepth2_min Minimum depth (m) of particles connectivity (depth range 2); default (10000) causes biotracker to ignore depth range 2
#' @param connectDepth2_max Maximum depth (m) of particles connectivity (depth range 2); default (10000) causes biotracker to ignore depth range 2
#' @param connectivityInterval Connectivity recording frequency (h)
#' @param connectivityThresh Distance threshold for connectivity recording
#' @param recordLocations Record hourly particle locations?
#' @param recordArrivals Record particle arrivals to sites?
#'
#' @return Character vector with all properties; if properties_file_path is set, output is written to that file (overwriting any existing file)
#' @export
#'
set_biotracker_properties <- function(
    properties_file_path=NULL,
    coordOS="true",
    mesh0="/home/sa04ts/hydro/meshes/WeStCOMS2_mesh.nc",
    meshType0="FVCOM",
    hfFilePrefix0="westcoms2",
    hfDir0="/home/sa04ts/hydro/WeStCOMS2/Archive/",
    hfDirPrefix0="netcdf_",
    hfDirSuffix0="",
    mesh1="",
    meshType1="",
    hfFilePrefix1="",
    hfDir1="",
    hfDirPrefix1="",
    hfDirSuffix1="",
    hfFilePrefix2="swan",
    hfDir2="/home/sa04ts/hydro/WeStCOMS2/Archive_daily/",
    hfDirPrefix2="netcdf_",
    hfDirSuffix2="F",
    sitefile="../../data/farm_sites.csv",
    sitefileEnd="../../data/farm_sites.csv",
    siteDensityPath="",
    daylightPath="",
    verboseSetUp="false",
    start_ymd=20190401,
    numberOfDays=7,
    checkOpenBoundaries="false",
    openBoundaryThresh=500,
    duplicateLastDay="true",
    recordsPerFile1=25,
    dt=3600,
    maxDepth=10000,
    parallelThreads=4,
    parallelThreadsHD=4,
    releaseScenario=1,
    releaseInterval=1,
    nparts=1,
    startDepth=1,
    fixDepth="false",
    stepsPerStep=30,
    variableDh="false",
    variableDhV="false",
    D_h=0.1,
    D_hVert=0.001,
    stokesDrift="false",
    salinityThreshCopepodidMin=23,
    salinityThreshCopepodidMax=31,
    salinityThreshNaupliusMin=23,
    salinityThreshNaupliusMax=31,
    swimLightLevel="true",
    lightThreshCopepodid=2.06e-5,
    lightThreshNauplius=0.392,
    swimUpSpeedMean=NULL,
    swimUpSpeedStd=NULL,
    swimUpSpeedCopepodidMean=-0.0005,
    swimUpSpeedCopepodidStd=0.0001,
    swimUpSpeedNaupliusMean=-0.00025,
    swimUpSpeedNaupliusStd=0.00005,
    swimDownSpeedMean=NULL,
    swimDownSpeedStd=NULL,
    swimDownSpeedCopepodidMean=0.001,
    swimDownSpeedCopepodidStd=0.0002,
    swimDownSpeedNaupliusMean=0.001,
    swimDownSpeedNaupliusStd=0.0002,
    passiveSinkingIntercept=0.001527,
    passiveSinkingSlope=-0.0000168,
    eggTemp_fn="constant",
    eggTemp_b="",
    mortSal_fn="constant",
    mortSal_b="",
    viabletime=-1,
    maxParticleAge=-1,
    viableDegreeDays=40,
    maxDegreeDays=150,
    recordImmature="false",
    recordPsteps="false",
    splitPsteps="false",
    pstepsInterval=168,
    pstepsMaxDepth=10000,
    recordVertDistr="false",
    vertDistrInterval=1,
    vertDistrMax=20,
    recordMovement="false",
    recordElemActivity="false",
    recordConnectivity="true",
    connectImmature="false",
    connectDepth1_min=0,
    connectDepth1_max=10000,
    connectDepth2_min=10000,
    connectDepth2_max=10000,
    connectivityInterval=24,
    connectivityThresh=100,
    recordLocations="false",
    recordArrivals="false"
) {
  params <- c(
    coordOS=coordOS,
    mesh0=mesh0,
    meshType0=meshType0,
    hfFilePrefix0=hfFilePrefix0,
    hfDir0=hfDir0,
    hfDirPrefix0=hfDirPrefix0,
    hfDirSuffix0=hfDirSuffix0,
    mesh1=mesh1,
    meshType1=meshType1,
    hfFilePrefix1=hfFilePrefix1,
    hfDir1=hfDir1,
    hfDirPrefix1=hfDirPrefix1,
    hfDirSuffix1=hfDirSuffix1,
    hfFilePrefix2=hfFilePrefix2,
    hfDir2=hfDir2,
    hfDirPrefix2=hfDirPrefix2,
    hfDirSuffix2=hfDirSuffix2,
    sitefile=sitefile,
    sitefileEnd=sitefileEnd,
    siteDensityPath=siteDensityPath,
    daylightPath=daylightPath,
    verboseSetUp=verboseSetUp,
    start_ymd=start_ymd,
    numberOfDays=numberOfDays,
    checkOpenBoundaries=checkOpenBoundaries,
    openBoundaryThresh=openBoundaryThresh,
    duplicateLastDay=duplicateLastDay,
    recordsPerFile1=recordsPerFile1,
    dt=dt,
    maxDepth=maxDepth,
    parallelThreads=parallelThreads,
    parallelThreadsHD=parallelThreadsHD,
    releaseScenario=releaseScenario,
    releaseInterval=releaseInterval,
    nparts=nparts,
    startDepth=startDepth,
    fixDepth=fixDepth,
    stepsPerStep=stepsPerStep,
    variableDh=variableDh,
    variableDhV=variableDhV,
    D_h=D_h,
    D_hVert=D_hVert,
    stokesDrift=stokesDrift,
    salinityThreshCopepodidMin=salinityThreshCopepodidMin,
    salinityThreshCopepodidMax=salinityThreshCopepodidMax,
    salinityThreshNaupliusMin=salinityThreshNaupliusMin,
    salinityThreshNaupliusMax=salinityThreshNaupliusMax,
    swimLightLevel=swimLightLevel,
    lightThreshCopepodid=lightThreshCopepodid,
    lightThreshNauplius=lightThreshNauplius,
    swimUpSpeedMean=swimUpSpeedMean,
    swimUpSpeedStd=swimUpSpeedStd,
    swimUpSpeedCopepodidMean=ifelse(is.null(swimUpSpeedMean), swimUpSpeedCopepodidMean, swimUpSpeedMean),
    swimUpSpeedCopepodidStd=ifelse(is.null(swimUpSpeedStd), swimUpSpeedCopepodidStd, swimUpSpeedStd),
    swimUpSpeedNaupliusMean=ifelse(is.null(swimUpSpeedMean), swimUpSpeedNaupliusMean, swimUpSpeedMean),
    swimUpSpeedNaupliusStd=ifelse(is.null(swimUpSpeedStd), swimUpSpeedNaupliusStd, swimUpSpeedStd),
    swimDownSpeedMean=swimDownSpeedMean,
    swimDownSpeedStd=swimDownSpeedStd,
    swimDownSpeedCopepodidMean=ifelse(is.null(swimDownSpeedMean), swimDownSpeedCopepodidMean, swimDownSpeedMean),
    swimDownSpeedCopepodidStd=ifelse(is.null(swimDownSpeedStd), swimDownSpeedCopepodidStd, swimDownSpeedStd),
    swimDownSpeedNaupliusMean=ifelse(is.null(swimDownSpeedMean), swimDownSpeedNaupliusMean, swimDownSpeedMean),
    swimDownSpeedNaupliusStd=ifelse(is.null(swimDownSpeedStd), swimDownSpeedNaupliusStd, swimDownSpeedStd),
    passiveSinkingIntercept=passiveSinkingIntercept,
    passiveSinkingSlope=passiveSinkingSlope,
    eggTemp_fn=eggTemp_fn,
    eggTemp_b=eggTemp_b,
    mortSal_fn=mortSal_fn,
    mortSal_b=mortSal_b,
    viabletime=viabletime,
    maxParticleAge=maxParticleAge,
    viableDegreeDays=viableDegreeDays,
    maxDegreeDays=maxDegreeDays,
    recordImmature=recordImmature,
    recordPsteps=recordPsteps,
    splitPsteps=splitPsteps,
    pstepsInterval=pstepsInterval,
    pstepsMaxDepth=pstepsMaxDepth,
    recordVertDistr=recordVertDistr,
    vertDistrInterval=vertDistrInterval,
    vertDistrMax=vertDistrMax,
    recordMovement=recordMovement,
    recordElemActivity=recordElemActivity,
    recordConnectivity=recordConnectivity,
    connectImmature=connectImmature,
    connectDepth1_min=connectDepth1_min,
    connectDepth1_max=connectDepth1_max,
    connectDepth2_min=connectDepth2_min,
    connectDepth2_max=connectDepth2_max,
    connectivityInterval=connectivityInterval,
    connectivityThresh=connectivityThresh,
    recordLocations=recordLocations,
    recordArrivals=recordArrivals
  )
  if(params["eggTemp_b"]=="") params <- params[-which(names(params)=="eggTemp_b")]
  if(params["mortSal_b"]=="") params <- params[-which(names(params)=="mortSal_b")]

  properties_out <- paste(names(params), params, sep="=", collapse="\n")

  if(!is.null(properties_file_path)) {
    cat(properties_out |>
          # correct Windows directories
          str_replace_all("\\\\", "\\\\\\\\") |>
          str_replace_all("\\ ", "\\\\\\\\ "),
        "\n", file=properties_file_path)
  }

  return(properties_out)
}

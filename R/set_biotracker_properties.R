#' Generate properties for biotracker
#'
#' @param properties_file_path filename for properties file; if NULL, character vector is returned only
#' @param destinationDirectory Output directory
#' @param coordOS Use OSGB1936 (EPSG 27700)? 'false' = WGS84 (EPSG 4326)
#' @param datadir Primary data directory
#' @param datadirPrefix Primary data file prefix
#' @param datadirSuffix Primary data file suffix
#' @param mesh1 Primary mesh file
#' @param mesh1Type Primary mesh type
#' @param location Primary domain
#' @param minchVersion Primary mesh WeStCOMS version
#' @param datadir2 Secondary data directory
#' @param datadir2Prefix Secondary data file prefix
#' @param datadir2Suffix Secondary data file suffix
#' @param mesh2 Secondary mesh file
#' @param mesh2Type Secondary mesh type
#' @param location2 Secondary domain
#' @param minchVersion2 Secondary mesh WeStCOMS version
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
#' @param releaseScenario 1 = consistent release across time steps
#' @param releaseInterval Interval (hours) for releaseScenario==1
#' @param nparts Particles per site per release
#' @param setStartDepth Set particle starting depth?
#' @param startDepth Particle starting depth
#' @param stepsPerStep RK4 steps per timestep
#' @param variableDh Include variable horizontal diffusion? Uses WeStCOMS viscofh field
#' @param variableDhV Include variable vertical diffusion? Uses WeStCOMS kh field
#' @param D_h Horizontal diffusion coefficient
#' @param D_hVert Vertical diffusion coefficient
#' @param salinityThreshMin Lower salinity threshold for sinking (all sink)
#' @param salinityThreshMax Upper salinity threshold for sinking (none sink)
#' @param mortalityRate Mortality rate if salinityMort=false
#' @param salinityMort Calculate mortality based on salinity?
#' @param swimLightLevel Should particles swim upward if light is sufficient?
#' @param vertSwimSpeedMean Mean upward swim speed (m/s)
#' @param vertSwimSpeedStd SD for upward swim speed (m/s)
#' @param vertSwimSpeedCopepodidMean Mean upward swim speed (m/s); ignored if vertSwimSpeedMean != NULL
#' @param vertSwimSpeedCopepodidStd SD for upward swim speed (m/s); ignored if vertSwimSpeedStd != NULL
#' @param vertSwimSpeedNaupliusMean Mean upward swim speed (m/s); ignored if vertSwimSpeedMean != NULL
#' @param vertSwimSpeedNaupliusStd SD for upward swim speed (m/s); ignored if vertSwimSpeedStd != NULL
#' @param sinkingRateMean Mean downward swimming speed (m/s)
#' @param sinkingRateStd SD for downward swimming speed (m/s)
#' @param sinkingRateCopepodidMean Mean downward swimming speed (m/s); ignored if sinkingRateMean != NULL
#' @param sinkingRateCopepodidStd SD for downward swimming speed (m/s); ignored if sinkingRateStd != NULL
#' @param sinkingRateNaupliusMean Mean downward swimming speed (m/s); ignored if sinkingRateMean != NULL
#' @param sinkingRateNaupliusStd SD for downward swimming speed (m/s); ignored if sinkingRateStd != NULL
#' @param eggTemp_b0 Intercept for temperature-dependent egg production
#' @param eggTemp_b1 Slope for temperature-dependent egg production
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
    datadir="/media/archiver/common/sa01da-work/WeStCOMS2/Archive/",
    datadirPrefix="netcdf_",
    datadirSuffix="",
    mesh1="/home/sa04ts/FVCOM_meshes/WeStCOMS2_mesh.nc",
    mesh1Type="FVCOM",
    location="westcoms",
    minchVersion=2,
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
    releaseScenario=1,
    releaseInterval=1,
    nparts=1,
    setStartDepth="true",
    startDepth=1,
    fixDepth="false",
    stepsPerStep=30,
    variableDh="false",
    variableDhV="false",
    D_h=0.1,
    D_hVert=0.001,
    salinityThreshMin=23,
    salinityThreshMax=31,
    mortalityRate=0.01,
    salinityMort="true",
    swimLightLevel="true",
    vertSwimSpeedMean=NULL,
    vertSwimSpeedStd=NULL,
    vertSwimSpeedCopepodidMean=-0.0005,
    vertSwimSpeedCopepodidStd=0.0001,
    vertSwimSpeedNaupliusMean=-0.00025,
    vertSwimSpeedNaupliusStd=0.00005,
    sinkingRateMean=NULL,
    sinkingRateStd=NULL,
    sinkingRateCopepodidMean=0.001,
    sinkingRateCopepodidStd=0.0002,
    sinkingRateNaupliusMean=0.001,
    sinkingRateNaupliusStd=0.0002,
    eggTemp_b0=28.2,
    eggTemp_b1=0,
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
    recordElemActivity="true",
    recordConnectivity="true",
    connectivityInterval=24,
    connectivityThresh=100,
    recordLocations="false",
    recordArrivals="false"
) {
  params <- c(
    coordOS=coordOS,
    datadir=datadir,
    datadirPrefix=datadirPrefix,
    datadirSuffix=datadirSuffix,
    mesh1=mesh1,
    mesh1Type=mesh1Type,
    location=location,
    minchVersion=minchVersion,
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
    releaseScenario=releaseScenario,
    releaseInterval=releaseInterval,
    nparts=nparts,
    setStartDepth=setStartDepth,
    startDepth=startDepth,
    fixDepth=fixDepth,
    stepsPerStep=stepsPerStep,
    variableDh=variableDh,
    variableDhV=variableDhV,
    D_h=D_h,
    D_hVert=D_hVert,
    salinityThreshMin=salinityThreshMin,
    salinityThreshMax=salinityThreshMax,
    mortalityRate=mortalityRate,
    salinityMort=salinityMort,
    swimLightLevel=swimLightLevel,
    vertSwimSpeedMean=vertSwimSpeedMean,
    vertSwimSpeedStd=vertSwimSpeedStd,
    vertSwimSpeedCopepodidMean=ifelse(is.null(vertSwimSpeedMean), vertSwimSpeedCopepodidMean, vertSwimSpeedMean),
    vertSwimSpeedCopepodidStd=ifelse(is.null(vertSwimSpeedStd), vertSwimSpeedCopepodidStd, vertSwimSpeedStd),
    vertSwimSpeedNaupliusMean=-ifelse(is.null(vertSwimSpeedMean), vertSwimSpeedNaupliusMean, vertSwimSpeedMean),
    vertSwimSpeedNaupliusStd=ifelse(is.null(vertSwimSpeedStd), vertSwimSpeedNaupliusStd, vertSwimSpeedStd),
    sinkingRateMean=sinkingRateMean,
    sinkingRateStd=sinkingRateStd,
    sinkingRateCopepodidMean=ifelse(is.null(sinkingRateMean), sinkingRateCopepodidMean, sinkingRateMean),
    sinkingRateCopepodidStd=ifelse(is.null(sinkingRateStd), sinkingRateCopepodidStd, sinkingRateStd),
    sinkingRateNaupliusMean=ifelse(is.null(sinkingRateMean), sinkingRateNaupliusMean, sinkingRateMean),
    sinkingRateNaupliusStd=ifelse(is.null(sinkingRateStd), sinkingRateNaupliusStd, sinkingRateStd),
    eggTemp_b0=eggTemp_b0,
    eggTemp_b1=eggTemp_b1,
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
    connectivityInterval=connectivityInterval,
    connectivityThresh=connectivityThresh,
    recordLocations=recordLocations,
    recordArrivals=recordArrivals
  )

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

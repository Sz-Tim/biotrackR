% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/set_biotracker_properties.R
\name{set_biotracker_properties}
\alias{set_biotracker_properties}
\title{Generate properties for biotracker}
\usage{
set_biotracker_properties(
  properties_file_path = NULL,
  coordOS = "true",
  datadir = "/media/archiver/common/sa01da-work/WeStCOMS2/Archive/",
  datadirPrefix = "netcdf_",
  datadirSuffix = "",
  mesh1 = "/home/sa04ts/FVCOM_meshes/WeStCOMS2_mesh.nc",
  mesh1Type = "FVCOM",
  location = "westcoms",
  minchVersion = 2,
  sitefile = "../../data/farm_sites.csv",
  sitefileEnd = "../../data/farm_sites.csv",
  siteDensityPath = "",
  daylightPath = "",
  verboseSetUp = "false",
  start_ymd = 20190401,
  numberOfDays = 7,
  checkOpenBoundaries = "false",
  openBoundaryThresh = 500,
  duplicateLastDay = "true",
  recordsPerFile1 = 25,
  dt = 3600,
  maxDepth = 10000,
  parallelThreads = 4,
  releaseScenario = 1,
  releaseInterval = 1,
  nparts = 1,
  setStartDepth = "true",
  startDepth = 1,
  fixDepth = "false",
  stepsPerStep = 30,
  variableDh = "false",
  variableDhV = "false",
  D_h = 0.1,
  D_hVert = 0.001,
  salinityThreshMin = 23,
  salinityThreshMax = 31,
  mortalityRate = 0.01,
  salinityMort = "true",
  swimLightLevel = "true",
  lightLevelCopepodid = 2.06e-05,
  lightLevelNauplius = 0.392,
  swimUpSpeedMean = NULL,
  swimUpSpeedStd = NULL,
  swimUpSpeedCopepodidMean = -5e-04,
  swimUpSpeedCopepodidStd = 1e-04,
  swimUpSpeedNaupliusMean = -0.00025,
  swimUpSpeedNaupliusStd = 5e-05,
  swimDownSpeedMean = NULL,
  swimDownSpeedStd = NULL,
  swimDownSpeedCopepodidMean = 0.001,
  swimDownSpeedCopepodidStd = 2e-04,
  swimDownSpeedNaupliusMean = 0.001,
  swimDownSpeedNaupliusStd = 2e-04,
  passiveSinkingIntercept = 0.001527,
  passiveSinkingSlope = -1.68e-05,
  eggTemp_b0 = 28.2,
  eggTemp_b1 = 0,
  viabletime = -1,
  maxParticleAge = -1,
  viableDegreeDays = 40,
  maxDegreeDays = 150,
  recordImmature = "false",
  recordPsteps = "false",
  splitPsteps = "false",
  pstepsInterval = 168,
  pstepsMaxDepth = 10000,
  recordVertDistr = "false",
  vertDistrInterval = 1,
  vertDistrMax = 20,
  recordMovement = "false",
  recordElemActivity = "true",
  recordConnectivity = "true",
  connectImmature = "false",
  connectDepth1_min = 0,
  connectDepth1_max = 10000,
  connectDepth2_min = 10000,
  connectDepth2_max = 10000,
  connectivityInterval = 24,
  connectivityThresh = 100,
  recordLocations = "false",
  recordArrivals = "false"
)
}
\arguments{
\item{properties_file_path}{filename for properties file; if NULL, character vector is returned only}

\item{coordOS}{Use OSGB1936 (EPSG 27700)? 'false' = WGS84 (EPSG 4326)}

\item{datadir}{Primary data directory}

\item{datadirPrefix}{Primary data file prefix}

\item{datadirSuffix}{Primary data file suffix}

\item{mesh1}{Primary mesh file}

\item{mesh1Type}{Primary mesh type}

\item{location}{Primary domain}

\item{minchVersion}{Primary mesh WeStCOMS version}

\item{sitefile}{Site location file (csv with headers)}

\item{sitefileEnd}{Site end location file (csv with headers)}

\item{siteDensityPath}{Site lice density path (csv with headers; row = site, col = date)}

\item{daylightPath}{File with daylight hours (row = day, column 1 = sunrise hour, column 2 = sunset hour)}

\item{verboseSetUp}{Additional info}

\item{start_ymd}{Start date YYYYMMDD}

\item{numberOfDays}{Number of days to simulate}

\item{checkOpenBoundaries}{Check for exits through open boundaries?}

\item{openBoundaryThresh}{Threshold for boundary exits (m)}

\item{duplicateLastDay}{TRUE; duplicates final day of hydrodynamic files}

\item{recordsPerFile1}{Records per hydrdynamic file}

\item{dt}{Time step}

\item{maxDepth}{Limit for particle depth (m, positive = deeper)}

\item{parallelThreads}{Number of threads}

\item{releaseScenario}{1 = consistent release across time steps}

\item{releaseInterval}{Interval (hours) for releaseScenario==1}

\item{nparts}{Particles per site per release}

\item{setStartDepth}{Set particle starting depth?}

\item{startDepth}{Particle starting depth}

\item{stepsPerStep}{RK4 steps per timestep}

\item{variableDh}{Include variable horizontal diffusion? Uses WeStCOMS viscofh field}

\item{variableDhV}{Include variable vertical diffusion? Uses WeStCOMS kh field}

\item{D_h}{Horizontal diffusion coefficient}

\item{D_hVert}{Vertical diffusion coefficient}

\item{salinityThreshMin}{Lower salinity threshold for sinking (all sink)}

\item{salinityThreshMax}{Upper salinity threshold for sinking (none sink)}

\item{mortalityRate}{Mortality rate if salinityMort=false}

\item{salinityMort}{Calculate mortality based on salinity?}

\item{swimLightLevel}{Should particles swim upward if light is sufficient}

\item{lightLevelCopepodid}{Light level (umol/m2/s) stimulating upward swimming}

\item{lightLevelNauplius}{Light level (umol/m2/s) stimulating upward swimming}

\item{swimUpSpeedMean}{Mean upward swim speed (m/s)}

\item{swimUpSpeedStd}{SD for upward swim speed (m/s)}

\item{swimUpSpeedCopepodidMean}{Mean upward swim speed (m/s); ignored if swimUpSpeedMean != NULL}

\item{swimUpSpeedCopepodidStd}{SD for upward swim speed (m/s); ignored if swimUpSpeedStd != NULL}

\item{swimUpSpeedNaupliusMean}{Mean upward swim speed (m/s); ignored if swimUpSpeedMean != NULL}

\item{swimUpSpeedNaupliusStd}{SD for upward swim speed (m/s); ignored if swimUpSpeedStd != NULL}

\item{swimDownSpeedMean}{Mean downward swimming speed (m/s)}

\item{swimDownSpeedStd}{SD for downward swimming speed (m/s)}

\item{swimDownSpeedCopepodidMean}{Mean downward swimming speed (m/s); ignored if swimDownSpeedMean != NULL}

\item{swimDownSpeedCopepodidStd}{SD for downward swimming speed (m/s); ignored if swimDownSpeedStd != NULL}

\item{swimDownSpeedNaupliusMean}{Mean downward swimming speed (m/s); ignored if swimDownSpeedMean != NULL}

\item{swimDownSpeedNaupliusStd}{SD for downward swimming speed (m/s); ignored if swimDownSpeedStd != NULL}

\item{passiveSinkingIntercept}{Intercept for passive sinking based on salinity}

\item{passiveSinkingSlope}{Slope for passive sinking based on salinity}

\item{eggTemp_b0}{Intercept for temperature-dependent egg production}

\item{eggTemp_b1}{Slope for temperature-dependent egg production}

\item{viabletime}{Time at which particles become viable (h)}

\item{maxParticleAge}{Time at which particles die (h)}

\item{viableDegreeDays}{Degree days at which particles become viable}

\item{maxDegreeDays}{Degree days at which particles die}

\item{recordImmature}{Record immature particles in addition to mature? Affects psteps & vertDistr}

\item{recordPsteps}{Record particle steps? Matrix with density/element}

\item{splitPsteps}{Split Psteps by release location?}

\item{pstepsInterval}{Psteps recording frequency (h)}

\item{pstepsMaxDepth}{Maximum depth for tallying a particle (m)}

\item{recordVertDistr}{Record vertical distribution of particles per element?}

\item{vertDistrInterval}{Vert distribution recording frequency (h)}

\item{vertDistrMax}{Maximum depth to record (\code{0:vertDistrMax}) with all deeper particles lumped into last group}

\item{recordMovement}{Record movement details for sample of particles?}

\item{recordElemActivity}{Record overall element/hourly particle activity?}

\item{recordConnectivity}{Record connectivity (mature particles only) between sites?}

\item{connectImmature}{Record connectivity for immature particles between sites?}

\item{connectDepth1_min}{Minimum depth (m) of particles connectivity (depth range 1); default = 0}

\item{connectDepth1_max}{Maximum depth (m) of particles connectivity (depth range 1); default = 10000}

\item{connectDepth2_min}{Minimum depth (m) of particles connectivity (depth range 2); default (10000) causes biotracker to ignore depth range 2}

\item{connectDepth2_max}{Maximum depth (m) of particles connectivity (depth range 2); default (10000) causes biotracker to ignore depth range 2}

\item{connectivityInterval}{Connectivity recording frequency (h)}

\item{connectivityThresh}{Distance threshold for connectivity recording}

\item{recordLocations}{Record hourly particle locations?}

\item{recordArrivals}{Record particle arrivals to sites?}

\item{destinationDirectory}{Output directory}

\item{datadir2}{Secondary data directory}

\item{datadir2Prefix}{Secondary data file prefix}

\item{datadir2Suffix}{Secondary data file suffix}

\item{mesh2}{Secondary mesh file}

\item{mesh2Type}{Secondary mesh type}

\item{location2}{Secondary domain}

\item{minchVersion2}{Secondary mesh WeStCOMS version}
}
\value{
Character vector with all properties; if properties_file_path is set, output is written to that file (overwriting any existing file)
}
\description{
Generate properties for biotracker
}

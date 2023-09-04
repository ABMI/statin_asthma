library(asthma02142)

# Optional: specify where the temporary files (used by the Andromeda package) will be created:
options(fftempdir = "temp folder location")

# Maximum number of cores to be used:
maxCores <- parallel::detectCores()

# The folder where the study intermediate and result files will be written:
outputFolder <- "output folder location"


# Details for connecting to the server:

connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = "sql server",
                                                                server = '128.1.99.58',
                                                                user = '',
                                                                password = '',
                                                                pathToDriver = '')
conn <- DatabaseConnector::connect(connectionDetails)

# The name of the database schema where the CDM data can be found:
cdmDatabaseSchema <- "CDMPv534_ABMI.dbo"

# The name of the database schema and table where the study-specific cohorts will be instantiated:
cohortDatabaseSchema <- "cohortDb.dbo"
cohortTable <- 'asthma02142'

# Some meta-information that will be used by the export function:
databaseId <- "CDMPv534_ABMI"
databaseName <- "CDMPv534_ABMI"
databaseDescription <- "CDMPv534_ABMI"

# For Oracle: define a schema that can be used to emulate temp tables:
oracleTempSchema <- NULL

execute(connectionDetails = connectionDetails,
        cdmDatabaseSchema = cdmDatabaseSchema,
        cohortDatabaseSchema = cohortDatabaseSchema,
        cohortTable = cohortTable,
        oracleTempSchema = oracleTempSchema,
        outputFolder = outputFolder,
        databaseId = databaseId,
        databaseName = databaseName,
        databaseDescription = databaseDescription,
        createCohorts = TRUE,
        synthesizePositiveControls = TRUE,
        runAnalyses = TRUE,
        packageResults = TRUE,
        maxCores = maxCores)

resultsZipFile <- file.path(outputFolder, "export", paste0("Results_", databaseId, ".zip"))
dataFolder <- file.path(outputFolder, "shinyData")

# You can inspect the results if you want:
prepareForEvidenceExplorer(resultsZipFile = resultsZipFile, dataFolder = dataFolder)
launchEvidenceExplorer(dataFolder = dataFolder, blind = FALSE, launch.browser = FALSE)

# Upload the results to the OHDSI SFTP server:
privateKeyFileName <- ""
userName <- ""
uploadResults(outputFolder, privateKeyFileName, userName)

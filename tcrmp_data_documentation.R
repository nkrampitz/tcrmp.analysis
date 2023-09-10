#' TCRMP Benthic Cover Data
#'
#' A dataset containing percent cover of 107 individual benthic
#' categories and 12 additional summary benthic categories. Each
#' row represents the benthic cover of one transect at a
#' location for one sampling period. Variables are defined below.
#'
#' \itemize{
#'   \item SampleYear. Monitoring period in which that data was
#'   collected. Note - SampleYear may not always match the year
#'   from the benthic sample date.
#'   \item SampleMonth. Month in which data was collected. Note -
#'   when a location was not surveyed fully in the same month, the
#'   month in which sampling began is used as the sample month.
#'   \item Period. Sample period in which the data falls. "Annual"
#'   refers to normal annual TCRMP sampling. "PeakBL" refers to
#'   additional sampling to capture peak bleaching due to thermal
#'   stress. "PostBL" refers to additional sampling as a follow up
#'   to thermal stress. "SCTLD" refers to additional sampling at a
#'   subset of TCRMP locations to monitor the progression of Stony
#'   Coral Tissue Loss Disease. "WS" refers to additional sampling
#'   at a subset of TCRMP locations during research cruises aboard
#'   the R/V Walton Smith.
#'   \item Location. Monitoring location name
#'   \item Transect. Permanent transect number (1-6)
#'   \item AA. \emph{Agaricia agaricites} cover
#'   \item AC. \emph{Acropora cervicornis} cover
#'   \item AF. \emph{Agaricia fragilis} cover
#'   \item AG. \emph{Agaricia grahamae} cover
#'   \item AGSP. \emph{Agaricia} sp. cover
#'   \item AH. \emph{Agaricia humilis} cover
#'   \item AL. \emph{Agaricia lamarcki} cover
#'   \item AP. \emph{Acropora palmata} cover
#'   \item APR. \emph{Acropora prolifera} cover
#'   \item AT. \emph{Agaricia tenuifolia} cover
#'   \item AU. \emph{Agaricia undata} cover
#'   \item CN. \emph{Colpophyllia natans} cover
#'   \item CORAL. unidentified coral cover
#'   \item CORJU. unidentified juvenile coral cover
#'   \item DCY. \emph{Dendrogyra cylindrus} cover
#'   \item DL. \emph{Diploria labrythiformis} cover
#'   \item DSO. \emph{Dichocoenia stokesii} cover
#'   \item EF. \emph{Eusmilia fastigiata} cover
#'   \item FF. \emph{Favia fragum} cover
#'   \item HC. \emph{Helioseris cucullata} cover
#'   \item IR. \emph{Isophyllia rigida} cover
#'   \item IS. \emph{Isophyllia sinuosa} cover
#'   \item MAFO. \emph{Madracis formosa} cover
#'   \item MAL. \emph{Mycetophyllia aliciae} cover
#'   \item MAN. \emph{Mussa angulosa} cover
#'   \item MAR. \emph{Manicina areolata} cover
#'   \item MC. \emph{Montastraea cavernosa} cover
#'   \item MD. \emph{Madracis decactis} cover
#'   \item MDA. \emph{Mycetophyllia daniana} cover
#'   \item MF. \emph{Mycetophyllia ferox} cover
#'   \item MILA. \emph{Millepora alcicornis} cover
#'   \item MILC. \emph{Millepora complanata} cover
#'   \item MILS. \emph{Millepora squarrosa} cover
#'   \item ML. \emph{Mycetophyllia lamarckiana} cover
#'   \item MM. \emph{Madracis mirabilis} cover
#'   \item MME. \emph{Meandrina meandrites} cover
#'   \item MSPP. \emph{Montastraea} sp. cover
#'   \item MYSP. \emph{Mycetophyllia} sp. cover
#'   \item OA. \emph{Orbicella annularis} cover
#'   \item OD. \emph{Oculina diffusa} cover
#'   \item OFAV. \emph{Orbicella faveolata} cover
#'   \item OFRA. \emph{Orbicella franksi} cover
#'   \item OX. \emph{Orbicella annularis} species complex cover
#'   \item PA. \emph{Porites astreoides} cover
#'   \item PB. \emph{Porites branneri} cover
#'   \item PBSP. Branching \emph{Porites} sp. cover
#'   \item PC. \emph{Porites colonensis} cover
#'   \item PCL. \emph{Pseudodiplora clivosa} cover
#'   \item PD. \emph{Porites divaricata} cover
#'   \item PF. \emph{Porites furcata} cover
#'   \item PP. \emph{Porites porites} cover
#'   \item PS. \emph{Psuedodiploria strigosa} cover
#'   \item SB. \emph{Solenastrea bournoni} cover
#'   \item SC. \emph{Scolymia cubsensis} cover
#'   \item SCSP. \emph{Scolymia} sp. cover
#'   \item SH. \emph{Solenastrea hyades} cover
#'   \item SI. \emph{Stephanocoenia intersepta} cover
#'   \item SL. \emph{Scolymia lacera} cover
#'   \item SR. \emph{Siderastrea radians} cover
#'   \item SS. \emph{Siderastrea siderea} cover
#'   \item SSPP. \emph{Siderastrea} sp. cover
#'   \item TC. \emph{Tubastraea coccinea} cover
#'   \item BRIA. \emph{Briareum asbestinum} cover
#'   \item ENGO. Encrusting gorgonian cover
#'   \item ERYTH. \emph{Erythropodium caribaeorum} cover
#'   \item FAN. Sea fan cover
#'   \item GO. Unidentified gorgonian cover
#'   \item PLUME. Sea plume cover
#'   \item ROD. Sea rod cover
#'   \item WHIP. Sea whip cover
#'   \item BALL. Ball sponge cover
#'   \item BASP. Barrel sponge cover
#'   \item BOSP. Boring sponge cover
#'   \item CLIO. \emph{Cliona delitrix} cover
#'   \item CLSP. \emph{Cliona} sp. cover
#'   \item ENSP. Encrusting sponge cover
#'   \item ROPE. Rope sponge cover
#'   \item SPO. Unidentified sponge cover
#'   \item TUBE. Tube sponge cover
#'   \item PALY. \emph{Palythoa caribaeorum} cover
#'   \item ZO. Unidentified zoanthid cover
#'   \item ZOSO. \emph{Zoanthus sociatus} cover
#'   \item ANEM. Anemone cover
#'   \item CMOR. Corallimorpharian cover
#'   \item AMPH. \emph{Amphiroa} sp. cover
#'   \item CLAD. \emph{Cladophora} sp. cover
#'   \item DICT. \emph{Dictyota} sp. cover
#'   \item HALI. \emph{Halimeda} sp. cover
#'   \item LIAG. \emph{Liagora} sp. cover
#'   \item LOBO. \emph{Lobophora} sp. cover
#'   \item MACA. Unidentified macroalgae cover
#'   \item MICRO. \emph{Microdictyon} sp. cover
#'   \item PEY. \emph{Peyssonnellia} sp. cover
#'   \item RAMI. \emph{Ramicrusta} sp. cover
#'   \item SARG. \emph{Sargassum} sp. cover
#'   \item FLCY. Filamentous cyanobacteria cover
#'   \item LYNG. \emph{Lyngbia} sp. cover
#'   \item SCHIZ. \emph{Schizothrix} sp. cover
#'   \item EAC. Epilithic algae community cover
#'   \item B. Boulder cover
#'   \item P. Pavement cover
#'   \item R. Rubble cover
#'   \item RO. Rhodolith cover
#'   \item S. Sand cover
#'   \item O. Other living cover
#'   \item UNK. Unknown cover
#'   \item CoralCov. Coral cover, total
#'   \item GorgCov. Gorgonian cover, total
#'   \item SpoCov. Sponge cover, total
#'   \item ZoanCov. Zoanthid cover, total
#'   \item OtherLiveCov. Other living cover, total
#'   \item MacaCov. Macroalgae cover, total
#'   \item CyanCov. Cyanobacteria cover, total
#'   \item NonLiveCov. Non-living cover, total
#'   \item OrbicellaCov. \emph{Orbicella} sp. cover, total
#'   \item AgariciaCov. \emph{Agaricia} sp. cover, total
#'   \item OtherCoralCov. Other coral cover, total
#'   \item ClionaCov. \emph{Cliona} sp. cover, total
#' }
#'
#' @docType data
#' @keywords datasets
#' @name tcrmp_benthic
#' @usage data(tcrmp_benthic)
#' @format A data frame with 4006 rows and 124 variables
#' @references Ennis RS, Kadison E, Heidmann SL, Brandt ME, Henderson
#'   LM, Smith TB (2020) The United States Virgin Islands Territorial
#'   Coral Reef Monitoring Program. 2020 Annual Report. University of
#'   the Virgin Islands, United States Virgin Islands 299pp
#' @source \url{https://sites.google.com/site/usvitcrmp/available-data}
NULL


#' TCRMP Location Metadata
#'
#' A dataset containing TCRMP location metadata. Variables are defined
#' below.
#'
#' \itemize{
#'   \item Location. Monitoring location name
#'   \item Code. 3-letter code that refers to a monitoring location
#'   \item Island. The island at which the monitoring location is located
#'   \item Latitude. Monitoring location latitude
#'   \item Longitude. Monitoring location longitude
#'   \item ReefComplex. The reef complex (nearshore, offshore, or
#'     mesophotic) to which the monitoring location belongs
#'   \item Depth. Monitoring location depth in meters
#'   \item YearAdded. The year in which the monitoring location was added
#'     to the program
#'   \item Rugosity. Monitoring location rugosity in meters
#' }
#'
#' @docType data
#' @keywords datasets
#' @name tcrmp_metadata
#' @usage data(tcrmp_metadata)
#' @format A data frame with 34 rows and 9 variables
#' @references Ennis RS, Kadison E, Heidmann SL, Brandt ME, Henderson
#'   LM, Smith TB (2020) The United States Virgin Islands Territorial
#'   Coral Reef Monitoring Program. 2020 Annual Report. University of
#'   the Virgin Islands, United States Virgin Islands 299pp
#' @source \url{https://sites.google.com/site/usvitcrmp/tcrmp-info}
NULL


#' TCRMP Benthic Sample Dates
#'
#' A dataset containing benthic sample dates for each TCRMP location.
#' Variables are defined below.
#'
#' \itemize{
#'   \item Location. Monitoring location name
#'   \item SampleDate. Date on which benthic sampling was conducted
#'     at the monitoring location
#'   \item SampleYear. Monitoring period in which that data was
#'   collected. Note - SampleYear may not always match the year
#'   from the benthic sample date.
#'   \item SampleMonth. Month in which data was collected. Note -
#'   when a location was not surveyed fully in the same month, the
#'   month in which sampling began is used as the sample month.
#'   \item Period. Sample period in which the data falls. "Annual"
#'   refers to normal annual TCRMP sampling. "PeakBL" refers to
#'   additional sampling to capture peak bleaching due to thermal
#'   stress. "PostBL" refers to additional sampling as a follow up
#'   to thermal stress. "SCTLD" refers to additional sampling at a
#'   subset of TCRMP locations to monitor the progression of Stony
#'   Coral Tissue Loss Disease. "WS" refers to additional sampling
#'   at a subset of TCRMP locations during research cruises aboard
#'   the R/V Walton Smith.
#' }
#'
#' @docType data
#' @keywords datasets
#' @name tcrmp_benthic_sample_date
#' @usage data(tcrmp_benthic_sample_date)
#' @format A data frame with 670 rows and 5 variables
#' @references Ennis RS, Kadison E, Heidmann SL, Brandt ME, Henderson
#'   LM, Smith TB (2020) The United States Virgin Islands Territorial
#'   Coral Reef Monitoring Program. 2020 Annual Report. University of
#'   the Virgin Islands, United States Virgin Islands 299pp
#' @source \url{https://sites.google.com/site/usvitcrmp/available-data}
NULL


#' TCRMP Coral Health Assessment Sample Dates
#'
#' A dataset containing coral health assessment sample dates for each
#' TCRMP location. Variables are defined below.
#'
#' \itemize{
#'   \item Location. Monitoring location name
#'   \item SampleDate. Date on which coral health assessment sampling
#'     was conducted at the monitoring location
#'   \item SampleYear. Monitoring period in which that data was
#'   collected. Note - SampleYear may not always match the year
#'   from the benthic sample date.
#'   \item SampleMonth. Month in which data was collected. Note -
#'   when a location was not surveyed fully in the same month, the
#'   month in which sampling began is used as the sample month.
#'   \item Period. Sample period in which the data falls. "Annual"
#'   refers to normal annual TCRMP sampling. "PeakBL" refers to
#'   additional sampling to capture peak bleaching due to thermal
#'   stress. "PostBL" refers to additional sampling as a follow up
#'   to thermal stress. "SCTLD" refers to additional sampling at a
#'   subset of TCRMP locations to monitor the progression of Stony
#'   Coral Tissue Loss Disease. "WS" refers to additional sampling
#'   at a subset of TCRMP locations during research cruises aboard
#'   the R/V Walton Smith.
#' }
#'
#' @docType data
#' @keywords datasets
#' @name tcrmp_health_sample_date
#' @usage data(tcrmp_health_sample_date)
#' @format A data frame with 641 rows and 5 variables
#' @references Ennis RS, Kadison E, Heidmann SL, Brandt ME, Henderson
#'   LM, Smith TB (2020) The United States Virgin Islands Territorial
#'   Coral Reef Monitoring Program. 2020 Annual Report. University of
#'   the Virgin Islands, United States Virgin Islands 299pp
#' @source \url{https://sites.google.com/site/usvitcrmp/available-data}
NULL


#' TCRMP \emph{Diadema antillarum} Data
#'
#' A dataset containing \emph{Diadema antillarum} density in which
#' each row represents the density of one transect at a location for
#' one sampling period. Variables are defined below.
#'
#' \itemize{
#'   \item Location. Monitoring location name
#'   \item SampleYear. Monitoring period in which that data was
#'   collected. Note - SampleYear may not always match the year
#'   from the benthic sample date.
#'   \item Period. Sample period in which the data falls. "Annual"
#'   refers to normal annual TCRMP sampling. "PeakBL" refers to
#'   additional sampling to capture peak bleaching due to thermal
#'   stress. "PostBL" refers to additional sampling as a follow up
#'   to thermal stress. "SCTLD" refers to additional sampling at a
#'   subset of TCRMP locations to monitor the progression of Stony
#'   Coral Tissue Loss Disease. "WS" refers to additional sampling
#'   at a subset of TCRMP locations during research cruises aboard
#'   the R/V Walton Smith.
#'   \item Transect. Transect number
#'   \item DiadDens. \emph{Diadema antillarum} density as individuals
#'     per 100 sq. meters
#' }
#'
#' @docType data
#' @keywords datasets
#' @name tcrmp_diadema
#' @usage data(tcrmp_diadema)
#' @format A data frame with 4723 rows and 5 variables
#' @references Ennis RS, Kadison E, Heidmann SL, Brandt ME, Henderson
#'   LM, Smith TB (2020) The United States Virgin Islands Territorial
#'   Coral Reef Monitoring Program. 2020 Annual Report. University of
#'   the Virgin Islands, United States Virgin Islands 299pp
#' @source \url{https://sites.google.com/site/usvitcrmp/available-data}
NULL


#' TCRMP Fish Census Data
#'
#' A dataset containing fish abundance and biomass from TCRMP fish
#' census surveys in which each row represents a species on a transect
#' at a location for one sampling period. Variables are defined below.
#'
#' \itemize{
#'   \item Location. Monitoring location name
#'   \item Month. Month in which data was collected. Note -
#'   when a location was not surveyed fully in the same month, the
#'   month in which sampling began is used as the sample month.
#'   \item Year. Year in which data was collected.
#'   \item SampleYear. Monitoring period in which that data was
#'   collected. Note - SampleYear may not always match the year
#'   from the benthic sample date.
#'   \item Period. Sample period in which the data falls. "Annual"
#'   refers to normal annual TCRMP sampling. "PeakBL" refers to
#'   additional sampling to capture peak bleaching due to thermal
#'   stress. "PostBL" refers to additional sampling as a follow up
#'   to thermal stress. "SCTLD" refers to additional sampling at a
#'   subset of TCRMP locations to monitor the progression of Stony
#'   Coral Tissue Loss Disease. "WS" refers to additional sampling
#'   at a subset of TCRMP locations during research cruises aboard
#'   the R/V Walton Smith. "PostHurr" refers to additional sampling
#'   after the passage of Hurricanes Irma and Maria in September 2017.
#'   \item Transect. Transect number
#'   \item ScientificName. Species scientific name
#'   \item CommonName. Species common name
#'   \item TrophicGroup. Trophic group to which the species belongs
#'     (i.e. herbivore, omnivore, piscivore, etc.)
#'   \item Metric. Species abundance or biomass (kg) per 100 sq.
#'     meters.
#'   \item Total. Total number of individuals observed per 100
#'     sq. meters or total biomass (kg) per 100 sq. meters.
#' }
#'
#' @docType data
#' @keywords datasets
#' @name tcrmp_fish
#' @usage data(tcrmp_fish)
#' @format A data frame with 206776 rows and 11 variables
#' @references Ennis RS, Kadison E, Heidmann SL, Brandt ME, Henderson
#'   LM, Smith TB (2020) The United States Virgin Islands Territorial
#'   Coral Reef Monitoring Program. 2020 Annual Report. University of
#'   the Virgin Islands, United States Virgin Islands 299pp
#' @source \url{https://sites.google.com/site/usvitcrmp/available-data}
NULL


#' TCRMP Coral Health Assessment Data
#'
#' A dataset containing coral health assessment data from TCRMP locations.
#' Each row represents the health assessment of an individual colony on a
#' transect at a location for one sampling period. All interaction variables
#' are recorded as percent of live coral tissue affected unless otherwise
#' noted. Variables are defined below.
#'
#' \itemize{
#'   \item Location. Monitoring location name
#'   \item SampleYear. Monitoring period in which that data was
#'   collected. Note - SampleYear may not always match the year
#'   from the benthic sample date.
#'   \item SampleMonth. Month in which data was collected. Note -
#'   when a location was not surveyed fully in the same month, the
#'   month in which sampling began is used as the sample month.
#'   \item Period. Sample period in which the data falls. "Annual"
#'   refers to normal annual TCRMP sampling. "PeakBL" refers to
#'   additional sampling to capture peak bleaching due to thermal
#'   stress. "PostBL" refers to additional sampling as a follow up
#'   to thermal stress. "SCTLD" refers to additional sampling at a
#'   subset of TCRMP locations to monitor the progression of Stony
#'   Coral Tissue Loss Disease. "WS" refers to additional sampling
#'   at a subset of TCRMP locations during research cruises aboard
#'   the R/V Walton Smith.
#'   \item Transect. Permanent transect number (1-6)
#'   \item SPP. Scleractinian coral species code. Codes match those
#'     reported in the documentation for TCRMP benthic cover.
#'   \item Length. Maximum colony diameter in cm
#'   \item Width. Colony width in cm measured perpendicular to
#'     length
#'   \item Height. Colony height in cm measured perpendicular to
#'     the plane of growth
#'   \item Dict. \emph{Dictyota} sp. interaction
#'   \item Lobo. \emph{Lobophora} sp. interaction
#'   \item Hali. \emph{Halimeda} sp. interaction
#'   \item Sarg. \emph{Sargassum} sp. interaction
#'   \item Calg. CCA interaction
#'   \item Fil. Filamentous turf algae interaction
#'   \item OtherMaca. Unidentified macroalgae interaction
#'   \item Cyano. Cyanobacteria interaction measured as the
#'     percent of living coral perimeter bordered by cyanobacteria
#'   \item Clio. \emph{Cliona delitrix} interaction measured as the
#'     percent of the colony skeletal structure with emergent
#'     oscula
#'   \item Icing. \emph{Mycale laevis} interaction measured as
#'     presence (P) or absence (NA)
#'   \item Spo. Unidentified sponge interaction
#'   \item Gorg. Unidentified gorgonian interaction
#'   \item Worm. Unidentified worm interaction
#'   \item Sed. Sediment interaction
#'   \item Other. Miscellaneous interaction
#'   \item NoDam. Total number damselfish by species present at a
#'     colony
#'   \item NoCvore. Total number coralivores present at a colony.
#'   \item Pred. Predation interaction
#'   \item Damage. Notes indicating colony damage
#'   \item BLP. Code indicating coral bleaching (BL) or paling (P)
#'   \item BL. Percent living colony tissue bleached
#'   \item P. Percent living colony tissue pale
#'   \item VP. Percent living colony tissue very pale
#'   \item SP. Percent living colony tissue slightly pale
#'   \item TotBL. Total percent living colony tissue bleached or pale
#'   \item OldMort. Percent old partial mortality
#'   \item RecMort. Percent recent partial mortality
#'   \item TotMort. Total percent partial mortality
#'   \item Dis1. Primary coral disease identification. Disease codes
#'     are as follows: ATL (acute tissue loss), BBD (Black Band Disease),
#'     DCOR (disease coral/unidentifed lesion), DSD (Dark Spot Disease),
#'     IMS (Intercostal Mortality Syndrome), PLA (White Plague), SCTLD
#'     (Stony Coral Tissue Loss Disease), UNK (Unknown disease), WBD
#'     (White Band Disease), YBD (Yellow Band Disease).
#'   \item Dis2. Secondary coral disease identification. Disease codes
#'     are as above.
#'   \item ATL. Percent colony affected by acute tissue loss
#'   \item BBD. Percent colony affected by Black Band Disease
#'   \item DCOR. Percent colony affected by an unidentified lesion
#'   \item DSD. Percent colony affected by Dark Spot Disease
#'   \item IMS. Percent colony affected by Intercostal Mortality Syndrome
#'   \item PLA. Percent colony affected by White Plague
#'   \item SCTLD. Percent colony affected by Stony Coral Tissue Loss Disease
#'   \item UNK. Percent colony affected by an unknown disease
#'   \item WBD. Percent colony affected by White Band Disease
#'   \item YBD. Percent colony affected by Yellow Band Disease
#'   \item TotDis. Total percent colony affected by disease
#' }
#'
#' @docType data
#' @keywords datasets
#' @name tcrmp_health
#' @usage data(tcrmp_health)
#' @format A data frame with 72715 rows and 50 variables
#' @references Ennis RS, Kadison E, Heidmann SL, Brandt ME, Henderson
#'   LM, Smith TB (2020) The United States Virgin Islands Territorial
#'   Coral Reef Monitoring Program. 2020 Annual Report. University of
#'   the Virgin Islands, United States Virgin Islands 299pp
#' @source \url{https://sites.google.com/site/usvitcrmp/available-data}
NULL


#' TCRMP Location Bleaching Thresholds
#'
#' A dataset containing bleaching thresholds for each TCRMP location.
#' Variables are defined below.
#'
#' \itemize{
#'   \item Location. Monitoring location name
#'   \item BT. Location bleaching threshold in degrees Celsius
#'   \item MMM. Location maximum monthly mean in degrees Celsius
#' }
#'
#' @docType data
#' @keywords datasets
#' @name tcrmp_bl_th
#' @usage data(tcrmp_bl_th)
#' @format A data frame with 43 rows and 3 variables
#' @references Ennis RS, Kadison E, Heidmann SL, Brandt ME, Henderson
#'   LM, Smith TB (2020) The United States Virgin Islands Territorial
#'   Coral Reef Monitoring Program. 2020 Annual Report. University of
#'   the Virgin Islands, United States Virgin Islands 299pp
#' @source \url{https://sites.google.com/site/usvitcrmp/available-data}
NULL


#' TCRMP Temperature Sample Dates
#'
#' A dataset containing dates on which temperature loggers were
#' swapped for each TCRMP location. Variables are defined below.
#'
#' \itemize{
#'   \item Location. Monitoring location name
#'   \item SampleDate. Date on which coral health assessment sampling
#'     was conducted at the monitoring location
#'   \item SampleYear. Monitoring period in which that data was
#'   collected. Note - SampleYear may not always match the year
#'   from the benthic sample date.
#'   \item SampleMonth. Month in which data was collected. Note -
#'   when a location was not surveyed fully in the same month, the
#'   month in which sampling began is used as the sample month.
#'   \item Period. Sample period in which the data falls. "Annual"
#'   refers to normal annual TCRMP sampling. "PeakBL" refers to
#'   additional sampling to capture peak bleaching due to thermal
#'   stress. "PostBL" refers to additional sampling as a follow up
#'   to thermal stress. "SCTLD" refers to additional sampling at a
#'   subset of TCRMP locations to monitor the progression of Stony
#'   Coral Tissue Loss Disease. "WS" refers to additional sampling
#'   at a subset of TCRMP locations during research cruises aboard
#'   the R/V Walton Smith.
#' }
#'
#' @docType data
#' @keywords datasets
#' @name tcrmp_temp_sample_date
#' @usage data(tcrmp_temp_sample_date)
#' @format A data frame with 701 rows and 5 variables
#' @references Ennis RS, Kadison E, Heidmann SL, Brandt ME, Henderson
#'   LM, Smith TB (2020) The United States Virgin Islands Territorial
#'   Coral Reef Monitoring Program. 2020 Annual Report. University of
#'   the Virgin Islands, United States Virgin Islands 299pp
#' @source \url{https://sites.google.com/site/usvitcrmp/tcrmp-info}
NULL


#' TCRMP Algae Heights Data
#'
#' A dataset containing algae canopy height data from TCRMP locations.
#' Each row represents an individual height measurement on a transect at
#' a location for one sampling period. All height measurements are
#' recorded in centimeters. Encrusting macroalgae are typically not
#' measured and are represented by a height value of 0.0 or "NA".
#' Variables are defined below.
#'
#' \itemize{
#'   \item Location. Monitoring location name
#'   \item SampleYear. Monitoring period in which that data was
#'   collected. Note - SampleYear may not always match the year
#'   from the benthic sample date.
#'   \item SampleMonth. Month in which data was collected. Note -
#'   when a location was not surveyed fully in the same month, the
#'   month in which sampling began is used as the sample month.
#'   \item Period. Sample period in which the data falls. "Annual"
#'   refers to normal annual TCRMP sampling. "PeakBL" refers to
#'   additional sampling to capture peak bleaching due to thermal
#'   stress. "PostBL" refers to additional sampling as a follow up
#'   to thermal stress. "SCTLD" refers to additional sampling at a
#'   subset of TCRMP locations to monitor the progression of Stony
#'   Coral Tissue Loss Disease. "WS" refers to additional sampling
#'   at a subset of TCRMP locations during research cruises aboard
#'   the R/V Walton Smith.
#'   \item SampleDate. Date on which algae height sampling was
#'     conducted at the monitoring location
#'   \item Transect. Permanent transect number (1-6)
#'   \item Substrate. Indicates the genus or type of algae
#'   \item Height. Height of algae (when applicable) in cm
#' }
#'
#' @docType data
#' @keywords datasets
#' @name tcrmp_algae
#' @usage data(tcrmp_algae)
#' @format A data frame with 27428 rows and 8 variables
#' @references Ennis RS, Kadison E, Heidmann SL, Brandt ME, Henderson
#'   LM, Smith TB (2020) The United States Virgin Islands Territorial
#'   Coral Reef Monitoring Program. 2020 Annual Report. University of
#'   the Virgin Islands, United States Virgin Islands 299pp
#' @source \url{https://sites.google.com/site/usvitcrmp/available-data}
NULL

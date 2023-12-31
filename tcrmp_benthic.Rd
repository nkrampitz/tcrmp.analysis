% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tcrmp_data_documentation.R
\docType{data}
\name{tcrmp_benthic}
\alias{tcrmp_benthic}
\title{TCRMP Benthic Cover Data}
\format{
A data frame with 4006 rows and 124 variables
}
\source{
\url{https://sites.google.com/site/usvitcrmp/available-data}
}
\usage{
data(tcrmp_benthic)
}
\description{
A dataset containing percent cover of 107 individual benthic
categories and 12 additional summary benthic categories. Each
row represents the benthic cover of one transect at a
location for one sampling period. Variables are defined below.
}
\details{
\itemize{
\item SampleYear. Monitoring period in which that data was
collected. Note - SampleYear may not always match the year
from the benthic sample date.
\item SampleMonth. Month in which data was collected. Note -
when a location was not surveyed fully in the same month, the
month in which sampling began is used as the sample month.
\item Period. Sample period in which the data falls. "Annual"
refers to normal annual TCRMP sampling. "PeakBL" refers to
additional sampling to capture peak bleaching due to thermal
stress. "PostBL" refers to additional sampling as a follow up
to thermal stress. "SCTLD" refers to additional sampling at a
subset of TCRMP locations to monitor the progression of Stony
Coral Tissue Loss Disease. "WS" refers to additional sampling
at a subset of TCRMP locations during research cruises aboard
the R/V Walton Smith.
\item Location. Monitoring location name
\item Transect. Permanent transect number (1-6)
\item AA. \emph{Agaricia agaricites} cover
\item AC. \emph{Acropora cervicornis} cover
\item AF. \emph{Agaricia fragilis} cover
\item AG. \emph{Agaricia grahamae} cover
\item AGSP. \emph{Agaricia} sp. cover
\item AH. \emph{Agaricia humilis} cover
\item AL. \emph{Agaricia lamarcki} cover
\item AP. \emph{Acropora palmata} cover
\item APR. \emph{Acropora prolifera} cover
\item AT. \emph{Agaricia tenuifolia} cover
\item AU. \emph{Agaricia undata} cover
\item CN. \emph{Colpophyllia natans} cover
\item CORAL. unidentified coral cover
\item CORJU. unidentified juvenile coral cover
\item DCY. \emph{Dendrogyra cylindrus} cover
\item DL. \emph{Diploria labrythiformis} cover
\item DSO. \emph{Dichocoenia stokesii} cover
\item EF. \emph{Eusmilia fastigiata} cover
\item FF. \emph{Favia fragum} cover
\item HC. \emph{Helioseris cucullata} cover
\item IR. \emph{Isophyllia rigida} cover
\item IS. \emph{Isophyllia sinuosa} cover
\item MAFO. \emph{Madracis formosa} cover
\item MAL. \emph{Mycetophyllia aliciae} cover
\item MAN. \emph{Mussa angulosa} cover
\item MAR. \emph{Manicina areolata} cover
\item MC. \emph{Montastraea cavernosa} cover
\item MD. \emph{Madracis decactis} cover
\item MDA. \emph{Mycetophyllia daniana} cover
\item MF. \emph{Mycetophyllia ferox} cover
\item MILA. \emph{Millepora alcicornis} cover
\item MILC. \emph{Millepora complanata} cover
\item MILS. \emph{Millepora squarrosa} cover
\item ML. \emph{Mycetophyllia lamarckiana} cover
\item MM. \emph{Madracis mirabilis} cover
\item MME. \emph{Meandrina meandrites} cover
\item MSPP. \emph{Montastraea} sp. cover
\item MYSP. \emph{Mycetophyllia} sp. cover
\item OA. \emph{Orbicella annularis} cover
\item OD. \emph{Oculina diffusa} cover
\item OFAV. \emph{Orbicella faveolata} cover
\item OFRA. \emph{Orbicella franksi} cover
\item OX. \emph{Orbicella annularis} species complex cover
\item PA. \emph{Porites astreoides} cover
\item PB. \emph{Porites branneri} cover
\item PBSP. Branching \emph{Porites} sp. cover
\item PC. \emph{Porites colonensis} cover
\item PCL. \emph{Pseudodiplora clivosa} cover
\item PD. \emph{Porites divaricata} cover
\item PF. \emph{Porites furcata} cover
\item PP. \emph{Porites porites} cover
\item PS. \emph{Psuedodiploria strigosa} cover
\item SB. \emph{Solenastrea bournoni} cover
\item SC. \emph{Scolymia cubsensis} cover
\item SCSP. \emph{Scolymia} sp. cover
\item SH. \emph{Solenastrea hyades} cover
\item SI. \emph{Stephanocoenia intersepta} cover
\item SL. \emph{Scolymia lacera} cover
\item SR. \emph{Siderastrea radians} cover
\item SS. \emph{Siderastrea siderea} cover
\item SSPP. \emph{Siderastrea} sp. cover
\item TC. \emph{Tubastraea coccinea} cover
\item BRIA. \emph{Briareum asbestinum} cover
\item ENGO. Encrusting gorgonian cover
\item ERYTH. \emph{Erythropodium caribaeorum} cover
\item FAN. Sea fan cover
\item GO. Unidentified gorgonian cover
\item PLUME. Sea plume cover
\item ROD. Sea rod cover
\item WHIP. Sea whip cover
\item BALL. Ball sponge cover
\item BASP. Barrel sponge cover
\item BOSP. Boring sponge cover
\item CLIO. \emph{Cliona delitrix} cover
\item CLSP. \emph{Cliona} sp. cover
\item ENSP. Encrusting sponge cover
\item ROPE. Rope sponge cover
\item SPO. Unidentified sponge cover
\item TUBE. Tube sponge cover
\item PALY. \emph{Palythoa caribaeorum} cover
\item ZO. Unidentified zoanthid cover
\item ZOSO. \emph{Zoanthus sociatus} cover
\item ANEM. Anemone cover
\item CMOR. Corallimorpharian cover
\item AMPH. \emph{Amphiroa} sp. cover
\item CLAD. \emph{Cladophora} sp. cover
\item DICT. \emph{Dictyota} sp. cover
\item HALI. \emph{Halimeda} sp. cover
\item LIAG. \emph{Liagora} sp. cover
\item LOBO. \emph{Lobophora} sp. cover
\item MACA. Unidentified macroalgae cover
\item MICRO. \emph{Microdictyon} sp. cover
\item PEY. \emph{Peyssonnellia} sp. cover
\item RAMI. \emph{Ramicrusta} sp. cover
\item SARG. \emph{Sargassum} sp. cover
\item FLCY. Filamentous cyanobacteria cover
\item LYNG. \emph{Lyngbia} sp. cover
\item SCHIZ. \emph{Schizothrix} sp. cover
\item EAC. Epilithic algae community cover
\item B. Boulder cover
\item P. Pavement cover
\item R. Rubble cover
\item RO. Rhodolith cover
\item S. Sand cover
\item O. Other living cover
\item UNK. Unknown cover
\item CoralCov. Coral cover, total
\item GorgCov. Gorgonian cover, total
\item SpoCov. Sponge cover, total
\item ZoanCov. Zoanthid cover, total
\item OtherLiveCov. Other living cover, total
\item MacaCov. Macroalgae cover, total
\item CyanCov. Cyanobacteria cover, total
\item NonLiveCov. Non-living cover, total
\item OrbicellaCov. \emph{Orbicella} sp. cover, total
\item AgariciaCov. \emph{Agaricia} sp. cover, total
\item OtherCoralCov. Other coral cover, total
\item ClionaCov. \emph{Cliona} sp. cover, total
}
}
\references{
Ennis RS, Kadison E, Heidmann SL, Brandt ME, Henderson
LM, Smith TB (2020) The United States Virgin Islands Territorial
Coral Reef Monitoring Program. 2020 Annual Report. University of
the Virgin Islands, United States Virgin Islands 299pp
}
\keyword{datasets}

aboutText <- "To be completed."

#contributorsText <- cat(paste("Thomas Hladish, Ph.D., Department of Biology, and the Emerging Pathogens Institute, University of Florida, Gainesville, USA", "Arlin Stoltzfus, Ph.D., National Institute of Standards and Technology", "Kok Ben Toh, M.Sc., School of Natural Resources and Environment, University of Florida, Gainesville, USA", "Sanjana Bhargava, B.S., University of Florida, Gainesville, USA", "Alexander Pillai,  Microbiology major, University of Florida, Gainesville, USA", "Dianela Perdomo, Biology major, University of Florida, Gainesville, USA", sep="\n"))

plotStateDisclaimer <- "See \"Plot Information\" below or \"About\" page for more information on how case numbers are reported and anticipated."

plotCountyDisclaimer <- "The shaded region of the plot represents data from the past 14 days and is likely to be revised." 

plotClickInstr <- "Click on a column or drag over a region in the plot to see more information for the selected days."

eventDateDesc <- "Lab event date. Can be intepreted as sample collection date with some exceptions. Date provided by Florida DOH."

reportedCaseDesc <- "A reported case is case that has already been reported to the Florida DOH and its detail is publicly available."

anticipatedCaseDesc <- "Reported number of cases on a particular event day may not be finalized because there can be significant delay between 
sample collection and case confirmation. We create a predictive model ('nowcasting model') that anticipates how many more positive cases could have their 
sample collected on a particular event date but has yet to be confirmed. Anticipated numbers come with uncertainty, the 95% prediction interval is 
represented in the plot by the error bar. Note that anticipated cases can be negative because numbers of cases can be revised downward."

nowcastingDesc <- "Reported number of cases on a particular event day may not be finalized because there can be significant delay between 
lab event (e.g. sample collection) and case confirmation. In recent days, the median number of days between lab event and case confirmation is
around 3, and 90% of the cases were confirmed within 10 days after first lab event. Nevertheless, the lab event date of the report cases is 
subjected to revision from day to day for up to 2 weeks or more after initial assignment. Our nowcasting model anticpates the number of cases
at 14 days after the lab event day to serve as a guidance. This is done by accounting for the past trend in reporting delay trend, in daily revision
of cases event date, and currently reported numbers. Details about the model will be made available in near future."

plotStateHospDisclaimer <- "Numbers inferred from DOH daily updates"

plotStateDeathDisclaimer <- "Numbers inferred from DOH daily updates"

plotStatePosDisclaimer <- "TO BE WRITTEN"

plotCountyPosDisclaimer <- "TO BE WRITTEN"

aboutText <- "This visualizer is created by Hladish Lab members to visualize the COVID-19 pandemic in Florida. 
This visualizer is currently intended for internal use only. Aside from the nowcasting models which we use to 
account for reporting delay/revision in numbers statewide cases and deaths, all other numbers are extracted from 
data published by Florida DOH."

#contributorsText <- cat(paste("Thomas Hladish, Ph.D., Department of Biology, and the Emerging Pathogens Institute, University of Florida, Gainesville, USA", "Arlin Stoltzfus, Ph.D., National Institute of Standards and Technology", "Kok Ben Toh, M.Sc., School of Natural Resources and Environment, University of Florida, Gainesville, USA", "Sanjana Bhargava, B.S., University of Florida, Gainesville, USA", "Alexander Pillai,  Microbiology major, University of Florida, Gainesville, USA", "Dianela Perdomo, Biology major, University of Florida, Gainesville, USA", sep="\n"))

plotStateDisclaimer <- "See \"Plot details\" below or \"About\" page for more information."

plotCountyDisclaimer <- "Shaded region: data subject to revision due to delays in reporting recent events." 

plotClickInstr <- "Click on a column or drag over a region in the plot to see more information for the selected days."

eventDateDesc <- "Though reporting practices of health-care providers differ, the event date typically reflects the date of sample collection (by contrast to chart date, when the information is reported). Data from the Florida Department of Health."

reportedCaseDesc <- "A reported case has been diagnosed and reported to the Florida Department of Health."

anticipatedCaseDesc <- "A computer-based prediction of unreported cases using a 'nowcasting' model (error bars represent predicted 95 % confidence interval).  See the 'About' page for more information. Note that anticipated cases can be negative, i.e., the expected number of events on a date may be revised downward in the model."

nowcastingDesc <- "Reported number of cases on a particular event day may not be finalized because there can be significant delay between 
lab event (e.g. sample collection) and case confirmation. In recent days, the median number of days between lab event and case confirmation is
around 1, and 90% of the cases were confirmed within 2 days after first lab event. Our nowcasting model anticipates the number of cases
at 14 days after the lab event day to serve as a guidance. This is done by accounting for the past trend in reporting delay trend, in daily revision
of cases event date, and currently reported numbers. Details about the model will be made available in near future."

plotStateHospDisclaimer <- "Numbers inferred from DOH daily updates"

plotStateDeathDisclaimer <- "Numbers inferred from DOH daily updates"

plotStatePosDisclaimer <- "Numbers inferred from DOH daily updates. Calculated by number of residents with positive results over 
number of residents with test results per day"

plotCountyPosDisclaimer <- "Numbers inferred from DOH daily updates. Calculated by total daily number of residents with positive 
results over total daily number of residents with test results over past 7 days"

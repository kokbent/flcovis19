output$contributorsText <- renderUI({
  HTML(paste("<b>Thomas Hladish, Ph.D.</b><br/>Department of Biology, and the Emerging Pathogens Institute, University of Florida, Gainesville, USA",
             "<b>Arlin Stoltzfus, Ph.D.</b><br/>National Institute of Standards and Technology",
             "<b>Kok Ben Toh, M.Sc.</b><br/>School of Natural Resources and Environment, University of Florida, Gainesville, USA",
             "<b>Sanjana Bhargava, B.S.</b><br/>University of Florida, Gainesville, USA",
             "<b>Alexander Pillai</b><br/>Microbiology major, University of Florida, Gainesville, USA",
             "<b>Dianela Perdomo</b><br/>Biology major, University of Florida, Gainesville, USA", sep = "<br/>"))
})
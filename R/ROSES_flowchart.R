


ROSES_flowchart <- function (data,
                             interactive = FALSE,
                             type,
                             combined = TRUE,
                             font = 'Helvetica',
                             input_colour = 'HoneyDew2',
                             output_colour = 'LightSteelBlue1',
                             main_colour = 'Black',
                             arrow_colour = 'Black',
                             arrow_head = 'normal',
                             arrow_tail = 'none') {
  
  if (type == 'review'){
    #------------------------------------------------------------------------
    if(nrow(ftexcl) > 5){
      ftexclh <- 8.5 - ((nrow(ftexcl)-5)/4)
    } else {
      ftexclh <- 8.5
    }
    
    if (combined == TRUE){
      ystart <- 0
      titleabstract <- paste0('tandaincl [label = \'', paste0(text_wrap(tandaincl_text, 40),
                                 '\n(n = ',
                                 tandaincl,
                                 ')'
      ), "', width = 4, height = 0.5, pos='",4,",",11.5,"!', tooltip = ", tooltips[4], "]
      
      tandaexcl [label = '", paste0(text_wrap(tandaexcl_text, 40),
                                    '\n(n = ',
                                    tandaexcl,
                                    ')'
      ), "', width = 4, height = 0.5, pos='", 9, ",", 11.5, "!', tooltip = '", tooltips[4], "']")
      tanda_edges <- 'deduped->tandaincl;\ntandaincl->tandaexcl;\ntandaincl->ftretr;\n'
      screenboxh <- 7.3
      screenboxy <- 9.9
      searchboxy <- 14.5
    } else {
      ystart <- 1.5
      titleabstract <- paste0('titleincl [label = \'', paste0(text_wrap(titleincl_text, 40),
                                                              '\n(n = ',
                                                              titleincl,
                                                              ')'
      ), "', width = 4, height = 0.5, pos='",4,",", 13,"!', tooltip = ", tooltips[4], "]
      
      titleexcl [label = '", paste0(text_wrap(titleexcl_text, 40),
                                    '\n(n = ',
                                    titleexcl,
                                    ')'
      ), "', width = 4, height = 0.5, pos='", 9, ",", 13, "!', tooltip = '", tooltips[4], "']
      
      abstractincl [label = \'", paste0(text_wrap(abstractincl_text, 40),
                                                              '\n(n = ',
                                                              abstractincl,
                                                              ')'
      ), "', width = 4, height = 0.5, pos='",4,",",11.5,"!', tooltip = ", tooltips[4], "]
      
      abstractexcl [label = '", paste0(text_wrap(abstractexcl_text, 40),
                                    '\n(n = ',
                                    abstractexcl,
                                    ')'
      ), "', width = 4, height = 0.5, pos='", 9, ",", 11.5, "!', tooltip = '", tooltips[4], "']
      ")
      tanda_edges <- 'deduped->titleincl;\ntitleincl->titleexcl;\ntitleincl->abstractincl;\nabstractincl->abstractexcl;\nabstractincl->ftretr;\n'
      screenboxh <- 8.65
      screenboxy <- 10.65
      searchboxy <- 16
    }
    
    #------------------------------------------------------------------------
    x <- DiagrammeR::grViz(
      paste0("digraph TD {
      
      graph[splines=ortho, layout=neato, tooltip = 'Click the boxes for further information', fontsize = 20]
      
      node [shape = box]
      searching [color = LightSteelBlue2, label='', style = 'filled,rounded', pos='0,", searchboxy, "!', width = 0.4, height = 1.4, tooltip = '", tooltips[1], "'];
      screening [color = LightSteelBlue2, label='', style = 'filled,rounded', pos='0,", screenboxy, "!', width = 0.4, height = ", screenboxh, ", tooltip = '", tooltips[1], "'];
      casynth [color = LightSteelBlue2, label='', style = 'filled,rounded', pos='0,2.5!', width = 0.4, height = 4.4, tooltip = '", tooltips[1], "'];\n
      
      node [shape = box,
            fontname = ", font, ",
            color = ", input_colour, "]
      dbresults [label = '", paste0(text_wrap(dbresults_text, 40),
                                    '\n(n = ',
                                    dbresults,
                                    ')'
                                    ), "', width = 4, height = 0.5, pos='4,",ystart+14.5,"!', tooltip = '", tooltips[4], "', penwidth = 4]
      
      otherresults [label = '", paste0(text_wrap(otherresults_text, 40),
                                    '\n(n = ',
                                    otherresults,
                                    ')'
      ), "', width = 4, height = 0.5, pos='9,",ystart+14.5,"!', tooltip = '", tooltips[4], "', penwidth = 4]
      
      prescreened [label = '", paste0(text_wrap(prescreened_text, 20),
                                      '\n(n = ',
                                      prescreened,
                                      ')'
      ), "', width = 2.5, height = 0.5, pos='2,7!', tooltip = '", tooltips[4], "', penwidth = 4]
    
      node [shape = box,
            fontname = ", font, ",
            color = ", main_colour, "]
      deduped [label = '", paste0(text_wrap(deduped_text, 40),
                                  '\n(n = ',
                                  deduped,
                                  ')'
    ), "', width = 4, height = 0.5, pos='4,",ystart+13,"!', tooltip = '", tooltips[4], "']
      
      dupesremoved [label = '", paste0(text_wrap(dupesremoved_text, 40),
                                    '\n(n = ',
                                    dupesremoved,
                                    ')'
      ), "', width = 4, height = 0.5, pos='9,",ystart+13,"!', tooltip = '", tooltips[4], "']
    
      ", titleabstract, "
    
      ftretr [label = '", paste0(text_wrap(ftretr_text, 40),
                                 '\n(n = ',
                                 ftretr,
                                 ')'
      ), "', width = 4, height = 0.5, pos='4,10!', tooltip = '", tooltips[4], "']
      
      ftnotretr [label = '", paste0(text_wrap(ftnotretr_text, 40),
                                    '\n(',
                                    paste(paste(ftnotretr[1,], collapse = ', n ='), paste(ftnotretr[2,], collapse = ', n ='), sep = '; '),
                                    ')'
      ), "', width = 4, height = 0.5, pos='9,10!', tooltip = '", tooltips[4], "']  
    
      ftincl [label = '", paste0(text_wrap(ftincl_text, 40),
                                 '\n(n = ',
                                 ftincl,
                                 ')'
      ), "', width = 4, height = 0.5, pos='4,8.5!', tooltip = '", tooltips[4], "']
      
      ftexcl [label = '", paste0(text_wrap(ftexcl_text, 40),
                                 '\n\nExcluded on:\n',
                                 paste(paste0(ftexcl[,1], ' (n=', ftexcl[,2], ')'), collapse = '\n')
      ), "', width = 4, height = 0.5, pos='9,", ftexclh, "!', tooltip = '", tooltips[4], "']  
    
      studart [label = '", paste0(text_wrap(studart_text, 40),
                                 '\n(n = ',
                                 studart[1],
                                 ' / n = ',
                                 studart[2],
                                 ')'
      ), "', width = 4, height = 0.5, pos='4,5.5!', tooltip = '", tooltips[4], "']
    
      caincl [label = '", paste0(text_wrap(caincl_text, 40),
                                 '\n(n = ',
                                 caincl,
                                 ')'
      ), "', width = 4, height = 0.5, pos='4,4!', tooltip = '", tooltips[4], "']
      
      caexcl [label = '", paste0(text_wrap(caexcl_text, 40),
                                 '\n(n = ',
                                 caexcl,
                                 ')'
      ), "', width = 4, height = 0.5, pos='9,4!', tooltip = '", tooltips[4], "']
                                
      narrincl [label = '", paste0(text_wrap(narrincl_text, 40),
                                   '\n(n = ',
                                   narrincl,
                                   ')'
      ), "', width = 4, height = 0.5, pos='4,2.5!', tooltip = '", tooltips[4], "']
      
      finalexcl [label = '", paste0(text_wrap(finalexcl_text, 40),
                                    '\n(n = ',
                                    finalexcl,
                                    ')'
      ), "', width = 4, height = 0.5, pos='9,1!', tooltip = '", tooltips[4], "']
      
      node [shape = box,
            fontname = ", font, ",
            color = ", output_colour, "]
      finalincl [label = '", paste0(text_wrap(finalincl_text, 40),
                                    '\n(n = ',
                                    finalincl,
                                    ')'
      ), "', width = 4, height = 0.5, pos='4,1!', tooltip = '", tooltips[4], "', penwidth = 4]
      
      node [shape = square, width = 0, color=White]
      A0 [label = '', width = 0, height = 0, pos='9,", ystart+13.75, "!', tooltip='']
      A1 [label = '', width = 0, height = 0, pos='4,", ystart+13.75, "!', tooltip='']
      A2 [label = '', width = 0, height = 0, pos='4,7!', tooltip='']
      
      edge [color = ", arrow_colour, ", 
            arrowhead = ", arrow_head, ", 
            arrowtail = ", arrow_tail, ", 
            style = filled]
      deduped->dupesremoved;
      ", tanda_edges, "
      ftretr->ftnotretr;
      ftretr->ftincl;
      ftincl->ftexcl;
      prescreened->A2;
      studart->caincl;
      caincl->caexcl;
      caincl->narrincl;
      narrincl->finalincl;
      finalincl->finalexcl;
      
      edge [color = ", arrow_colour, ", 
            arrowhead = 'none', 
            arrowtail = ", arrow_tail, ", 
            style = filled]
      dbresults->A1;
      otherresults->A0;
      ftincl->A2;
      
      edge [color = ", arrow_colour, ", 
            arrowhead = ", arrow_head, ", 
            arrowtail = 'none', 
            style = filled]
      A1->deduped;      
      A2->studart;
      
      edge [color = ", arrow_colour, ", 
            arrowhead = 'none', 
            arrowtail = 'none', 
            style = filled]
      A0->A1
      
      }"))
    
    if (combined == 'TRUE'){
      insertJS <- function(plot){
        javascript <- htmltools::HTML('
var theDiv = document.getElementById("node1");
theDiv.innerHTML += "<text text-anchor=\'middle\' style=\'transform: rotate(-90deg);\' x=\'1022\' y=\'19\' font-family=\'Helvetica,sans-Serif\' font-size=\'14.00\'>Searching</text>";
var theDiv = document.getElementById("node2");
theDiv.innerHTML += "<text text-anchor=\'middle\' style=\'transform: rotate(-90deg);\' x=\'690\' y=\'19\' font-family=\'Helvetica,sans-Serif\' font-size=\'14.00\'>Screening</text>";
var theDiv = document.getElementById("node3");
theDiv.innerHTML += "<text text-anchor=\'middle\' style=\'transform: rotate(-90deg);\' x=\'160\' y=\'19\' font-family=\'Helvetica,sans-Serif\' font-size=\'14.00\'>Appraisal and Synthesis</text>";
                              ')
        htmlwidgets::appendContent(plot, htmlwidgets::onStaticRenderComplete(javascript))
      }
      x <- insertJS(x)
    } else {
      insertJS <- function(plot){
        javascript <- htmltools::HTML('
var theDiv = document.getElementById("node1");
theDiv.innerHTML += "<text text-anchor=\'middle\' style=\'transform: rotate(-90deg);\' x=\'1130\' y=\'19\' font-family=\'Helvetica,sans-Serif\' font-size=\'14.00\'>Searching</text>";
var theDiv = document.getElementById("node2");
theDiv.innerHTML += "<text text-anchor=\'middle\' style=\'transform: rotate(-90deg);\' x=\'740\' y=\'19\' font-family=\'Helvetica,sans-Serif\' font-size=\'14.00\'>Screening</text>";
var theDiv = document.getElementById("node3");
theDiv.innerHTML += "<text text-anchor=\'middle\' style=\'transform: rotate(-90deg);\' x=\'160\' y=\'19\' font-family=\'Helvetica,sans-Serif\' font-size=\'14.00\'>Appraisal and Synthesis</text>";
                              ')
        htmlwidgets::appendContent(plot, htmlwidgets::onStaticRenderComplete(javascript))
      }
      x <- insertJS(x)
    }
    
  } else if (type == 'map') {
    
    #------------------------------------------------------------------------
    if(nrow(ftexcl) > 5){
      ftexclh <- 5.5 - ((nrow(ftexcl)-5)/4)
    } else {
      ftexclh <- 5.5
    }
    
    if (combined == TRUE){
      ystart <- 0
      titleabstract <- paste0('tandaincl [label = \'', paste0(text_wrap(tandaincl_text, 40),
                                                              '\n(n = ',
                                                              tandaincl,
                                                              ')'
      ), "', width = 4, height = 0.5, pos='",4,",",8.5,"!', tooltip = ", tooltips[4], "]
      
      tandaexcl [label = '", paste0(text_wrap(tandaexcl_text, 40),
                                    '\n(n = ',
                                    tandaexcl,
                                    ')'
      ), "', width = 4, height = 0.5, pos='", 9, ",", 8.5, "!', tooltip = '", tooltips[4], "']")
      tanda_edges <- 'deduped->tandaincl;\ntandaincl->tandaexcl;\ntandaincl->ftretr;\n'
      screenboxh <- 7.3
      screenboxy <- 6.9
      searchboxy <- 11.5
    } else {
      ystart <- 1.5
      titleabstract <- paste0('titleincl [label = \'', paste0(text_wrap(titleincl_text, 40),
                                                              '\n(n = ',
                                                              titleincl,
                                                              ')'
      ), "', width = 4, height = 0.5, pos='",4,",", 10,"!', tooltip = ", tooltips[4], "]
      
      titleexcl [label = '", paste0(text_wrap(titleexcl_text, 40),
                                    '\n(n = ',
                                    titleexcl,
                                    ')'
      ), "', width = 4, height = 0.5, pos='", 9, ",", 10, "!', tooltip = '", tooltips[4], "']
      
      abstractincl [label = \'", paste0(text_wrap(abstractincl_text, 40),
                                        '\n(n = ',
                                        abstractincl,
                                        ')'
      ), "', width = 4, height = 0.5, pos='",4,",",8.5,"!', tooltip = ", tooltips[4], "]
      
      abstractexcl [label = '", paste0(text_wrap(abstractexcl_text, 40),
                                       '\n(n = ',
                                       abstractexcl,
                                       ')'
      ), "', width = 4, height = 0.5, pos='", 9, ",", 8.5, "!', tooltip = '", tooltips[4], "']
      ")
      tanda_edges <- 'deduped->titleincl;\ntitleincl->titleexcl;\ntitleincl->abstractincl;\nabstractincl->abstractexcl;\nabstractincl->ftretr;\n'
      screenboxh <- 8.65
      screenboxy <- 7.65
      searchboxy <- 13
    }
    
    #------------------------------------------------------------------------
    x <- DiagrammeR::grViz(
      paste0("digraph TD {
      
      graph[splines=ortho, layout=neato, tooltip = 'Click the boxes for further information', fontsize = 20]
      
      node [shape = box]
      searching [color = LightSteelBlue2, label='', style = 'filled,rounded', pos='0,", searchboxy, "!', width = 0.4, height = 1.4, tooltip = '", tooltips[1], "'];
      screening [color = LightSteelBlue2, label='', style = 'filled,rounded', pos='0,", screenboxy, "!', width = 0.4, height = ", screenboxh, ", tooltip = '", tooltips[1], "'];
      casynth [color = LightSteelBlue2, label='', style = 'filled,rounded', pos='0,1!', width = 0.4, height = 1.4, tooltip = '", tooltips[1], "'];\n
      
      node [shape = box,
            fontname = ", font, ",
            color = ", input_colour, "]
      dbresults [label = '", paste0(text_wrap(dbresults_text, 40),
                                    '\n(n = ',
                                    dbresults,
                                    ')'
      ), "', width = 4, height = 0.5, pos='4,",ystart+11.5,"!', tooltip = '", tooltips[4], "', penwidth = 4]
      
      otherresults [label = '", paste0(text_wrap(otherresults_text, 40),
                                       '\n(n = ',
                                       otherresults,
                                       ')'
      ), "', width = 4, height = 0.5, pos='9,",ystart+11.5,"!', tooltip = '", tooltips[4], "', penwidth = 4]
      
      prescreened [label = '", paste0(text_wrap(prescreened_text, 20),
                                      '\n(n = ',
                                      prescreened,
                                      ')'
      ), "', width = 2.5, height = 0.5, pos='2,4!', tooltip = '", tooltips[4], "', penwidth = 4]
    
      node [shape = box,
            fontname = ", font, ",
            color = ", main_colour, "]
      deduped [label = '", paste0(text_wrap(deduped_text, 40),
                                  '\n(n = ',
                                  deduped,
                                  ')'
      ), "', width = 4, height = 0.5, pos='4,",ystart+10,"!', tooltip = '", tooltips[4], "']
      
      dupesremoved [label = '", paste0(text_wrap(dupesremoved_text, 40),
                                       '\n(n = ',
                                       dupesremoved,
                                       ')'
      ), "', width = 4, height = 0.5, pos='9,",ystart+10,"!', tooltip = '", tooltips[4], "']
    
      ", titleabstract, "
    
      ftretr [label = '", paste0(text_wrap(ftretr_text, 40),
                                 '\n(n = ',
                                 ftretr,
                                 ')'
      ), "', width = 4, height = 0.5, pos='4,7!', tooltip = '", tooltips[4], "']
      
      ftnotretr [label = '", paste0(text_wrap(ftnotretr_text, 40),
                                    '\n(',
                                    paste(paste(ftnotretr[1,], collapse = ', n ='), paste(ftnotretr[2,], collapse = ', n ='), sep = '; '),
                                    ')'
      ), "', width = 4, height = 0.5, pos='9,7!', tooltip = '", tooltips[4], "']  
    
      ftincl [label = '", paste0(text_wrap(ftincl_text, 40),
                                 '\n(n = ',
                                 ftincl,
                                 ')'
      ), "', width = 4, height = 0.5, pos='4,5.5!', tooltip = '", tooltips[4], "']
      
      ftexcl [label = '", paste0(text_wrap(ftexcl_text, 40),
                                 '\n\nExcluded on:\n',
                                 paste(paste0(ftexcl[,1], ' (n=', ftexcl[,2], ')'), collapse = '\n')
      ), "', width = 4, height = 0.5, pos='9,", ftexclh, "!', tooltip = '", tooltips[4], "']  
    
      studart [label = '", paste0(text_wrap(studart_text, 40),
                                  '\n(n = ',
                                  studart[1],
                                  ' / n = ',
                                  studart[2],
                                  ')'
      ), "', width = 4, height = 0.5, pos='4,2.5!', tooltip = '", tooltips[4], "']
      
      node [shape = box,
            fontname = ", font, ",
            color = ", output_colour, "]
      finalmapincl [label = '", paste0(text_wrap(finalmapincl_text, 40),
                                    '\n(n = ',
                                    finalmapincl,
                                    ')'
      ), "', width = 4, height = 0.5, pos='4,1!', tooltip = '", tooltips[4], "', penwidth = 4]
      
      node [shape = square, width = 0, color=White]
      A0 [label = '', width = 0, height = 0, pos='9,", ystart+10.75, "!', tooltip='']
      A1 [label = '', width = 0, height = 0, pos='4,", ystart+10.75, "!', tooltip='']
      A2 [label = '', width = 0, height = 0, pos='4,4!', tooltip='']
      
      edge [color = ", arrow_colour, ", 
            arrowhead = ", arrow_head, ", 
            arrowtail = ", arrow_tail, ", 
            style = filled]
      deduped->dupesremoved;
      ", tanda_edges, "
      ftretr->ftnotretr;
      ftretr->ftincl;
      ftincl->ftexcl;
      prescreened->A2;
      studart->finalmapincl;
      
      edge [color = ", arrow_colour, ", 
            arrowhead = 'none', 
            arrowtail = ", arrow_tail, ", 
            style = filled]
      dbresults->A1;
      otherresults->A0;
      ftincl->A2;
      
      edge [color = ", arrow_colour, ", 
            arrowhead = ", arrow_head, ", 
            arrowtail = 'none', 
            style = filled]
      A1->deduped;      
      A2->studart;
      
      edge [color = ", arrow_colour, ", 
            arrowhead = 'none', 
            arrowtail = 'none', 
            style = filled]
      A0->A1
      
      }"))
    
    if (combined == 'TRUE'){
      insertJS <- function(plot){
        javascript <- htmltools::HTML('
var theDiv = document.getElementById("node1");
theDiv.innerHTML += "<text text-anchor=\'middle\' style=\'transform: rotate(-90deg);\' x=\'805\' y=\'19\' font-family=\'Helvetica,sans-Serif\' font-size=\'14.00\'>Searching</text>";
var theDiv = document.getElementById("node2");
theDiv.innerHTML += "<text text-anchor=\'middle\' style=\'transform: rotate(-90deg);\' x=\'475\' y=\'19\' font-family=\'Helvetica,sans-Serif\' font-size=\'14.00\'>Screening</text>";
var theDiv = document.getElementById("node3");
theDiv.innerHTML += "<text text-anchor=\'middle\' style=\'transform: rotate(-90deg);\' x=\'50\' y=\'19\' font-family=\'Helvetica,sans-Serif\' font-size=\'14.00\'>Synthesis</text>";
                              ')
        htmlwidgets::appendContent(plot, htmlwidgets::onStaticRenderComplete(javascript))
      }
      x <- insertJS(x)
    } else {
      insertJS <- function(plot){
        javascript <- htmltools::HTML('
var theDiv = document.getElementById("node1");
theDiv.innerHTML += "<text text-anchor=\'middle\' style=\'transform: rotate(-90deg);\' x=\'913\' y=\'19\' font-family=\'Helvetica,sans-Serif\' font-size=\'14.00\'>Searching</text>";
var theDiv = document.getElementById("node2");
theDiv.innerHTML += "<text text-anchor=\'middle\' style=\'transform: rotate(-90deg);\' x=\'520\' y=\'19\' font-family=\'Helvetica,sans-Serif\' font-size=\'14.00\'>Screening</text>";
var theDiv = document.getElementById("node3");
theDiv.innerHTML += "<text text-anchor=\'middle\' style=\'transform: rotate(-90deg);\' x=\'50\' y=\'19\' font-family=\'Helvetica,sans-Serif\' font-size=\'14.00\'>Synthesis</text>";
                              ')
        htmlwidgets::appendContent(plot, htmlwidgets::onStaticRenderComplete(javascript))
      }
      x <- insertJS(x)
    }
    
  } else {
    stop('Please specify the evidence synthesis type: review or map')
  }
    
    return(x)
}


#------------------------------------------------------------------------
text_wrap <- function(text, width){
  text <- stringr::str_wrap(text,
                            width = width)
  return(text)
}

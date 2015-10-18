best <- function(state, outcome) {
    ## Read outcome data
    outcomeCM<-read.csv("outcome-of-care-measures.csv",colClasses = "character") # if not using "colClass" option, death rate will be factor class and as.numberic() will get wrong data
    stateName<-outcomeCM[,7] # the State column (no NAs)
    
    stateNameUni<-unique(stateName[!is.na(stateName)])
    state.Name.Order <- stateNameUni[order(stateNameUni)]
    ## Check that state and outcome are valid
    const <- list(outcome.Index=c("heart attack","heart failure","pneumonia"),
                  state=state.Name.Order,
                  dr.index=list(heart.attack=11,heart.failure=17,pneumonia=23)
    )
    
    if(!is.element(state,const$state)){
        #return(sprintf("Error in best(%s, %s) : invalid state",state,outcome))
        stop("invalid state")
    }
    if(!is.element(outcome,const$outcome)){
        #return(sprintf("Error in best(%s, %s) : invalid outcome",state,outcome))
        stop("invalid outcome")
    }
    const.Index <- which(const$outcome.Index==outcome)         # unifiy outcome and " death rate column" using index
    dr <- suppressWarnings(as.numeric(outcomeCM[,const$dr.index[[const.Index]]]))
    orderHospitalAll<-outcomeCM[order(dr,outcomeCM[,2]),]
    orderHospitalAll[orderHospitalAll$State==state,][,2][1]
}
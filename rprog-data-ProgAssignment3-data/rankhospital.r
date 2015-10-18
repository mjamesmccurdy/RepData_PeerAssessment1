rankhospital <- function(state, outcome, num = "best") {
    outcomeCM<-read.csv("outcome-of-care-measures.csv",colClasses = "character") # if not using "colClass" option, death rate will be factor class and as.numberic() will get wrong data
    stateName<-outcomeCM[,7] # the State column (no NAs)
    stateNameUni<-unique(stateName[!is.na(stateName)])
    state.Name.Order <- stateNameUni[order(stateNameUni)]
    const <- list(outcome.Index=c("heart attack","heart failure","pneumonia"),
                  state=state.Name.Order,
                  dr.index=list(heart.attack=11,heart.failure=17,pneumonia=23),
                  num.index=c("best","worst")
    )
    if(!is.element(state,const$state)){
        stop("invalid state")
    }## Check that state and outcome are valid
    if(!is.element(outcome,const$outcome)){
        stop("invalid outcome")
    }## Check that state and outcome are valid
    const.Index <- which(const$outcome.Index==outcome)         # unifiy outcome and " death rate column" using index
    dr <- suppressWarnings(as.numeric(outcomeCM[,const$dr.index[[const.Index]]]))
    dr <- ifelse(rep(num==const$num.index[2],length(dr)),-dr,dr)  # reverse sorting so that no need to know lenthg of dr to get the worst hospital
    num <- ifelse(is.numeric(num),num,1)                          # when num is numberic we keep it untouched, esle set num=1
    orderHospitalAll<-outcomeCM[order(dr,outcomeCM[,2]),]
    orderHospitalAll[orderHospitalAll$State==state,][,2][num]     # 
}
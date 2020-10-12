library(rjson) 
z1 <- structure(c(0L, 1L, 9L, 0L, 9L, 9L, 9L, 9L, 1L, 1L, 0L, 9L, 0L,
1L, 0L, 9L, 9L, 1L, 9L, 9L, 0L, 9L, 0L,
1L, 0L, 9L, 9L, 1L, 9L, 9L), .Dim = c(10L, 3L), .Dimnames = list(
    c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10"), c("GO:0016765",
    "GO:0016740", "G2O:0016740"))) 
 
   
print(z1)


 
updateOnt = function(z) {
    ontFor <- fromJSON(file = "ont.json") 
    ontRev <- fromJSON(file = "ontRev.json") 
    #input - z : matrix of columns labeled With GO ids and rows as distinct proteins    
    annots = colnames(z) #annotation names from columns

    #ensure input is right type
    if(class(z) != 'matrix') {
        print('input to updateOnt not Matrix') 
        return(0)
    } 

    #create skip list to ignore all columns that don't have names in the ontology file 
    #TODO - make sure that colnames is stable (that it stays in order)
    skip=rep(1,length(annots))  
    skipind = 0
    for(col in 1:length(annots)){
        if(is.null( ontFor[annots[col]] [[1]]) ){
            skipind = skipind + 1 
            skip[skipind] = annots[col] 
        }

    }
    if(skipind > 0 ){
        skipind = skipind - 1
        skip = skip[1:skipind ]
    }
    else{
        skip = c()
    }

    if(length(z[1,]) - skipind < 2) {
        print('Too few valid columns for updateOnt function') 
        return(0)
    } 
  
    l = length(annots)
    #create matrix of zeroes to store possible update locations 
    m = matrix(0L,  l, l) 
    mR = matrix(0L, l, l) 

    for(row in 1:length(z[,1])){
        nines = rep(0,l)
        indNines = 0 
        for(col in 1:l){
            if(z[row,col] == 9){
                indNines = indNines + 1
                nines[indNines] = col
            } 
        }
        if(indNines > 0){
            for(col in 1:l){
                if(z[row,col] == 1 ){
                    for(mcol in 1:indNines){
                        m[mcol,col] = 1
                    }    
                }
                else if(z[row,col] == 0 ){
                    for(mcol in 1:indNines){
                        mR[mcol,col] = 1
                    }    
                }  
            } 
        } 
    } 
    for(col in 1:l){
        flag = FALSE
        for(row in 1:l){
            if(m[row,col]==1 & !flag){
                potentials = traverseOnt(annots[col],ontFor)
                flag = TRUE                
                 
            }
            if(flag){
                if(annots[row] %in% potentials){
                    for(zRow in 1:length(z[,1])){
                        if(z[zRow,col] == 1 & z[zRow,row] == 9){
                            z[zRow,row] = 1 
                        }
                    }
                 }
            }     
        } 
    }
     for(col in 1:l){
        flag = FALSE
        for(row in 1:l){
            if(mR[row,col]==1 & !flag){
                potentials = traverseOnt(annots[col],ontRev)
                flag = TRUE                
                 
            }
            if(flag){
                if(annots[row] %in% potentials){
                    for(zRow in 1:length(z[,1])){
                        if(z[zRow,col] == 0 & z[zRow,row] == 9){
                            z[zRow,row] = 0 
                        }
                    }
                 }
            }     
        } 
    }

    return(z)
} 






traverseOnt = function(startAnnot,ont){
 
    maxRes = 3000 #max number of is_a relationships to return
    stack = rep(1,maxRes)  
    indexCurr = 1
    indexLast = 0 

    a = unlist(ont[startAnnot])
    if(length(a) > 0){
        for(entry in 1:length(a)){
            indexLast = indexLast + 1
            x = a[entry]
            stack[indexLast] = x
        } 
    }

    while(indexCurr <= indexLast){
        a = unlist(ont[stack[indexCurr]])  
        if(length(a) > 0){
            #print('1length ok')
            for(entry in 1:length(a)){  
                #print('2loop ok')
                if(!(a[entry] %in% stack)){
                    #print('3not visited')
                    indexLast = indexLast + 1
                    stack[indexLast] = a[entry]
                }
            } 
        }
        indexCurr = indexCurr + 1
    }
    return(stack[1:indexLast])  
 
} 

 results = updateOnt(z1)
 print(results)
 #ontRev <- fromJSON(file = "ontRev.json") 


#traverseOnt('GO:0016740',ontRev)
#[1] "GO:0016765" "GO:0016740" "GO:0003824" "GO:0003674"




 


#previous version of code stored just in case for the moment
 if(FALSE){



    for(row in 1:length(z[,1])){ 

        #update only rows where there is an unknown and a positive or negative experimental value
        if (9 %in% z[row,] & (0 %in% z[row,] | 1 %in% z[row,])) { 
            check = rep(' ',length(z[1,]))  #stores GO ids labeled with 9 in a particular row 
            checkInd = 1
            #for each row find columns to potentially update and store them (columns with 9's in the given row)
            for(col in 1:length(z[row,])){
                if(z[row,col] == 9 & !(annots[col] %in% skip)) {
                     check[checkInd] = annots[col]
                     checkInd = checkInd + 1
                }
            }
            check = check[1:checkInd-1]

            #for each positive experimental evidence (1), check the parent is_a relationship  to see if you can update columns with 9s
            for(col in 1:length(z[row,])){
                if(z[row,col] == 1 & !(annots[col] %in% skip)) {
                    potentials = traverseOnt(annots[col],ontFor)
                    print(length(potentials))
                    for(test in 1:length(check)){
                        if(check[test] %in% potentials) {
                            z[row,check[test]] = 1  
                        }
                    }
                }    
                if(z[row,col] == 0 & !(annots[col] %in% skip)) {
                    potentials = traverseOnt(annots[col],ontRev)
                    print('rev')
                    print(length(potentials))
                    for(test in 1:length(check)){
                        if(check[test] %in% potentials) {
                            z[row,check[test]] = 0  
                        }
                    }                                
                }


                 
            }
        }       
    } 
 }

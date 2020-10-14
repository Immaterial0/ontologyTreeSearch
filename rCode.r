

#TODO - reuse skip index?  
#TODO - add comments
#TODO - add alternate relationships extension
#TODO - see if loop checking is necessary
#TODO - preprocess data to determine size of traversal array to generate



library(rjson) 


updateOnt = function(z) {
    #input z = matrix with column names as GO ids and rows as distinct annotated objects 

    ontFor <- fromJSON(file = "ont.json") #is_a relationships
    ontRev <- fromJSON(file = "ontRev.json") #reverse of is_a relationships
 

    annots = colnames(z) #annotation names from columns
    l = length(annots)




    #ensure input is right type
    if(class(z) != 'matrix') {
        print('input to updateOnt not Matrix') 
        return(0)
    } 

    #create skip list to ignore all columns that don't have names in the ontology file 
    #TODO - make sure that colnames is stable (that it stays in order)
    skip=rep(1,l)  
    skipind = 0
    for(col in 1:l){
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

    if(l - skipind < 2) {
        print('Too few valid columns for updateOnt function') 
        return(0)
    } 
 
    #create matrix of zeroes to store possible update locations 
    m = matrix(0L,  l, l) 
    mR = matrix(0L, l, l) 


    for(row in 1:length(z[,1])){
        nines = rep(0,l)
        indNines = 0 
        for(col in 1:l){ #get list of all nines in a row
            if(z[row,col] == 9){
                indNines = indNines + 1
                nines[indNines] = col
            } 
        }
        if(indNines > 0){
            for(col in 1:l){ #when there are nines in a row, update m and mR if 1 or 0 in a row respectively
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
    for(col in 1:l){ #if m matrix indicates potential updates for an annotation, trace ontology tree and update any relevant annotations
        flag = FALSE
        for(row in 1:l){ #   
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
     for(col in 1:l){ #same as above but for mR
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





#traverses provided json file passed in ont parameter and generates list of 
traverseOnt = function(startAnnot,ont){
    maxRes = strtoi(unlist(ont[startAnnot])[1]) #max number of is_a relationships to return
    stack = rep("GO:0000000",maxRes)  
    indexCurr = 1
    indexLast = 0 

    a = unlist(ont[startAnnot])
    if(length(a) > 0){
        for(entry in 2:length(a)){
            indexLast = indexLast + 1
            x = a[entry]
            stack[indexLast] = x
        } 
    }

    while(indexCurr <= indexLast){
        a = unlist(ont[stack[indexCurr]])  
        if(length(a) > 0){
            #print('1length ok')
            for(entry in 2:length(a)){  
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
    print(indexLast)
    return(stack[1:indexLast])  
 
} 




#simple test of function
testOnt = function(){

z1 <- structure(c(0L, 1L, 9L, 0L, 9L, 9L, 9L, 9L, 1L, 1L, 0L, 9L, 0L, 1L, 0L, 9L, 9L, 1L, 9L, 9L, 0L, 9L, 0L,
1L, 0L, 9L, 9L, 1L, 9L, 9L), 
.Dim = c(10L, 3L), 
.Dimnames = list( c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10"), c("GO:0016765", "GO:0016740", "G2O:0016740"))) 
 
   
print(z1)
results = updateOnt(z1)
print(results)
}
 
 
#testOnt()






#ontRev <- fromJSON(file = "ontRevLen.json")   
#unlist(ontRev['GO:0008150'])[1]



 #test1 = traverseOnt('GO:0008150' ,ontRev)
 #print(length(test1))
#[1] "GO:0016765" "GO:0016740" "GO:0003824" "GO:0003674"

#GO:0008150


 


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

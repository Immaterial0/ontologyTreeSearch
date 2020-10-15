

#TODO - reuse skip index?  
 #  - add alternate relationships extension //make sure it works
#  - see if loop checking is necessary // it is
#  - preprocess data to determine size of traversal array to generate // almost seems slower



library(rjson) 


updateOnt = function(z,r = c()) {
    #input z = matrix with column names as GO ids and rows as distinct annotated objects 
    #input r = array of relationships desired, empty array gives just is_a relations, 'all' gives all, otherwise use code key TODO

    ontFor <- fromJSON(file = "ont.json") #is_a relationships
    ontRev <- fromJSON(file = "ontRev.json") #reverse of is_a relationships
    ontAdd = fromJSON(file = 'r.json') #additional relationships json

    #obs,alt,con,rep


    allFor = c('rp', 'rrp', 'rrn', 'rr', 'ro', 're', 'rh', 'rd', 'i', 'ir', 'ip', 'irp', 'irn', 'ih', 'io', 'id', 'd' )
    if(is.vector(r)){
        for(ele in 1:length(r)){
            if(!(e %in% allFor)){
                print('Invalid tag in relationship array')
                print(e)
                return(0)
            }
        }
    }
    else if(r == 'all'){
        r = allFor
    }
    else{
        print('relationship array is not a vector or "all" ')
        return(0)
    }
    
   
    annots = colnames(z) #annotation names from columns
    l = length(annots)

    #check for obsolete names in column list
    obsolete = ontAdd['obs'][[1]]
    obsnames = names(obsolete)
    consider = ontAdd['con'][[1]]
    connames = names(consider)
    replacedby = ontAdd['rep'][[1]]
    repnames = names(replacedby)
    altid = ontAdd['alt'][[1]]
    altnames = names(altid)

    for(i in 1:l){ #check to make sure this works
        if(annots[i] %in% obsnames){
            print('Obsolete annotation in use')
            if(annots[i] %in% connames){
                for(j in 1:length(consider[annots[i]])){
                    print(paste('consider :',consider[annots[i]][j]))
                }
            }
            if(annots[i] %in% repnames){
                for(j in 1:length(replacedby[annots[i]])){
                    print(paste('replacedby :',replacedby[annots[i]][j]))
                }
            }
        }
    }



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
                potentials = traverseOnt(annots[col],ontFor,r,ontAdd,1)
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
                potentials = traverseOnt(annots[col],ontRev,r,ontAdd,0)
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
traverseOnt = function(startAnnot,ont,r,ontAdd,dir){     
    d = FALSE

    if(dir == 1){
        maxRes = 200 #initial number of is_a relationships to fill array with
        r = c(r,'alt','rep') #automatically do altid and replacedby
        if('d' %in% r) {
            r = r[r != 'd']
            d = TRUE 
        }
    } else{
        maxRes = 3000
        r = c( 'alt','rep')
    }
    stack = rep("GO:0000000",maxRes)  
    indexCurr = 1
    indexLast = 0 
    rl = lengt(r)

    assign('d',ontAdd['d'][[1]]) 


    for(i in 1:length(r)){
        assign(r[i],ontAdd[r[i]][[1]]) 
        if(!is.null(eval(parse(text = r[i]))[[startAnnot]])){
            for(j in 1:length(eval(parse(text = r[i]))[[startAnnot]])){ #is this the right way to do loops here? 
                indexLast = indexLast + 1
                stack[indexLast] = eval(parse(text = r[i]))[[startAnnot]][j]

            }
        }
    }

     


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
        for(i in 1:length(r)){
            if(!is.null(eval(parse(text = r[i]))[[stack[indexCurr]]])){
                for(j in 1:length(eval(parse(text = r[i]))[[stack[indexCurr]]])){ #is this the right way to do loops here? 
                    indexLast = indexLast + 1
                    stack[indexLast] = eval(parse(text = r[i]))[[stack[indexCurr]]][j]

            }
        }
    }
        
        indexCurr = indexCurr + 1

    }
    disjoint = rep(1,30)
    disindex = 0
    for(i in 1:length(stack)) {
        if(!is.null(eval(parse(text = 'd'))[[stack[i]]])){
            for(j in 1:length(eval(parse(text = 'd'))[[stack[i]])) {
                disindex = disindex + 1
                disjoint[disindex] = eval(parse(text = 'd'))[[stack[i]][j]
            }
        }

    }
    if(disindex > 0){ #load revont
        dres = c()
        for(i in 1:disindex){
            q = traverseOnt(disjoint[i],ontRev,r,ontAdd,0)
            dres = c(dres,q)
        }
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

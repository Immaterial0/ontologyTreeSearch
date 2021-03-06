
#Code key for relationships
#Relationships names numbeer of instance in data set (~sept 2020) and tags

#--disjoint_from 30 d

#relationship: 
#--part_of 8517 rp 
#--positively_regulates 3057 rrp
#--negatively_regulates 3079 rrn
#--regulates 3533 rr
#--occurs_in 192 ro
#--ends_during 1 re
#--has_part 776 rh
#--happens_during 8 rd 

#intersection_of: 
#--intersection_of: 11812 i
#--regulates 3561 ir
#--part_of 1926 ip
#--positively_regulates 3056 irp 
#--negatively_regulates 3075 irn
#--has_part 52 ih
#--occurs_in 195 io
#--happens_during 10 id



library(rjson) 


updateOnt = function(z,r = 'none') {
    #input z = matrix with column names as GO ids and rows as distinct annotated objects 
    #input r = array of relationships desired, empty array gives just is_a relations, 'all' gives all, otherwise use code key 

    ontAdd = fromJSON(file = 'r.json')
    ontFor <- fromJSON(file = "ont.json") 

   #ensure input is right type
    if(class(z) != 'matrix') {
        print('input to updateOnt not Matrix') 
        return(0)
    }  
    none = c('rp')
    mainfor = c('rp', 'rrp', 'rrn', 'rr' )
    allFor = c('rp', 'rrp', 'rrn', 'rr', 'ro', 're', 'rh', 'rd', 'i', 'ir', 'ip', 'irp', 'irn', 'ih', 'io', 'id', 'd' )
    if(is.vector(r)){
        if ( r == 'main'){
            r = mainfor
        } else if( r == 'all'){
            r = allFor
        } else if (r == 'none'){
            r = none
        } else {
            for(ele in 1:length(r)){
                if(!(r[ele] %in% allFor)){
                    print('Invalid tag in relationship array for updateOnt function')
                    print(r[ele])
                    return(0)
                }
            }
        }
    } else{
        print('relationship array is not a vector in updateOnt function')
        return(0)
    }
    #print(r)
   #obs,alt,con,rep

    annots = colnames(z) #annotation names from columns
    l = length(annots)
 
    #check for obsolete names in column list
    obsolete = ontAdd['obs'][[1]]
    obsnames = names(obsolete) #names of obsolete 
    consider = ontAdd['con'][[1]]
    connames = names(consider) 
    replacedby = ontAdd['rep'][[1]]
    repnames = names(replacedby)
    altid = ontAdd['alt'][[1]]
    altnames = names(altid)


    #check if any of the annotation names in input matrix are obsolete, output replaced by and consider names if they are
    for(i in 1:l){  
        if(annots[i] %in% obsnames){
            print('Obsolete annotation in use as passed in to the updateOnt function')
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



 

    #create skip list to ignore all columns that don't have names in the ontology file (or alt or replaced by names)       
    skip=rep(1,l)  
    skipind = 0
    for(col in 1:l){
        if(is.null( ontFor[annots[col]] [[1]]) && !( annots[col] %in% repnames) && !(annots[col] %in% altnames)){
            skipind = skipind + 1 
            skip[skipind] = annots[col] 
         }

    }
    if(skipind > 0 ){
        #skipind = skipind - 1
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
    numUpdates = 0

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
                    for(mcol in nines){ #TODO make sure this works
                        m[mcol,col] = 1
                    }    
                }
                else if(z[row,col] == 0 ){
                    for(mcol in nines){
                        mR[mcol,col] = 1
                    }    
                }  
            } 
        } 
    } 
    for(col in 1:l){ #if m matrix indicates potential updates for an annotation, trace ontology tree and update any relevant annotations
        flag = FALSE
        if(!(annots[col] %in% skip)){   
            for(row in 1:l){  
                if(m[row,col]==1 && !flag && !(annots[row] %in% skip)){
                    potentials = traverseOnt(annots[col],1,r)
                    flag = TRUE                
                    
                }
                if(flag){
                    if(annots[row] %in% potentials || sum(replacedby[[annots[row]]] %in% potentials) > 0|| sum(replacedby[[annots[row]]] %in% potentials) > 0 ){
                        for(zRow in 1:length(z[,1])){
                            if(z[zRow,col] == 1 & z[zRow,row] == 9){
                                z[zRow,row] = 1 
                                numUpdates = numUpdates + 1
                            }
                        }
                    }
                }  
            }     
        } 
    }
     for(col in 1:l){ #same as above but for mR
        flag = FALSE
        if(!(annots[col] %in% skip)){  
            for(row in 1:l){
                if(mR[row,col]==1 && !flag && !(annots[row] %in% skip)){
                    potentials = traverseOnt(annots[col],0,r)
                    flag = TRUE                
                    
                }
                if(flag){
                    if(annots[row] %in% potentials || sum(replacedby[[annots[row]]] %in% potentials) > 0|| sum(replacedby[[annots[row]]] %in% potentials) > 0){
                        for(zRow in 1:length(z[,1])){
                            if(z[zRow,col] == 0 & z[zRow,row] == 9){
                                z[zRow,row] = 0 
                                numUpdates = numUpdates + 1
                            }
                        }
                    }
                }     
            } 
        }
    }
    print(paste('total number of updates : ',numUpdates))
    return(z)
} 





#traverses provided json file passed in ont parameter and generates list of 
traverseOnt = function(startAnnot,dir,r=c()){     
    d = FALSE #flag for if disjoint is in set of relationships to test
    ontAdd = fromJSON(file = 'r.json') #JSON of relationships
 
    if(dir == 1){
        ont <- fromJSON(file = "ont.json") 
        maxRes = 200 #initial number of is_a relationships to fill array with
        r = c(r,'alt','rep') #automatically do altid and replacedby

        #disjoint is a special case and is handled seperately
        if('d' %in% r) {
            r = r[r != 'd']
            d = TRUE 
         }

    } else{
        ont <- fromJSON(file = "ontRev.json")   
        maxRes = 3000 
        r = c( 'alt','rep')
    }

    stack = rep("GO:0000000",maxRes)  #not really a stack, more or a queue actually I think  
    indexCurr = 1
    indexLast = 0 
    rl = length(r)


    #Assigns strings in r to nested JSON objects in ontAdd and adds initial relations in r for start annot
    for(j in 1:rl){
        assign(r[j],ontAdd[r[j]][[1]]) 
        if(!is.null(eval(parse(text = r[j]))[[startAnnot]])){
            for(k in 1:length(eval(parse(text = r[j]))[[startAnnot]])){  
                indexLast = indexLast + 1
                stack[indexLast] = eval(parse(text = r[j]))[[startAnnot]][k]

            }
        }
    }  
     

    #adds the initial relationships to stack for the startannot
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

        #a represents the list of objects that the current object being considered in stack has a is_a relationship to
        #we will thus add each of those to the stack (really more of a queue I think)
        if(length(a) > 0){
            for(entry in 1:length(a)){  
                if(!(a[entry] %in% stack)){
                    indexLast = indexLast + 1
                    stack[indexLast] = a[entry]
                }
            } 
        }
        
        #same idea as the loop above, but now it looks at all the additional relationships that are in the r list. 
        for(k in 1:rl){
            if(!is.null(eval(parse(text = r[k]))[[stack[indexCurr]]])){
                for(j in 1:length(eval(parse(text = r[k]))[[stack[indexCurr]]])){ #is this the right way to do loops here? 
                    indexLast = indexLast + 1
                    stack[indexLast] = eval(parse(text = r[k]))[[stack[indexCurr]]][j]

                }
            }
        }
        
        indexCurr = indexCurr + 1
    }
    stack = stack[1:indexLast] 

    if(d == TRUE){
        assign('d',ontAdd['d'][[1]])  
        disjoint = rep(1,30)
        disindex = 0

        #checks each element in stack to see if it has a disjoint value, and adds all disjoint elements for that annotation using inner loop
        for(i in 1:length(stack)) {
            if(!is.null(eval(parse(text = 'd'))[[stack[i]]])){
                for(j in 1:length(eval(parse(text = 'd'))[[stack[i]]])) {
                    disindex = disindex + 1
                    disjoint[disindex] = eval(parse(text = 'd'))[[stack[i]]][j]
                }
            }

        }
         #if we find a disjoint example, we will create 
        dres = c() 
        if(disindex > 0){  
            for(i in 1:disindex){
                q = traverseOnt(disjoint[i],0)
                dres = c(dres,q) #this list can have duplicates 
            }
        }
         stack = c(stack,dres) 
    } 
    #print(indexLast)
    return(stack)   
} 




#simple test of function
testOnt = function(){

 
 z1 <- structure(c(9L,0 ,1L,1,1L,9,9,1L), 
 .Dim = c(4L,2L), 
 .Dimnames = list(c("1","2",'3','4'),c("GO:0001906",'GO:0044848' )))
   
 

print(z1)
results = updateOnt(z1,c('d')  )
print(results)
}
 
 
testOnt() 

if (FALSE){
    z1 <- structure(c(0L, 1L, 9L, 0L, 9L, 9L, 9L, 9L, 1L, 1L, 1, 1, 1, 9, 9, 1, 0, 0, 0, 0, 0, 9L, 0L,
1L, 0L, 9L, 9L, 1L, 9L, 9L,1,1,0,0,1,9,9,1,0,9,1,9,1,1,0,9,0,1,9,9), 
.Dim = c(10L, 5L), 
.Dimnames = list( c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10"), c("GO2:0016765", "GO2:0003690", "GO:0003697",'GO:0003677','GO:0098847'
))) 
}
 

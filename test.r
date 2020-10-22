library(rjson) 

#strings are vectors, length 0 vectors aren't vectors
#class of strings and vectors of strings are the same
#equality comparisons don't work with null? which includes 0 length vectors? 


        ontFor <- fromJSON(file = "ont.json") 

ontFor["GO:0003690"] [[1]]


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
    for(i in 1:rl){
        assign(r[i],ontAdd[r[i]][[1]]) 
        if(!is.null(eval(parse(text = r[i]))[[startAnnot]])){
            for(j in 1:length(eval(parse(text = r[i]))[[startAnnot]])){  
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
        for(i in 1:rl){
            if(!is.null(eval(parse(text = r[i]))[[stack[indexCurr]]])){
                for(j in 1:length(eval(parse(text = r[i]))[[stack[indexCurr]]])){ #is this the right way to do loops here? 
                    indexLast = indexLast + 1
                    stack[indexLast] = eval(parse(text = r[i]))[[stack[indexCurr]]][j]

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
        if(disindex > 0){  
            dres = c()
            for(i in 1:disindex){
                q = traverseOnt(disjoint[i],0)
                dres = c(dres,q) #this list can have duplicates 
            }
        }
        stack = c(stack,dres)
         
    } 
    print(indexLast)
    return(stack)   
} 

 
 #test1 = traverseOnt('GO:0001906' ,1,c('d')  )
 #print( test1 )



#largest
#GO:0008150
    #ontAdd = fromJSON(file = 'r.json') #JSON of relationships

    #altid = ontAdd['alt'][[1]]

    #print(altid['GO:0000050'] )

 #altid[['GO:0000050']] %in% c('a','hi','2') 




typeof(c( ) )
typeof('none')
typeof(c(1))
typeof(c('hi','there'))




if(FALSE){
 q = c(1)
 is.vector(q)
 x = c('a','f','b','c')
x[!(x %in% c('a','b'))]
 
 
    ont <- fromJSON(file = "r.json") #is_a relationships

allFor = c('rp', 'rrp', 'rrn', 'rr', 'ro', 're', 'rh', 'rd', 'i', 'ir', 'ip', 'irp', 'irn', 'ih', 'io', 'id', 'd' )
x = 'rp'
assign(x,ont[x][[1]])

temp = c(1,1,1,1,1) 
  !is.null(eval(parse(text = x))[['GO:0006911']])
eval(parse(text = x))[['GO:0000156']][1]

if(FALSE){
        q = ont['rd'][[1]]
        #print(q) 
        names = names(q)  
        is.list(q) 
        if( 'GO:0031569' %in% names){
            print('in q')
        } else{
            print('not in q')
        }
        r = unlist(q)
        'GO:0031569' %in% r
         q['GO:0031569']  
        typeof(q)
        q = unlist(q)
        typeof(q)
        length(q)
        q
        split(q,' ')
            #print(q)
}
}




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

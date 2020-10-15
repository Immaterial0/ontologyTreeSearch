library(rjson) 
 
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
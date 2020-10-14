let fs = require('fs');
const { exit } = require('process');
let go = fs.readFileSync('ontRev.JSON', 'utf8');
let ont = JSON.parse(go)
let ok = Object.keys(ont)

let maxArr = []
let idmax = ''
let dist = []
dist.length = ok.length

for(let i = 0,j=0,k=0;i<ok.length;i++){
    id = ok[i]
    temp = []
    //if(i%1000 == 0) console.log(i)
    for(j = 0;j<ont[ok[i]].length;j++){
        /*if(temp.includes(ont[ok[i]][j])){
            //console.log('loop detected')
 
        }  
        else{ */
            temp.push(ont[ok[i]][j])
 
        //}
    }
    for(k=0;k<temp.length;k++){
         if(!(temp[k] in ont)){
            //console.log(temp,temp[k],'not in ont')
            continue
        } 
        for(j = 0;j<ont[temp[k]].length;j++){
           /* if(temp.includes(ont[temp[k]][j])){
                //console.log(temp,'loop detected',ont[temp[k]][j])
               // continue
    
            }  
            else{ */
                temp.push(ont[temp[k]][j])
     
           // }
        }
        if(k > 50000000){
            console.log('over 1M',temp.slice(-30,-1))
            exit()
        }
    }        
    if(temp.length > maxArr.length) maxArr = temp, idmax = id  
    dist[i] = temp.length

}



console.log(maxArr.length,idmax)

function saveLengths(){
    for(let i = 0;i<ok.length;i++){
        ont[ok[i]].unshift(dist[i])

    }

    ontJSON = JSON.stringify(ont)

    fs.writeFile("ontRevLen.json", ontJSON	, function(err) {
        if (err) {
            console.log(err);
        }
    });
}
//loops do indeed exist in this set seemingly, perhaps consider if there is a reason. 


/*
//test the length by reading up the tree in reverse direction - much slower but another way to verify that tree size is correct by different method

t = ['GO:0008150']

for(let k = 0;k<t.length;k++){
    for(let i = 0;i<ok.length;i++) {
        for(j=0;j<ont[ok[i]].length;j++){
            if(ont[ok[i]][j] == t[k] && !t.includes(ok[i])){
                t.push(ok[i])
            }     

        }

    }
    console.log(t.length)
}
console.log(t.length)

*/
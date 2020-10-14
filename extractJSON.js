var fs = require('fs');
 
go = fs.readFileSync('go.obo', 'utf8');
 
 //console.log(go.substring(0, 10000))
  goArr = go.split('\n\n')
 console.log(goArr[1].split('\n').length)
 
 
 atrID = {}
 count = 0 
 countmatch = 0
 obsolete = 0
 countloop1 = 0
 countOntLinks = 0
 
 ont = {}

 r = {}
 rnames = ['rp','rrp','rrn','rr','ro','re','rh','rd']
 rtags = ['relationship: part_of GO:', 
 'relationship: positively_regulates GO:',
 'relationship: negatively_regulates GO:',
 'relationship: regulates GO:',
 'relationship: occurs_in GO:', 
 'relationship: ends_during GO:',  
 'relationship: has_part GO:',
 'relationship: happens_during GO:'
]
rcount = [0,0,0,0,0,0,0,0]

inter = {}
inames = ['i','ir','ip','irp','irn','ih','io','id']
itags = ['intersection_of: GO:',
 	 'intersection_of: regulates GO:',
	'intersection_of: part_of GO:',
	'intersection_of: positively_regulates GO:',
	'intersection_of: negatively_regulates GO:',
	'intersection_of: has_part GO:',
	'intersection_of: occurs_in GO:',
	'intersection_of: happens_during GO:' 
]
icount = [0,0,0,0,0,0,0,0]

d = {}

altid = {}

obsolete = {}

consider = {}

replaced = {}


for(let i = 0;i<rnames.length;i++){
	r[rnames[i]] = {}
}

for(let j = 0;j<inames.length;j++){
	inter[inames[j]] = {}
}

 for (let i = 1,j=1;i<goArr.length;i++){
 	temp = goArr[i].split('\n')
	if(temp[0].search(/\[Term\]/) > -1){  
 		//countloop1++
	
		t = temp[1].match(/GO:\d+/) 
		if(t == null) {
			console.log(goArr[i])
			//break
		}
		ont[t[0]] = [ ] 
	}
	else{
		continue
	}
	for (j = 1;j<temp.length;j++) {
		countmatch++
		 
		
		matches = temp[j].match(/^[\w: ]+GO:/)
 		if(matches && matches[0]=='is_a:'){
			ont[t[0]].push(temp[j].match(/GO:\d+/)[0])
			countOntLinks++
		 }
		

		for(g = 0;g<rtags.length;g++){
			if(matches && matches[0]==rtags[g]){
				if(t[0] in r[rnames[g]]){
					r[rnames[g]][t[0]].push(temp[j].match(/GO:\d+/)[0])
				}
				else{
					r[rnames[g]][t[0]] = [ temp[j].match(/GO:\d+/)[0] ]
				}
				rcount[g]++
			}

		}

		for(g = 0;g<itags.length;g++){
			if(matches && matches[0]==itags[g]){
				if(t[0] in inter[inames[g]]){
					inter[inames[g]][t[0]].push(temp[j].match(/GO:\d+/)[0])
				}
				else{
					inter[inames[g]][t[0]] = [ temp[j].match(/GO:\d+/)[0] ]
				}
				icount[g]++
			}

		}
		 
		if(matches && matches[0]=='disjoint_from: GO:'){
			if(t[0] in d){
				d[t[0]].push(temp[j].match(/GO:\d+/)[0])
			}
			else{
				d[t[0]] = [ temp[j].match(/GO:\d+/)[0] ]
			} 		 }
		
			if(matches && matches[0]=='alt_id: GO:'){
				if(t[0] in altid){
					altid[t[0]].push(temp[j].match(/GO:\d+/)[0])
				}
				else{
					altid[t[0]] = [ temp[j].match(/GO:\d+/)[0] ]
				} 		 }
			if(matches && matches[0]=='consider: GO:'){
				if(t[0] in consider){
					consider[t[0]].push(temp[j].match(/GO:\d+/)[0])
				}
				else{
					consider[t[0]] = [ temp[j].match(/GO:\d+/)[0] ]
				} 		 }
			if(matches && matches[0]=='replaced_by: GO:'){
			
				if(t[0] in replaced){
					replaced[t[0]].push(temp[j].match(/GO:\d+/)[0])
				}
				else{
					replaced[t[0]] = [ temp[j].match(/GO:\d+/)[0] ]
				} 		
			}
			matches2 = temp[j].match(/^is_obsolete:/)	
			if(matches2 && matches2[0]=='is_obsolete:'){
				obsolete[t[0]] = 1
			}
				
		if(matches && !atrID[matches[0]]){
			
			atrID[matches[0]] = 1
			count++
		}
	}
 	
 }
 console.log(icount)
 ontRev = {}
 
 ontKeys = Object.keys(ont)
 for(i = 0;i<ontKeys.length;i++){
 	for(j = 0;j<ont[ontKeys[i]].length;j++){
		if(!ontRev[ont[ontKeys[i]][j]]) {
			ontRev[ont[ontKeys[i]][j]] = []
		}	 
		ontRev[ont[ontKeys[i]][j]].push(ontKeys[i])
	} 
 
 }
 
 //number terms, number of lines under all terms total, number attr names for terms, number of as_if links
 //console.log(countloop1,countmatch,count,countOntLinks)
 //console.log(atrID)
 
 rJSON = JSON.stringify(r)

 fs.writeFile("r.json", rJSON, function(err) {
	if (err) {
		console.log(err);
	}
}); 

iJSON = JSON.stringify(inter)

fs.writeFile("i.json", iJSON, function(err) {
   if (err) {
	   console.log(err);
   }
}); 
dJSON = JSON.stringify(d)

fs.writeFile("d.json", dJSON, function(err) {
   if (err) {
	   console.log(err);
   }
}); 

altidJSON = JSON.stringify(altid)


fs.writeFile("altid.json", altidJSON, function(err) {
	if (err) {
		console.log(err);
	}
 }); 

 replacedJSON = JSON.stringify(replaced)


 fs.writeFile("replaced.json", replacedJSON, function(err) {
	 if (err) {
		 console.log(err);
	 }
  }); 

  considerJSON = JSON.stringify(consider)


fs.writeFile("consider.json", considerJSON, function(err) {
	if (err) {
		console.log(err);
	}
 }); 
 obsoleteJSON = JSON.stringify(obsolete)


fs.writeFile("obsolete.json", obsoleteJSON, function(err) {
	if (err) {
		console.log(err);
	}
 }); 



 function saveont(){
 ontJSON = JSON.stringify(ont)
 ontRevJSON = JSON.stringify(ontRev)
 
 fs.writeFile("ont.json", ontJSON, function(err) {
     if (err) {
         console.log(err);
     }
 }); 
 
 fs.writeFile("ontRev.json", ontRevJSON	, function(err) {
     if (err) {
         console.log(err);
     }
 });

}
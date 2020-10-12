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
 
 for (let i = 1,j=1;i<goArr.length;i++){
 	temp = goArr[i].split('\n')
	if(temp[0].search(/\[Term\]/) > -1){  
 		countloop1++
	
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
		 
		
		matches = temp[j].match(/^\w+:/)
		if(matches && matches[0]=='is_a:'){
			ont[t[0]].push(temp[j].match(/GO:\d+/)[0])
			countOntLinks++
	 	}	 
		
		if(matches && !atrID[matches[0]]){
			
			atrID[matches[0]] = 1
			count++
		}
	}
 	
 }
 
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
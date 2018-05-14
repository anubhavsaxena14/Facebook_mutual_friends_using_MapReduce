mapFinal=function(k,val){
    v2=c();
    v3=c();
    for(v in val){
        l1=strsplit(v,'->');
        v1=unlist(l1);
        
        
        mainFrnd=v1[1];
        friends=strsplit(v1[2],'\\s');
        
        friends.vector=unlist(friends);
        friends.vector=sort(friends.vector);
        
        frndStr=paste(friends.vector,collapse=' ');
        
        for(frn in friends.vector){
            if(frn>mainFrnd){
                s=paste(mainFrnd,' ',frn);
                v2=c(v2,s);
                v3=c(v3,frndStr);
            }else{
                s=paste(frn,' ',mainFrnd);
                v2=c(v2,s);
                v3=c(v3,frndStr);
            }
        }
    }
    return(keyval(v2,v3));
    
}


reduceFinal=function(k,va){
	val=unlist(va);
	frnd=c();
	v1=strsplit(val[1],'\\s');
	v1=unlist(v1);
	v2=strsplit(val[2],'\\s');
	v2=unlist(v2);
	
	for(v in v1){
		if(v %in% v2){
			frnd=paste(frnd,v);
		}
		
		
	}
	return(keyval(k,frnd));
}

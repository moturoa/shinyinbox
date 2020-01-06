Shiny.addCustomMessageHandler("selectedMessages", function(data){
  
  let checked = $("#"+data.inbox_id+" input:checked")
                .map(function() { return this.id; })
                .get(); 
                
  Shiny.onInputChange(data.shiny_id, checked);
  
});

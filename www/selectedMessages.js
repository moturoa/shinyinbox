Shiny.addCustomMessageHandler("selectedMessages", function(data){
  
  let checked = $("#box1-table_inbox input:checked")
                .map(function() { return this.id; })
                .get(); 
                
  Shiny.onInputChange(data.shiny_id, checked);
  
});

$(document).ready(function(){
	

  $("*").on("click", function(){

  		let checkboxes = document.querySelectorAll("input[type=checkbox]:checked");

  		let vals = [];
  		for(let i = 0; i < checkboxes.length; i++){

  			vals[i] = checkboxes[i].id;

  		};

      Shiny.onInputChange("main_inbox-checkedrows", vals);
      Shiny.onInputChange("user_inbox-checkedrows", vals);
      Shiny.onInputChange("pand_inbox-checkedrows", vals);

  });


});

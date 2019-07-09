$(document).ready(function(){
  $("#steps").on("stepper:selection-changed", function(event, selected) {
    var help = $("#help");
    var newSection = help.find("#help-" + selected);
    help.find(".help-section").not(newSection).removeClass("active");
    newSection.addClass("active");
  });
  
  $("#help-toggle").on("click", function(event) {
    $("#help").toggle();
  });
});


$(document).ready(function(){
  // Update help section when setup stepper changes
  $("#steps").on("stepper:selection-changed", function(event, selected) {
    var help = $("#help");
    var newSection = help.find("#help-" + selected);
    help.find(".help-section").not(newSection).removeClass("active");
    newSection.addClass("active");
  });
  
  // Help button toggles help content
  $("#help-button").on("click", function(event) {
    $("#page").toggleClass("help-on");
    $(window).trigger("resize"); // Force shiny to resize plots
  });
});

Shiny.addCustomMessageHandler("activate_tab", function(message) {
  $("#" + message.tab).tab("show");
});


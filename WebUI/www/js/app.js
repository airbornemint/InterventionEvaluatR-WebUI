function showHelpSection(section) {
    var help = $("#help");
    var newSection = help.find("#help-" + section);
    help.find(".help-section").not(newSection).removeClass("active");
    newSection.addClass("active");
}

$(document).ready(function(){
  // Update help section when setup stepper changes
  $("#steps").on("stepper:selection-changed", function(event, selected) {
    showHelpSection(selected);
  });
  
  // Help button toggles help content
  $("#help-button").on("click", function(event) {
    $("#page").toggleClass("help-on");
    $(window).trigger("resize"); // Force shiny to resize plots
  });
  
  $('a[data-toggle="tab"]').on("show.bs.tab", function(event) {
    if ($(event.target).attr("id") == "nav-results-tab") {
      showHelpSection("results");
    } else {
      showHelpSection($("#nav-setup ul.stepper > li.active").attr("id"));
    }
  });
});

Shiny.addCustomMessageHandler("activate_tab", function(message) {
  $("#" + message.tab).tab("show");
});


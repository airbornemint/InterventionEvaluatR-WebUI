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

function updateProgress(progress) {
  // In here, items is a dict whose keys are unique IDs for progress items, and whose values are dict(name, done)
  // For each item, we find an existing progress item with that key, creating if necessary, then update the name and the doneness
  var progressList = $("#analysis-progress");
  progress.order.forEach(function(id, idx) {
    var item = progress.items[id];
    id = "progress-" + id;
    var itemElt = $("#" + id);
    if (!itemElt.length) {
      // Item doesn't exist, create
      itemElt = $("<li class='progress-item list-group-item d-flex justify-content-between align-items-center' />").attr("id", id).append(
        "<span class='name'/><span class='badge badge-warning badge-pill progress-waiting'><i class='fas fa-clock' aria-hidden='true'></i></span><span class='badge badge-success badge-pill progress-done'><i class='fas fa-check' aria-hidden='true'></i></span>"
      ).append(
        '<span class="spinner-border spinner-border-sm text-primary progress-current" role="status"><span class="sr-only">In progress…</span></span>'
      );
      
      // Insert at the correct place
      var children = progressList.children();
      if (idx < children.length) {
        children.eq(idx).before(itemElt);
      } else {
        progressList.append(itemElt);
      }
    }

    // Item exists, update name and doneness
    if ('name' in item) {
      itemElt.find(".name").text(item.name);
    }
    if ('done' in item) {
      itemElt.toggleClass('done', item.done).removeClass('waiting');
    } else {
      itemElt.addClass('waiting');
    }
  });
  
}

// This kludge is explained in results.R
setInterval(function(){
    $.ajax({ url: window.location.href.split('#')[0] + "/.session-data/" + Shiny.shinyapp.config.sessionId
 + "/progress.json", success: function(data){
        updateProgress(data);
    }, dataType: "json"});
}, 5000);


// Shiny.addCustomMessageHandler("update_analysis_progress", function(message) {
//   updateProgress(message.items);
// });


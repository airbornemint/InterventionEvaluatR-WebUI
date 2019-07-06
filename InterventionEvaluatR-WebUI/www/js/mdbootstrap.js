// Stepper input bindings
var stepperBinding = new Shiny.InputBinding();

$.extend(stepperBinding, {
  find: function(scope) {
    return $(scope).find("ul.stepper");
  },

  initialize: function(el){
    this.setValue(el, $(el).data("stepper-selected"));
  },
  
  getValue: function(el) {
    return $(el).data("stepper-selected");
  },

  setValue: function(el, value) {
    var step = $(el).children("li#" + value);
    this.setStep($(el), step);
  },

  setStep: function(el, step) {
    el.data("stepper-selected", step.attr("id"));
    el.children("li").not(step).removeClass("active");
    step.addClass("active");
  },

  subscribe: function(el, callback) {   
    var bindings = this;
    $(el).find("> li > a").on('click.mdb-stepper', function(event) {
      var step = $(event.target).closest("ul.stepper > li");
      bindings.setStep($(el), step);
      callback(false);
    });
  },
  
  unsubscribe: function(el, callback) {   
      $(el).children("li > a").off('.mdb-stepper');
  }
});

Shiny.inputBindings.register(stepperBinding);

Shiny.addCustomMessageHandler("md_update_stepper", function(message) {
  if (message.value) {
    stepperBinding.setValue($("#" + message.stepper), message.value);
  }
});

Shiny.addCustomMessageHandler("md_update_stepper_step", function(message) {
  if (message.completed !== null) {
    $("#" + message.stepper).children("#" + message.step).toggleClass("completed", message.completed);
  }
});

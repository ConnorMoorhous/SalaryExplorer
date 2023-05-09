$(document).on("click", ".nav-btn", function(evt) {

    var el = $(evt.target)

    el.trigger("change")

  });

var navInputBinding = new Shiny.InputBinding()
$.extend(navInputBinding, {
  find: function(scope) {
    return $(scope).find("#nav")
  },
  getValue: function(el) {
    return $(el).find("input:checked").attr("value")
  },
  setValue: function(el, value) {
    
  },
  subscribe: function(el, callback) {
    $(el).on("change.navInputBinding", function(e) {
      callback()
    })
  },
  unsubscribe: function(el) {
    $(el).off(".navInputBinding")
  }
})

Shiny.inputBindings.register(navInputBinding)
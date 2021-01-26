shinyjs.emphasize = function(params) {
  $(params.selector).addClass("emphasizeable").addClass("emphasized");
  setTimeout(function() {
    $(params.selector).removeClass("emphasized");
  }, 1500);
};

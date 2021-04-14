$(() => {
  $(document).on("click", ".reset-numeric-input", e => {
    let $el = $(e.target).closest(".flex-container").find("input");
    let binding = $el.data("shiny-input-binding");
    binding.setValue($el[0], $el.attr("value"));
    $el.trigger("change");
  });
});

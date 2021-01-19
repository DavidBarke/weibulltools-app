$(".r-function").on("click", ".up-down-btn", function(e) {
  console.log("x");
  $(this).find("i").toggleClass("fa-chevron-down");
  $(this).find("i").toggleClass("fa-chevron-up");
  $(this).parents(".r-function").find(".r-function-body").slideToggle();
});

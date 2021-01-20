$('.r-function').on('click', '.up-down-btn', function(e) {
  $(this).find('i').toggleClass('fa-chevron-down');
  $(this).find('i').toggleClass('fa-chevron-up');
  rFunction = $(this).parents('.r-function');
  rFunction.find('.r-function-body').slideToggle();
  rFunction.find('.r-function-placeholder').slideToggle();
});

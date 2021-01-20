$(function() {
  $('[data-toggle="popover-click"]').popover();
  $('[data-toggle="popover-click"]').on("shown.bs.popover", function() {
    setTimeout(function() {
      $('[data-toggle="popover-click"]').popover('hide');
    }, 1000);
  });
});

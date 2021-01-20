bindPopoverClick = function() {
  $('[data-toggle="popover-click"]').popover();
  $('[data-toggle="popover-click"]').on("shown.bs.popover", function() {
    setTimeout(function() {
      $('[data-toggle="popover-click"]').popover('hide');
    }, 1000);
  });
};

bindTooltipHover = function() {
  $('[data-toggle="tooltip-hover"]').tooltip({
    boundary: 'window'
  });
};

$(bindPopoverClick);
$(bindPopoverHover);
$(bindTooltipHover);

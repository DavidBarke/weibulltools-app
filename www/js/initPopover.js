bindPopoverClick = function() {
  $('[data-toggle="popover-click"]').popover();
  $('[data-toggle="popover-click"]').off("shown.bs.popover").on("shown.bs.popover", function() {
    setTimeout(function() {
      $('[data-toggle="popover-click"]').popover('hide');
    }, 1000);
  });
};

bindTooltipHover = function() {
  $('[data-toggle="tooltip-hover"]').tooltip({
    boundary: 'window'
  });

  // Hide tooltip on next click. This is necessary to remove the tooltip from
  // a dropdown button.
  $('[data-toggle="tooltip-hover"]').off("shown.bs.tooltip").on("shown.bs.tooltip", function() {
    $(document).one("click", function() {
      $('[data-toggle="tooltip-hover"]').tooltip("hide");
    });
  });
};

$(bindPopoverClick);
$(bindTooltipHover);

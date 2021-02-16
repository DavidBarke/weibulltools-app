resizeIframe = function(obj) {
  obj.style.height = obj.contentWindow.document.documentElement.offsetHeight + 'px';
};

shinyjs.bindResizeIframe = function(params) {
  $(window).on('resize', function() {
    $('#' + params.id).each(function() {
      resizeIframe(this);
    });
  });

  $(window).on('scroll', function() {
    $('#' + params.id).each(function() {
      resizeIframe(this);
    });
  });
};

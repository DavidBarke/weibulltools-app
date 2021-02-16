resizeIframe = function(obj) {
  obj.style.height = obj.contentWindow.document.documentElement.offsetHeight + 'px';
};

$(window).on('resize', function() {
  $('iframe').each(function() {
    resizeIframe(this);
  });
});

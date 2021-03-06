// Taken from pkgdown: https://github.com/r-lib/pkgdown/blob/73a84ad8af9a5763b83266fd649f90b429297e55/inst/assets/BS3/pkgdown.js#L72-L106
function changeTooltipMessage(element, msg) {
  var tooltipOriginalTitle=element.getAttribute('data-original-title');
  element.setAttribute('data-original-title', msg);
  $(element).tooltip('show');
  element.setAttribute('data-original-title', tooltipOriginalTitle);
}

if(Clipboard.isSupported()) {
  $(document).ready(function() {
    var copyButton = "<button type='button' class='btn btn-primary btn-copy-ex' type = 'submit' title='Copy to clipboard' aria-hidden='true' data-toggle='tooltip' data-placement='left' data-trigger='hover' data-clipboard-copy><i class='fa fa-copy' aria-hidden='true'></i></button>";

    $("pre.r").addClass("hasCopyButton");

    // Insert copy buttons:
    $(copyButton).prependTo(".hasCopyButton");

    // Initialize tooltips:
    $('.btn-copy-ex').tooltip({container: 'body'});

    // Initialize clipboard:
    var clipboardBtnCopies = new Clipboard('[data-clipboard-copy]', {
      text: function(trigger) {
        return trigger.parentNode.textContent;
      }
    });

    clipboardBtnCopies.on('success', function(e) {
      changeTooltipMessage(e.trigger, 'Copied!');
      e.clearSelection();
    });

    clipboardBtnCopies.on('error', function() {
      changeTooltipMessage(e.trigger,'Press Ctrl+C or Command+C to copy');
    });
  });
}

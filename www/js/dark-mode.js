$(document).on('shiny:connected', function() {
  const isDark = window.matchMedia('(prefers-color-scheme: dark)').matches;

  if (!$('body').hasClass('dark-mode') && !isDark) {
    $('#customSwitch1').click();
  }
});

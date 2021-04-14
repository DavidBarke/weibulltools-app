$(document).on('shiny:connected', function() {
  darkCookie = Cookies.get("dark-mode");

  var isDark;
  if (darkCookie !== undefined) {
    // Use cookie if present
    isDark = darkCookie == "true";
  } else {
    // Fall back on user preference
    isDark = window.matchMedia('(prefers-color-scheme: dark)').matches;
  }

  if (!$('body').hasClass('dark-mode') && !isDark) {
    $('#customSwitch1').click();
  }
});

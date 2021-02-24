shinyjs.getCookie = function(params) {
  var cookie = Cookies.get(params.cookie);
  Shiny.setInputValue(params.id, cookie);
};

shinyjs.setCookie = function(params) {
  Cookies.set(params.cookie, params.value, {
    sameSite: 'strict',
    expires: 1
  });
  Shiny.setInputValue(params.id, params.value);
};

shinyjs.rmCookie = function(params) {
  Cookies.remove(params.cookie, {
    sameSite: 'strict',
    expires: 1
  });
  Shiny.setInputValue(params.id, undefined);
};

getSidebarBinding = function() {
  return Shiny.inputBindings.bindings[3].binding;
};

bindEmphasizeVarname = function() {
  $(".varname-link").off("click.emphasize").on("click.emphasize", function() {
    tabName = $(this).attr("tab-name");
    sidebarBinding = getSidebarBinding();
    sidebarBinding.setValue(sidebarBinding.find(document), tabName);

    varname = $(this).attr("varname");
    let varEl = $(".r-function-varname[name=" + varname + "]");
    varEl.addClass("emphasized");
    setTimeout(function() {
      varEl.removeClass("emphasized");
    }, 1500);
  });
};

bindEmphasizeReferences = function() {
  $(".ref-link").off("click.emphasize").on("click.emphasize", function() {
    tabName = $(this).attr("tab-name");
    sidebarBinding = getSidebarBinding();
    sidebarBinding.setValue(sidebarBinding.find(document), tabName);

    reference = $(this).attr("reference");
    let varEl = $(".r-function-name[name=" + reference + "]");
    varEl.addClass("emphasized");
    setTimeout(function() {
      varEl.removeClass("emphasized");
    }, 1500);
  });
};

$(bindEmphasizeVarname);
$(bindEmphasizeReferences);


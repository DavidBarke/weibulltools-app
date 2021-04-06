updateConnectedTabBox = function() {
  $(".nav-tabs[connected-id]").on("click", function(e) {
    let that = this;
    // Wait minimal amount of time so that .active is update beforehand
    setTimeout(function() {
      let id = $(that).attr("connected-id");
      let activeValue = $(that).find(".active").attr("data-value");
      $("#" + id).find('[data-value="' + activeValue + '"').tab("show");
    }, 0);
  });
};

$(updateConnectedTabBox);

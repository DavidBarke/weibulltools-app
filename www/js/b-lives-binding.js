var binding = new Shiny.InputBinding();

$.extend(binding, {

  find: function(scope) {
    return $(scope).find(".b-lives-input");
  },

  initialize: function(el) {
    console.log("init");
    let initValues = $(el).attr("data-value").split(",").map(parseFloat);
    $(el).data("value", initValues);

    this.renderBadges(el, initValues);
  },

  getValue: function(el) {
    return $(el).data("value");
  },

  setValue: function(el, data) {
    $(el).data("value", data.value);

    this.renderBadges(el, data.value);

    $(el).trigger("update");
  },

  receiveMessage: function(el, data) {
    this.setValue(el, data);
  },

  subscribe: function(el, callback) {
    $(el).on("update.b-lives", function(e) {
      callback();
    });
  },

  unsubscribe: function(el) {
    $(el).off("b-lives");
  },

  renderBadges: function(el, values) {
    $(el).empty();
    values.map(value => {
      $(el).append(this.badge(value));
    });
  },

  badge: function(value) {
    return `<span class="left badge badge-primary">${value}</span> `;
  }

});

Shiny.inputBindings.register(binding, "b-lives-input");

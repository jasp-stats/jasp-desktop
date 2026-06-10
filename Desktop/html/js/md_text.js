JASPWidgets.md_text = Backbone.Model.extend({
  defaults: {
    content: "",
    name: "",
  },
});

JASPWidgets.md_textView = JASPWidgets.objectView.extend({
  initialize: function () {
    this._mdTextViewBase = JASPWidgets.objectView.prototype;
    this._mdTextViewBase.initialize.call(this);
    this.$el.addClass("jasp-md-text");
  },

  render: function () {
    var raw = this.model.get("content");
    var html =
      typeof marked !== "undefined"
        ? marked.parse(raw)
        : raw.replace(/</g, "&lt;").replace(/\n/g, "<br>");
    this.$el.html(html);
    return this;
  },

  // ---- Required toolbar / menu interface --------------------------
  hasNotes: function () {
    return false;
  },
  hasCopy: function () {
    return true;
  },
  hasCitation: function () {
    return false;
  },
  hasEditTitle: function () {
    return false;
  },
  hasRemove: function () {
    return false;
  },

  copyMenuClicked: function () {
    var exportParams = new JASPWidgets.Exporter.params();
    exportParams.format = JASPWidgets.ExportProperties.format.html;
    exportParams.process = JASPWidgets.ExportProperties.process.copy;
    exportParams.htmlImageFormat =
      JASPWidgets.ExportProperties.htmlImageFormat.temporary;
    exportParams.includeNotes = false;
    this.exportBegin(exportParams);
    return true;
  },

  exportBegin: function (exportParams, completedCallback) {
    if (exportParams === undefined)
      exportParams = new JASPWidgets.Exporter.params();
    else if (exportParams.error) return false;

    var html = this.$el.html();
    var text = this.$el.text();
    var exportContent = new JASPWidgets.Exporter.data(text, html);

    if (completedCallback !== undefined)
      completedCallback.call(this, exportParams, exportContent);
    else this.exportComplete(exportParams, exportContent);

    return true;
  },

  exportComplete: function (exportParams, exportContent) {
    if (!exportParams.error) pushHTMLToClipboard(exportContent, exportParams);
  },

  /** md_text blocks use the objectView render path but have no sub-views.
     Override constructChildren to prevent the inherited one from crashing
     (md_text meta entries have no nested 'meta' array). */
  constructChildren: function (constructor, data) {
    // no-op: md_text has no children
  },
  hasViews: function () {
    return true; // prevent objectConstructor from closing us
  },
});

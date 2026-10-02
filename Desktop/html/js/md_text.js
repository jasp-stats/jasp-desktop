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
    this.editing = false;
  },

  events: {
    dblclick: "_startEdit",
  },

  _startEdit: function (e) {
    if (this.editing) return;
    this.editing = true;

    var raw = this.model.get("content");
    var self = this;

    // Build editing surface — textarea styled like Quill's Snow theme
    var wrapper = document.createElement("div");
    wrapper.style.cssText = "border:1px solid #ccc; border-radius:3px;";

    var toolbar = document.createElement("div");
    toolbar.style.cssText =
      "padding:4px 8px; background:#f5f6f6; border-bottom:1px solid #ccc; " +
      "font-size:11px; color:#888; font-family:sans-serif;";
    toolbar.textContent =
      i18n("Markdown \u2022 click away or Ctrl+Enter to save \u2022 Esc to cancel");

    var textarea = document.createElement("textarea");
    textarea.className = "jasp-md-text-editor";
    textarea.value = raw;
    textarea.style.cssText =
      "width:100%; min-height:10em; border:none; outline:none; " +
      "font-family:monospace; font-size:12px; padding:0.5em; " +
      "background:#fafafa; line-height:1.5; resize:vertical;";

    wrapper.appendChild(toolbar);
    wrapper.appendChild(textarea);
    this.$el.empty().append(wrapper);
    textarea.focus();

    var save = function () {
      if (!self.editing) return;
      var newContent = textarea.value;
      self.model.set("content", newContent);
      self.editing = false;
      self.render();
      self.trigger("contentChanged");
    };

    textarea.addEventListener("blur", save);
    textarea.addEventListener("keydown", function (ev) {
      if (ev.key === "Escape") {
        self.editing = false;
        self.render();
      }
      if (ev.key === "Enter" && (ev.ctrlKey || ev.metaKey)) {
        ev.preventDefault();
        save();
      }
    });
  },

  render: function () {
    if (this.editing) return this;
    var raw = this.model.get("content");
    var html =
      typeof marked !== "undefined"
        ? marked.parse(raw)
        : raw.replace(/</g, "&lt;").replace(/\n/g, "<br>");
    this.$el.html(html);

    if (typeof enhanceMarkdownTables === "function") {
      enhanceMarkdownTables(this.$el[0]);
    }

    // fix:https://github.com/jasp-stats/INTERNAL-jasp/issues/3294
    if (typeof MathJax !== "undefined" && MathJax.typesetPromise) {
      var el = this.el;
      MathJax.typesetClear([el]);
      setTimeout(function () {
          MathJax.typesetPromise([el]).catch(function (err) {
              console.warn("MathJax typeset notice:", err);
          });
      }, 0);
    }

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

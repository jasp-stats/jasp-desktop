JASPWidgets.image = JASPWidgets.Resizeable.extend({

	defaults: {
		title: "",
		width: 480,
		height: 320,
		data: null,
		custom: null,
		error: null
	}
});

JASPWidgets.imageView = JASPWidgets.View.extend({

	initialize: function () {

		this.toolbar = new JASPWidgets.Toolbar({ className: "jasp-toolbar" })
		this.toolbar.setParent(this);

		this.resizer = new JASPWidgets.ResizeableView({ model: this.model, className: "jasp-resize" });

		this.listenTo(this.resizer, "ResizeableView:resized", this.onResized)
		this.listenTo(this.resizer, "ResizeableView:resizeStart", this.onResizeStart)
		this.listenTo(this.resizer, "ResizeableView:resizeStop", this.onResizeStop)
		var self = this;
		this.resizer.resizeTargetElement = function () {
			var t = self.$(".jasp-image-holder");
			return self.$(".jasp-image-holder");
		};
		this.resizer.resizeDisabled = function () {
			var custom = self.model.get("custom");
			return custom === null;
		};
	},

	setNoteBox: function (key, noteBox) {
		this.noteBox = noteBox;
		this.noteBoxKey = key;
	},

	hasNotes: function () {
		return this.$el.hasClass('jasp-images-image') === false;
	},

	notesMenuClicked: function (noteType, visibility) {

		this.noteBox.setVisibilityAnimate(visibility);

		return true;
	},

	noteOptions: function () {
		var options = { key: this.noteBoxKey, menuText: 'Add Note', visible: this.noteBox.visible };
		if (this.noteBox.visible)
			options.menuText = 'Remove Note';

		return [options];
	},

	onResized: function (w, h) {
		var options = {};
		var custom = this.model.get("custom");
		if (custom !== null) {
			if (_.has(custom, "width"))
				options[custom.width] = w;
			if (_.has(custom, "height"))
				options[custom.height] = h;
		}

		this.model.trigger("CustomOptions:changed", options);
	},

	onResizeStart: function (w, h) {
		this.$el.addClass("jasp-image-resizable");
	},

	onResizeStop: function (w, h) {
		this.$el.removeClass("jasp-image-resizable");
	},

	events: {
			'mouseenter': '_hoveringStart',
			'mouseleave': '_hoveringEnd',
	},

	_hoveringStart: function (e) {
		this.toolbar.setVisibility(true);
		this.resizer.setVisibility(true);
	},

	_hoveringEnd: function (e) {
		this.toolbar.setVisibility(false);
		this.resizer.setVisibility(false);
	},

	render: function () {
		var html = ''
		var title = this.model.get("title");
		var status = this.model.get("status");
		var error = this.model.get("error");
		var data = this.model.get("data");
		var custom = this.model.get("custom");

		if (title) {
			this.toolbar.title = title;
			this.toolbar.titleTag = "h3";
		}

		this.toolbar.render();

		this.$el.append(this.toolbar.$el);

		var classes = ""
		if (error)
			classes += " error-state"

		html += '<div class="jasp-image-holder ' + classes + '>'

		html += '<div class="jasp-image-image" style="'

		if (data) {
			html += 'background-image : url(\'' + data + '?x=' + Math.random() + '\') ;'

			html += 'background-size : 100% 100% ;'
		}

		html += '">'

		if (error && error.errorMessage) {

			html += '<div  class="error-message-positioner">'
			html += '<div  class="error-message-box ui-state-error">'
			html += '<span class="error-message-symbol ui-icon ui-icon-alert"></span>'
			html += '<div  class="error-message-message">' + error.errorMessage + '</div>'
			html += '</div>'
			html += '</div>'
		}

		html += '</div>'

		html += '<div class="image-status"></div>';

		html += '</div>'

		this.$el.append(html)

		if (this.noteBox !== undefined) {
			this.noteBox.render();
			this.$el.append(this.noteBox.$el);
		}

		var $status = this.$el.find("div.image-status");
		$status.addClass(status);

		var self = this

		this.resizer.render();

		return this;
	},

	exportBegin: function (exportParams, completedCallback) {

		if (exportParams == undefined)
			exportParams = new JASPWidgets.Exporter.params();
		else if (exportParams.error)
			return false;

		var callback = this.exportComplete;
		if (completedCallback !== undefined)
			callback = completedCallback;

		if (exportParams.includeNotes && this.noteBox !== undefined) {
			var exportObject = {
				views: [this, this.noteBox],
				getStyleAttr: function () {
					return "style='display: block;'";
				}
			};
			var newParams = exportParams.clone();
			newParams.includeNotes = false;

			JASPWidgets.Exporter.begin(exportObject, newParams, callback, true);
		}
		else {
			var width = this.model.get("width");
			var height = this.model.get("height");

			var htmlImageFormatData = { resource: this.model.get("data") };
			if (exportParams.htmlOnly() && exportParams.htmlImageFormat === JASPWidgets.ExportProperties.htmlImageFormat.resource)
				callback.call(this, exportParams, new JASPWidgets.Exporter.data(null, this._getHTMLImage(htmlImageFormatData, width, height, exportParams)));
			else {
				var data = this.model.get("data");
				JASPWidgets.Encodings.base64Request(data, function (base64) {
					htmlImageFormatData.embedded = base64;
					if (exportParams.htmlImageFormat === JASPWidgets.ExportProperties.htmlImageFormat.temporary) {
						saveImageBegin(data, base64, function (fullpath) {
							htmlImageFormatData.temporary = fullpath;
							callback.call(this, exportParams, new JASPWidgets.Exporter.data(base64, this._getHTMLImage(htmlImageFormatData, width, height, exportParams)));
						}, this);
					}
					else
						callback.call(this, exportParams, new JASPWidgets.Exporter.data(base64, this._getHTMLImage(htmlImageFormatData, width, height, exportParams)));
				}, this);
			}
		}

		return true;
	},

	_getHTMLImage: function (htmlImageFormatData, width, height, exportParams) {
		var html = "";
		if (exportParams.htmlImageFormat === JASPWidgets.ExportProperties.htmlImageFormat.temporary)
			html = this._addHTMLWrapper('<img src="file:///' + htmlImageFormatData.temporary + '" style="width:' + width + 'px; height:' + height + 'px;" />\n', exportParams);
		else if (exportParams.htmlImageFormat === JASPWidgets.ExportProperties.htmlImageFormat.embedded)
			html = this._addHTMLWrapper('<div style="background-image : url(data:image/png;base64,' + htmlImageFormatData.embedded + '); width:' + width + 'px; height:' + height + 'px;"></div>', exportParams);
		else if (exportParams.htmlImageFormat === JASPWidgets.ExportProperties.htmlImageFormat.resource)
			html = this._addHTMLWrapper('<div style="background-image : url(\'' + htmlImageFormatData.resource + '\'); width:' + width + 'px; height:' + height + 'px;"></div>', exportParams);

		return html;
	},

	_addHTMLWrapper: function(innerHTML, exportParams)
	{
		var error = this.model.get("error");
		var title = this.model.get("title");
		var style = this.getStyleAttr();
		var text = '<div ' + style + '>\n';

		text += JASPWidgets.Exporter.getTitleHtml(this.toolbar, exportParams);
		text += innerHTML;
		text += JASPWidgets.Exporter.exportErrorWindow(this.$el.find('.error-message-positioner'), error);
		text += '</div>\n';

		return text;
	},

	exportComplete: function (exportParams, exportContent) {
		if (!exportParams.error && exportParams.process == JASPWidgets.ExportProperties.process.copy) {
			if (exportParams.htmlOnly())
				pushHTMLToClipboard(exportContent, exportParams);
			else if (exportParams.format == JASPWidgets.ExportProperties.format.raw)
				pushImageToClipboard(exportContent, exportParams);
		}
	},

	copyMenuClicked: function () {
		var exportParams = new JASPWidgets.Exporter.params();
		exportParams.format = JASPWidgets.ExportProperties.format.raw;
		exportParams.process = JASPWidgets.ExportProperties.process.copy;
		exportParams.htmlImageFormat = JASPWidgets.ExportProperties.htmlImageFormat.temporary;
		exportParams.includeNotes = false;

		this.exportBegin(exportParams);

		return true;
	},

	menuName: "Plot",

	onClose: function () {
		this.toolbar.close();
		this.resizer.close();
		if (this.noteBox !== undefined)
			this.noteBox.detach();
	}
});

JASPWidgets.image = JASPWidgets.Resizeable.extend({

	defaults: {
		title: "",
		width: 480,
		height: 320,
		data: null,
		status: "waiting",
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
			this.toolbar.titleTag = "h2";
			//this.$el.append('<h2>' + title + '</h2>');
		}

		this.toolbar.render();
		this.$el.append(this.toolbar.$el);

		var classes = ""
		if (status)
			classes += status

		if (error)
			classes += " error-state"

		html += '<div class="jasp-image-holder ' + classes + '>'

		html += '<div class="jasp-image-image" style="'

		if (data) {
			html += 'background-image : url(\'' + data + '?x=' + Math.random() + '\') ;'

			html += 'background-size : 100% 100% ;'
		}

		html += '"></div>'

		if (error && error.errorMessage) {

			html += '<div  class="error-message-positioner">'
			html += '<div  class="error-message-box ui-state-error">'
			html += '<span class="error-message-symbol ui-icon ui-icon-alert"></span>'
			html += '<div  class="error-message-message">' + error.errorMessage + '</div>'
			html += '</div>'
			html += '</div>'
		}

		html += '<div class="jasp-image-loader"></div>'

		html += '</div>'

		this.$el.append(html)

		var self = this

		this.resizer.render();

		return this;
	},

	exportBegin: function (exportParams) {

		if (exportParams == undefined)
			exportParams = JASPWidgets.Exporter.params();
		else if (exportParams.error)
			return false;

		if (exportParams.format === JASPWidgets.ExportProperties.format.raw && exportParams.imageFormat === JASPWidgets.ExportProperties.imageFormat.resource) {
			exportParams.error = true;
		}
		else {

			var data = this.model.get("data");

			if (exportParams.format === JASPWidgets.ExportProperties.format.html && exportParams.imageFormat === JASPWidgets.ExportProperties.imageFormat.resource) {
				var width = this.model.get("width");
				var height = this.model.get("height");
				var html = this._addHTMLWrapper('<div  style="background-image : url(\'' + data + '\'); width:' + width + 'px; height:' + height + 'px;"></div>');

				this.exportComplete(exportParams, html);
			}
			else {

				JASPWidgets.Encodings.base64Request(data, function (base64) {

					if (exportParams.format === JASPWidgets.ExportProperties.format.html) {

						var width = this.model.get("width");
						var height = this.model.get("height");

						if (exportParams.imageFormat === JASPWidgets.ExportProperties.imageFormat.temporary) {
							saveImageBegin(data, base64, function (fullpath) {

								var html = this._addHTMLWrapper('<img src="file:///' + fullpath + '" style="width:' + width + 'px; height:' + height + 'px;" />\n');
								this.exportComplete(exportParams, html);
							}, this);
						}
						else if (exportParams.imageFormat === JASPWidgets.ExportProperties.imageFormat.embedded) {
							var html = this._addHTMLWrapper('<div style="background-image : url(data:image/png;base64,' + base64 + '); width:' + width + 'px; height:' + height + 'px;"></div>');
							this.exportComplete(exportParams, html);
						}
					}
					else if (exportParams.format === JASPWidgets.ExportProperties.format.raw) {
						this.exportComplete(exportParams, base64);
					}
					else
						exportParams.error = true;

				}, this);
			}
		}

		return true;
	},

	_addHTMLWrapper: function(innerHTML)
	{
		var title = this.model.get("title");
		var style = this.getStyleAttr();
		var text = '<div ' + style + '>\n';

		text += JASPWidgets.Exporter.getTitleHtml(this.toolbar);
		text += innerHTML;
		text += '</div>\n';

		return text;
	},

	exportComplete: function (exportParams, data) {
		if (!exportParams.error && exportParams.process == JASPWidgets.ExportProperties.process.copy) {
			if (exportParams.format == JASPWidgets.ExportProperties.format.html)
				pushHTMLToClipboard(data);
			else if (exportParams.format == JASPWidgets.ExportProperties.format.raw)
				pushImageToClipboard(data);
		}
	},

	copyMenuClicked: function () {
		this.exportBegin({
			format: JASPWidgets.ExportProperties.format.raw,
			process: JASPWidgets.ExportProperties.process.copy,
			imageFormat: JASPWidgets.ExportProperties.imageFormat.temporary
		});
		return true;
	},

	menuName: "Plot",

	onClose: function () {
		this.toolbar.close();
		this.resizer.close();
	}
});

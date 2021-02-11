JASPWidgets.image = JASPWidgets.Resizeable.extend({

	defaults: {
		title:			"",
		width:			480,
		height:			320,
		data:			null,
		custom:			null,
		error:			null,
		name:			"",
		editOptions:	{},
		revision:		0
	}
});

JASPWidgets.imageView = JASPWidgets.objectView.extend({

	exportComplete: function (exportParams, exportContent) {
		if (!exportParams.error && exportParams.process == JASPWidgets.ExportProperties.process.copy) {
			if (exportParams.htmlOnly())
				pushHTMLToClipboard(exportContent, exportParams);
			else if (exportParams.format == JASPWidgets.ExportProperties.format.raw)
				pushImageToClipboard(exportContent, exportParams);
		}
	},

	copyMenuClicked: function () {
		var exportParams				= new JASPWidgets.Exporter.params();
		exportParams.format				= JASPWidgets.ExportProperties.format.raw;
		exportParams.process			= JASPWidgets.ExportProperties.process.copy;
		exportParams.htmlImageFormat	= JASPWidgets.ExportProperties.htmlImageFormat.temporary;
		exportParams.includeNotes		= false;

		this.exportBegin(exportParams);

		return true; 
	},


	hasNotes:					function() {	return this.$el.hasClass('jasp-collection-item')	=== false;	},
	isEditable:					function() {	return true;													},
	hasCollapse:				function() {	return this.$el.hasClass('jasp-collection-item')	=== false;	},
	isConvertible:				function() {	return this.model.get("convertible")				==  true;	},
	saveImageClicked:			function() {	this.model.trigger("SaveImage:clicked",							{ data: this.model.get("data"), width: this.model.get("width"), height: this.model.get("height"), name: this.model.get("name")							});	},
	editImageClicked:			function() {	this.model.trigger("EditImage:clicked",			this.myView,	{ data: this.model.get("data"), width: this.model.get("width"), height: this.model.get("height"), name: this.model.get("name"), title: this.model.get("title"), type: "interactive"		});	},
	showDependenciesClicked:	function() {	this.model.trigger("ShowDependencies:clicked",	this.model.get("name")); },

	menuName:			"Plot",
	myView:				undefined,
	indentChildren:		false,
	notePositionBottom:	true,

	constructChildren: function (constructor, data) {

		var self = this;
		this.toolbar.selectionElement = function () {
			return self.$el.find('.jasp-image-holder');
		};

		var imagePrimitive = new JASPWidgets.imagePrimitive({ model: this.model, className: "jasp-image-holder  jasp-display-primitive" });
		this.resizer = imagePrimitive.resizer;
		this.localViews.push(imagePrimitive);
		this.views.push(imagePrimitive);

		self.myView = imagePrimitive
	},
});

JASPWidgets.imagePrimitive = JASPWidgets.View.extend({

	initialize: function () {

		this.resizer = new JASPWidgets.ResizeableView({ model: this.model, className: "jasp-resize" });

		this.listenTo(this.resizer, "ResizeableView:resized",		this.onResized)
		this.listenTo(this.resizer, "ResizeableView:resizeStart",	this.onResizeStart)
		this.listenTo(this.resizer, "ResizeableView:resizeStop",	this.onResizeStop)
		var self = this;

		this.resizer.resizeTargetElement	= function () { return self.$el; };
		this.resizer.resizeDisabled			= function () { return self.model.get("custom") === null; };
	},

	onResized: function (w, h) {
		if (this.resizer.isResizing() && !this.resizeEventTriggered) {
			this.resizeEventTriggered = true;
			this.model.trigger("EditImage:clicked", this, { data: this.model.get("data"), width: w, height: h, type: "resize", name: this.model.get("name"), title: this.model.get("title") });
			
		}
	},

	onResizeStart: function (w, h) {
		this.resizeEventTriggered = false;
		this.setBackupValues(w, h);
		this.model.trigger("analysis:resizeStarted", this);
		this.$el.addClass("jasp-image-resizable");
	},

	onResizeStop: function (w, h) {
		this.$el.removeClass("jasp-image-resizable");
	},

	setBackupValues: function(w, h) {
		this.model.set({ preResizeWidth: w, preResizeHeight: h });
	},

	restoreSize: function() {
		var width = this.model.get("preResizeWidth");
		var height = this.model.get("preResizeHeight");
		this.model.set({ width: width, height: height });
	},

	events: {
		'mouseenter': '_hoveringStartImage',
		'mouseleave': '_hoveringEndImage',
	},

	_hoveringStartImage: function (e) {
		this.resizer.setVisibility(true);
	},

	_hoveringEndImage: function (e) {
		this.resizer.setVisibility(false);
	},
	
	setRevision: function(revision) {
		this.model.set({revision: revision})
	},
	
	reRender: function () {
		this.$el.find(".jasp-image-image").remove();
		this.render();
	},

	render: function () {
		var html	= ''
		var status	= this.model.get("status");
		var error	= this.model.get("error");
		var data	= this.model.get("data");
		var custom	= this.model.get("custom");

		var width	= this.model.get("width");
		var height	= this.model.get("height");

		if (error)
			this.$el.addClass("error-state");

		if (data) {
			html += '<div class="jasp-image-image"';
			var id = data.replace(/[^A-Za-z0-9]/g, '-');
			var url = window.globSet.tempFolder + data;
			html += ' id="' + id + '" style="';
			html += error ? 'background-image: linear-gradient(rgba(255,255,255,0.67), rgba(255,255,255,0.67)),' : 'background-image:'
			html += 'url(\'' + url + '?rev=' + this.model.get("revision") + '\'); '
			html += 'background-size : 100% 100%">'
		} else if (height > 100 && width > 100) {
			html += '<div class="jasp-image-image no-data' + (error ? ' error' : '') + '">'
		}

		if (error && error.errorMessage) {

			html += '<div  class="error-message-positioner">'
			html += '<div  class="error-message-box ui-state-error">'
			html += '<span class="error-message-symbol ui-icon ui-icon-alert"></span>'
			html += '<div  class="error-message-message">' + error.errorMessage + '</div>'
			html += '</div>'
			html += '</div>'
		}

		html += '<div class="image-status"></div>';
		html += '</div>'

		html += '</div>'

		this.$el.append(html)

		var $status = this.$el.find("div.image-status");
		$status.addClass(status);

		var $t = this.$el;
		$t.css({
			width: width,
			height: height
		});
		
		if (data)
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

		var width = this.model.get("width");
		var height = this.model.get("height");

		var htmlImageFormatData = { resource: this.model.get("data") };
		if (exportParams.htmlOnly() && exportParams.htmlImageFormat === JASPWidgets.ExportProperties.htmlImageFormat.resource)
			callback.call(this, exportParams, new JASPWidgets.Exporter.data(null, this._getHTMLImage(htmlImageFormatData, width, height, exportParams)));
		else {
			var data = this.model.get("data");
			convertToBase64Begin(data, function (base64) {
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

		return true;
	},

	_getHTMLImage: function (htmlImageFormatData, width, height, exportParams) {
		var html = "";
		if (exportParams.htmlImageFormat === JASPWidgets.ExportProperties.htmlImageFormat.temporary)
			html = '<img src="file:///' + htmlImageFormatData.temporary + '" style="width:' + width + 'px; height:' + height + 'px;" />';
		else if (exportParams.htmlImageFormat === JASPWidgets.ExportProperties.htmlImageFormat.embedded)
			html = '<div style="background-image : url(data:image/png;base64,' + htmlImageFormatData.embedded + '); background-size:' + width + 'px ' + height + 'px; width:' + width + 'px; height:' + height + 'px;"></div>';
		else if (exportParams.htmlImageFormat === JASPWidgets.ExportProperties.htmlImageFormat.resource)
			html = '<img src="' + htmlImageFormatData.resource + '" style="width:' + width + 'px; height:' + height + 'px;" />';

		var error = this.model.get("error");
		html += JASPWidgets.Exporter.exportErrorWindow(this.$el.find('.error-message-positioner'), error);

		return html;
	}
});

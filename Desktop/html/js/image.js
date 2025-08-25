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
		revision:		0,
		plotlyData:     "",
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


	hasNotes:					function() {	return this.$el.hasClass('jasp-collection-item') === false || this.$el.hasClass('jasp-image');	},
	hasCopy:					function() {	return this.model.get("error") === null;						},
	isEditable:					function() {	return this.model.get("error") === null;						},
	isConvertible:				function() {	return this.model.get("error") === null && this.model.get("convertible") ===  true;	},
	hasCollapse:				function() {	return this.$el.hasClass('jasp-collection-item')	=== false;	},
	hasInteractive:				function() {	return this.model.get("interactiveJsonData") !== null && this.model.get("interactiveJsonData") !== undefined;	},
	saveImageClicked:			function() {	this.model.trigger("SaveImage:clicked",							{ data: this.model.get("data"), width: this.model.get("width"), height: this.model.get("height"), name: this.model.get("name")							});	},
	editImageClicked:			function() {	this.model.trigger("EditImage:clicked",			this.myView,	{ data: this.model.get("data"), width: this.model.get("width"), height: this.model.get("height"), name: this.model.get("name"), title: this.model.get("title"), type: "interactive"		});	},
	interactiveImageClicked:	function() {
		// Toggle between interactive and static modes
		var isCurrentlyInteractive = this.model.get("interactive");
		this.model.set("interactive", !isCurrentlyInteractive);

		// Clear the current content and re-render
		// this.myView.$el.empty();
		this.myView.reRender();

		return true;
	},
	showDependenciesClicked:	function() {	this.model.trigger("ShowDependencies:clicked",	this.model.get("name")); },

	interactiveOptions: function() {
		var isInteractive = this.model.get("interactive");
		var text = isInteractive ? 'Static plot' : 'Interactive plot';
		return { menuText: text };
	},


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
		this.listenTo(this.resizer, "ResizeableView:viewResized",	this.onViewResized)
		this.listenTo(this.resizer, "ResizeableView:resizeStart",	this.onResizeStart)
		this.listenTo(this.resizer, "ResizeableView:resizeStop",	this.onResizeStop)
		var self = this;

		this.resizer.resizeTargetElement	= function () { return self.$el; };
		this.resizer.resizeDisabled			= function () { return self.model.get("custom") === null; };
	},

	onResized: function (w, h) {
		// This is called at the end of resize - handle final logic here
		if (this.resizer.isResizing() && !this.resizeEventTriggered) {
			this.resizeEventTriggered = true;
			// also done when the image is interactive so that when switching the correct png is shown
			this.model.trigger("EditImage:clicked", this, { data: this.model.get("data"), width: w, height: h, type: "resize", name: this.model.get("name"), title: this.model.get("title") });
		}
	},

	onViewResized: function (w, h) {
		// This is called during active resize - handle real-time updates here
		console.log("Real-time resize to:", w, h);

		// Update Plotly chart size in real-time during resize if it's an interactive plot
		if (this.model.get("interactive") && this.plotlyId) {
			var targetEl = document.getElementById(this.plotlyId);
			if (targetEl) {
				console.log("Resizing Plotly chart to:", w, h);

				// Update the div size directly
				targetEl.style.width = w + 'px';
				targetEl.style.height = h + 'px';

				// Check if the element has a valid Plotly chart before relayout
				if (targetEl.data && targetEl.layout && targetEl._fullLayout && targetEl._plotlyInitialized) {
					// Then tell Plotly to relayout
					Plotly.relayout(targetEl, {
						width: w,
						height: h
					}).catch(function(err) {
						console.warn("Plotly relayout failed:", err);
					});
				} else {
					console.warn("Plotly chart not ready for resize:", this.plotlyId);
				}
			} else {
				// Only warn if we're actually in interactive mode and expect the element to exist
				if (this.model.get("interactive")) {
					console.warn("Plotly element not found during resize (may have been removed):", this.plotlyId);
				}
			}
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

		// Reinitialize the resizer to ensure it works after mode changes
		this.resizer = new JASPWidgets.ResizeableView({ model: this.model, className: "jasp-resize" });
		var self = this;
		this.resizer.resizeTargetElement	= function () { return self.$el; };
		this.resizer.resizeDisabled			= function () { return self.model.get("custom") === null; };

		// Re-attach event listeners
		this.listenTo(this.resizer, "ResizeableView:resized",		this.onResized);
		this.listenTo(this.resizer, "ResizeableView:viewResized",	this.onViewResized);
		this.listenTo(this.resizer, "ResizeableView:resizeStart",	this.onResizeStart);
		this.listenTo(this.resizer, "ResizeableView:resizeStop",	this.onResizeStop);

		this.render();
	},

	render: function () {//interactive = false) {

		if (this.model.get("interactive")) {

			console.log("image.js: this is where the post step to run the json happens!");
			this.preRenderPlotly();

			// Only call jQuery if there's no error
			var error = this.model.get("interactiveConvertError");
			if (!error) {
				this.plotlyRetryCount = 0; // Reset retry counter
				jQuery(document).ready(() => this.renderPlotlyIfDivExists());
			}

		} else {
			console.log("image.js: jaspHtml but not plotly!")
			this.renderDefault();
		}
	},

	_buildErrorHTML: function(errorMessage) {
		var html = '';
		html += '<div class="error-message-positioner">';
		html += '<div class="error-message-box ui-state-error">';
		html += '<span class="error-message-symbol ui-icon ui-icon-alert"></span>';
		html += '<div class="error-message-message">' + errorMessage + '</div>';
		html += '</div>';
		html += '</div>';
		return html;
	},

	preRenderPlotly: function () {
		// Implementation for rendering Plotly charts
		// Generate a random (hopefully unique) ID only if we don't have one yet
		if (!this.plotlyId) {
			this.plotlyId = 'htmlwidget-' + Math.random().toString(16).substring(2, 18);
			console.log("Generated new Plotly ID:", this.plotlyId);
		} else {
			console.log("Reusing existing Plotly ID:", this.plotlyId);
		}

		// Get width and height from model (same as renderDefault)
		var width = this.model.get("width");
		var height = this.model.get("height");

		// Get error from model (same as renderDefault)
		var error = this.model.get("interactiveConvertError");

		var html = '';

		// Add error state class if there's an error (same as renderDefault)
		if (error)
			this.$el.addClass("error-state");

		// Create the div element that will contain the Plotly plot or error

		// Use shared error HTML builder
		if (error && error !== "") {
			if (height > 100 && width > 100) {
				html += '<div class="jasp-image-image no-data' + (error ? ' error' : '') + '">';
			}
			html += this._buildErrorHTML(error);
		} else {
			// Create the same structure as renderDefault when there's data
			html += '<div class="jasp-image-image"';
			html += ' id="plotly-container-' + this.plotlyId + '">';

			// Add the plotly div with proper dimensions
			html += '<div id="' + this.plotlyId + '" class="plotly html-widget html-widget-output" style="width:' + width + 'px; height:' + height + 'px;"></div>';
		}

		html += '<div class="image-status"></div>';
		html += '</div>';

		// Append the HTML to the element
		this.$el.append(html);

		// Apply width and height CSS (same as renderDefault)
		var $t = this.$el;
		$t.css({
			width: width,
			height: height
		});

		if (!error || error === "") {
			this.resizer.render();
		}
	},

	renderPlotlyIfDivExists: function () {

		if (!this.plotlyId) {
			console.warn("No plotly ID found.");
			return;
		}

		const targetEl = document.getElementById(this.plotlyId);
		console.log("Plotly render attempt - ID:", this.plotlyId, "Element found:", !!targetEl, "Visible:", targetEl ? $(targetEl).is(':visible') : false, "Retry count:", this.plotlyRetryCount || 0);

		if (targetEl && $(targetEl).is(':visible')) {
			const payload = this.model.get("interactiveJsonData");
			console.log("Rendering Plotly with payload:", payload);

			// Clear any existing plot first
			Plotly.purge(targetEl);
			targetEl._plotlyInitialized = false;

			// Then create new plot
			Plotly.newPlot(targetEl, payload.data, payload.layout)
				.then(() => {
					console.log("Plotly chart rendered successfully");
					// Mark the element as having a valid Plotly chart
					targetEl._plotlyInitialized = true;
				})
				.catch((err) => {
					console.error("Plotly rendering failed:", err);
					targetEl._plotlyInitialized = false;
				});
		} else {
			// Limit retries to prevent infinite loops
			this.plotlyRetryCount = (this.plotlyRetryCount || 0) + 1;
			if (this.plotlyRetryCount < 100) { // Max 5 seconds of retries
				console.log("Plotly element not ready, retrying... (attempt " + this.plotlyRetryCount + ")");
				setTimeout(this.renderPlotlyIfDivExists.bind(this), 50);
			} else {
				console.error("Failed to render Plotly after 100 attempts - giving up");
			}
		}
	},


	renderDefault: function () {
		var html	= ''
		var status	= this.model.get("status");
		var error	= this.model.get("error");
		var data	= this.model.get("data");
		var custom	= this.model.get("custom");

		var width	= this.model.get("width");
		var height	= this.model.get("height");

		// if rendering as plotly we need to do
		// this.$el.find(".jasp-image-image").remove();
		// then call render like in htmlNode.js
		// first check if this part works though!
		var interactive = this.model.get("interactive");
		console.log("Rendering name: " + this.model.get("name") + " | Interactive: " + interactive);

		if (error)
			this.$el.addClass("error-state");

		if (data) {
			html += '<div class="jasp-image-image"';
			var id = data.replace(/[^A-Za-z0-9]/g, '-');
			var url = insideJASP ? "plot://" + data : data;
			html += ' id="' + id + '" style="';
			html += error ? 'background-image: linear-gradient(rgba(255,255,255,0.67), rgba(255,255,255,0.67)),' : 'background-image:'
			html += 'url(\'' + url + '?rev=' + this.model.get("revision") + '\'); '
			html += 'background-size : 100% 100%">'
		} else if (height > 100 && width > 100) {
			html += '<div class="jasp-image-image no-data' + (error ? ' error' : '') + '">'
		}

		if (error && error.errorMessage) {
			html += this._buildErrorHTML(error.errorMessage);
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
			html = '<img src="file://' + htmlImageFormatData.temporary + '" style="width:' + width + 'px; height:' + height + 'px;" />';
		else if (exportParams.htmlImageFormat === JASPWidgets.ExportProperties.htmlImageFormat.embedded)
			html = '<div style="background-image : url(data:image/png;base64,' + htmlImageFormatData.embedded + '); background-size:' + width + 'px ' + height + 'px; width:' + width + 'px; height:' + height + 'px;"></div>';
		else if (exportParams.htmlImageFormat === JASPWidgets.ExportProperties.htmlImageFormat.resource)
			html = '<img src="' + htmlImageFormatData.resource + '" style="width:' + width + 'px; height:' + height + 'px;" />';

		var error = this.model.get("error");
		html += JASPWidgets.Exporter.exportErrorWindow(this.$el.find('.error-message-positioner'), error);

		return html;
	}
});

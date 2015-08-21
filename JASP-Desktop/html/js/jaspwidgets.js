var JASPWidgets = {};

JASPWidgets.Encodings = {
	getBase64: function (arrayBuffer) {
		var base64 = ''
		var encodings = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/'

		var bytes = new Uint8Array(arrayBuffer)
		var byteLength = bytes.byteLength
		var byteRemainder = byteLength % 3
		var mainLength = byteLength - byteRemainder

		var a, b, c, d
		var chunk

		// Main loop deals with bytes in chunks of 3
		for (var i = 0; i < mainLength; i = i + 3) {
			// Combine the three bytes into a single integer
			chunk = (bytes[i] << 16) | (bytes[i + 1] << 8) | bytes[i + 2]

			// Use bitmasks to extract 6-bit segments from the triplet
			a = (chunk & 16515072) >> 18 // 16515072 = (2^6 - 1) << 18
			b = (chunk & 258048) >> 12 // 258048   = (2^6 - 1) << 12
			c = (chunk & 4032) >> 6 // 4032     = (2^6 - 1) << 6
			d = chunk & 63               // 63       = 2^6 - 1

			// Convert the raw binary segments to the appropriate ASCII encoding
			base64 += encodings[a] + encodings[b] + encodings[c] + encodings[d]
		}

		// Deal with the remaining bytes and padding
		if (byteRemainder == 1) {
			chunk = bytes[mainLength]

			a = (chunk & 252) >> 2 // 252 = (2^6 - 1) << 2

			// Set the 4 least significant bits to zero
			b = (chunk & 3) << 4 // 3   = 2^2 - 1

			base64 += encodings[a] + encodings[b] + '=='
		} else if (byteRemainder == 2) {
			chunk = (bytes[mainLength] << 8) | bytes[mainLength + 1]

			a = (chunk & 64512) >> 10 // 64512 = (2^6 - 1) << 10
			b = (chunk & 1008) >> 4 // 1008  = (2^6 - 1) << 4

			// Set the 2 least significant bits to zero
			c = (chunk & 15) << 2 // 15    = 2^4 - 1

			base64 += encodings[a] + encodings[b] + encodings[c] + '='
		}

		return base64
	},

	base64Request: function (path, callback, context) {
		var xhr = new XMLHttpRequest();
		xhr.open('GET', path + '?x=' + Math.random(), true); //eg '/my/image/name.png'
		xhr.responseType = 'arraybuffer';

		var self = context || this;
		xhr.onload = function (e) {
			callback.call(self, JASPWidgets.Encodings.getBase64(this.response));
		};
		xhr.onerror = function (e) {
			alert(xhr.statusText);
		};

		xhr.send();
	},

	byteRequest: function (path, callback, context) {
		var xhr = new XMLHttpRequest();
		xhr.open('GET', path + '?x=' + Math.random(), true); //eg '/my/image/name.png'
		xhr.responseType = 'arraybuffer';

		var self = context || this;
		xhr.onload = function (e) {
			callback.call(self, this.response);
		};

		xhr.send();
	}
}

JASPWidgets.ExportProperties = {

	format: {
		raw: 0,
		html: 1, //bit 1
		formatted: 2, //bit 2
		formattedHTML: 3
	},

	process: {
		copy: 0,
		save: 1
	},

	htmlImageFormat: {
		temporary: 0,
		resource: 1,
		embedded: 2
	}
}

JASPWidgets.Exporter = {

	params: function() {

			this.format = JASPWidgets.ExportProperties.format.raw,
			this.process = JASPWidgets.ExportProperties.process.copy,
			this.htmlImageFormat = JASPWidgets.ExportProperties.htmlImageFormat.temporary,

			this.isFormatted = function () {
				return (this.format & JASPWidgets.ExportProperties.format.formatted) === JASPWidgets.ExportProperties.format.formatted
			},

			this.htmlOnly = function () {
				return (this.format & JASPWidgets.ExportProperties.format.html) === JASPWidgets.ExportProperties.format.html
			}
	},

	data: function (raw, html) {

		if (raw == null)
			raw = "";

		this.raw = raw;
		this.html = html;
	},

	begin: function (exportObj, exportParams, useNBSP, innerStyle) {

		if (innerStyle === undefined)
			innerStyle = "";

		if (useNBSP == undefined)
			useNBSP = false;

		if (exportParams == undefined)
			exportParams = new JASPWidgets.Exporter.params();
		else if (exportParams.error)
			return false;

		
		if (exportObj.views) {
			var viewList = exportObj.views;

			if (viewList.length === 0) {
				exportObj.exportComplete(exportParams, new JASPWidgets.Exporter.data(null, ""));
			}
			else {
				exportObj.buffer = [];
				exportObj.exportCounter = viewList.length;

				for (var i = 0; i < viewList.length; i++) {
					this._exportView(exportParams, viewList[i], i, exportObj, useNBSP, innerStyle);
				}
			}
		}
		else if (exportObj.exportBegin)
			exportObj.exportBegin(exportParams);
		else
			return false;
		

		return true;
	},

	_exportView: function (exportParams, view, i, parent, useNBSP, innerStyle) {
		var self = parent;
		var index = i;
		var originalExportComplete = view.exportComplete;
		view.exportComplete = function (exParams, exContent) {
			this.exportComplete = originalExportComplete;
			self.buffer[index] = exContent;
			self.exportCounter -= 1;
			if (self.exportCounter === 0) {
				var completeText = "";
				if (!exportParams.error) {
					completeText = "<div " + self.getStyleAttr() + "'>\n";
					completeText += "<div style='display:inline-block; " + innerStyle + "'>\n";
					if (self.toolbar !== undefined) {
						completeText += JASPWidgets.Exporter.getTitleHtml(self.toolbar, exportParams)
					}
					for (var j = 0; j < self.buffer.length; j++) {
						if (self.buffer[j]) {
							completeText += self.buffer[j].html;
							if (useNBSP && j < self.buffer.length && (JASPWidgets.Exporter.isInlineStyle(self.views[j].$el) == false))
								completeText += "&nbsp;";
						}
					}
					completeText += "</div>";
					completeText += "</div>";		
				}
				self.exportComplete(exportParams, new JASPWidgets.Exporter.data(null, completeText));
				self.buffer = [];
			}
		};
		view.exportBegin(exportParams);
	},

	getStyles: function (element, styleItems) {

		var css = element.css(styleItems);
		if (css === undefined)
			return "";

		var style = "";
		for (var i = 0; i < styleItems.length; i++)
		{
			var styleItem = styleItems[i];
			if (css[styleItem])
				style += styleItem + ": " + css[styleItem] + "; "
		}

		if (style)
			style = "style='" + style + "'";

		return style;
	},

	getHeaderStyles: function (element, exportParams) {
		if (exportParams.isFormatted())
			return JASPWidgets.Exporter.getStyles(element, ["padding", "text-align", "margin", "display", "float", "vertical-align", "font-size", "font", "font-weight"]);
		else
			return "";//JASPWidgets.Exporter.getStyles(element, ["display", "float"]);
	},

	getTableStyles: function (element, exportParams) {
		if (exportParams.isFormatted())
			return JASPWidgets.Exporter.getStyles(element, ["border-collapse", "border-top-width", "border-bottom-width", "border-left-width", "border-right-width", "border-color", "border-style", "padding", "text-align", "margin-bottom", "margin-top", "display", "float"]);
		else
			return JASPWidgets.Exporter.getStyles(element, ["border-collapse", "border-top-width", "border-bottom-width", "border-left-width", "border-right-width", "border-color", "border-style", "display", "float"]);
	},

	getTableContentStyles: function (element, exportParams) {
		if (exportParams.isFormatted())
			return JASPWidgets.Exporter.getStyles(element, ["border-collapse", "border-top-width", "border-bottom-width", "border-left-width", "border-right-width", "border-color", "border-style", "padding", "text-align", "margin", "display", "float", "font-size", "font-weight", "font"]);
		else
			return JASPWidgets.Exporter.getStyles(element, ["border-collapse", "border-top-width", "border-bottom-width", "border-left-width", "border-right-width", "border-color", "border-style", "display", "float", "text-align"]);
	},

	getErrorStyles: function (element, component) {
		if (component === "error-message-positioner")
			return JASPWidgets.Exporter.getStyles(element, ["padding", "margin", "display", "float", "height", "overflow", "position", "top", "z-index"]);
		else if (component === "error-message-box")
			return JASPWidgets.Exporter.getStyles(element, ["margin", "border", "background-color", "color", "padding", "display", "float", "border-radius", "min-width", "max-width", "white-space"]);
		else if (component === "error-message-symbol ")
			return JASPWidgets.Exporter.getStyles(element, ["margin", "border", "background-color", "color", "padding", "display", "float"]);
		else
			return JASPWidgets.Exporter.getStyles(element, ["margin", "border", "background-color", "color", "padding", "text-align", "display", "float", "vertical-align", "font-size", "font", "font-weight"]);
	},

	exportErrorWindow: function (element, error) {
		var text = "";

		if (error && error.errorMessage) {

			text += '<div ' + JASPWidgets.Exporter.getErrorStyles(element, 'error-message-positioner') + '>'
			text += '<div ' + JASPWidgets.Exporter.getErrorStyles(element.find('.error-message-box'), 'error-message-box') + '>'
			text += '<span ' + JASPWidgets.Exporter.getErrorStyles(element.find('.error-message-symbol'), 'error-message-symbol') + '></span>'
			text += '<div ' + JASPWidgets.Exporter.getErrorStyles(element.find('.error-message-message'), 'error-message-message') + '>' + error.errorMessage + '</div>'
			text += '</div>'
			text += '</div>'
		}

		return text;
	},

	isInlineStyle: function (element) {
		var css = element.css("display");

		return css === "inline" || css === "inline-block";
	},

	getTitleHtml: function (toolbar, exportParams) {
		var html = toolbar.title === undefined ? "" : toolbar.title;
		if (toolbar.titleTag !== undefined) {
			var headerStyles = " " + JASPWidgets.Exporter.getHeaderStyles(toolbar.$title(), exportParams);
			html = '<' + toolbar.titleTag + headerStyles + '>' + toolbar.title + '</' + toolbar.titleTag + '>';
		}

		return '<div style="display: inline-block;">' + html + '</div>';
	},
};

JASPWidgets.View = Backbone.View.extend({

	getStyleAttr: function (styleItems) {

		return JASPWidgets.Exporter.getStyles(this.$el, ["padding", "text-align", "margin-bottom", "margin-top", "margin-left", "margin-right", "display", "float", "position"]);

	},

	_isClosed: false,
	/** Removes DOM tree elements and any listeners from or too the view object */
	close: function () {
		if (!this._isClosed) {
			this.remove();
			this.off();
			this._isClosed = true;
			if (this.onClose)
				this.onClose();
		}
	},
})

JASPWidgets.Note = Backbone.Model.extend({
	defaults: {
		text: '',
		format: 'markdown'
	},

	toHtml: function () {
		if (this.get('format') === 'markdown') {
			this.set('format', 'html');
			var text = this.get('text');
			if (text !== '')
				this.set('text', Mrkdwn.toHtml(text));
		}
	},


});

JASPWidgets.NoteBox = JASPWidgets.View.extend({

	//#7C95CB
	//#F2F7FD

	ghostText: 'Type notes here...',
	editting: false,

	initialize: function () {
		this.visible = this.model.get('visible') !== null ? this.model.get('visible') : (this.model.get('text') !== '');
		this.internalChange = false;

		if (this.model.get('format') !== 'html')
			this.model.toHtml();

		this.listenTo(this.model, 'change:text', this.textChanged)
	},

	clear: function () {
		if (this.isTextboxEmpty() === false)
			this.$textbox.html('');

		this.model.set('format', 'html');
		this.model.set('text', '');
	},

	isTextboxEmpty: function() {
		return this.$textbox.text().length == 0 || this.$textbox.text() === this.ghostText;
	},

	textChanged: function () {
		if (this._textedChanging === true)
			return;

		this._textedChanging = true;
		if (this.model.get('format') !== 'html')
			this.model.toHtml();
		this.updateView();
		if (this._inited)
			this.trigger("NoteBox:textChanged");
		this._textedChanging = false;
	},

	updateView: function () {
		if (!this.internalChange && this.model.get('format') === 'html') {
			if (this.$textbox !== undefined) {
				var editting = this.model.get('editting');
				var html = this.model.get("text");

				if (!this.editting && this.isTextboxEmpty() && html.length === 0) {
					this.$textbox.html('<p>' + this.ghostText + '</p>');
					this.$textbox.addClass('jasp-ghost-text');
				}
				else {
					var focus = false;
					if (this.$textbox.text().length == 0 || this.$textbox.text() === this.ghostText)
						focus = true;

					if (html !== this.$textbox.html()) {
						this.$textbox.html(html);
						this.model.set('text', this.$textbox.html());
					}

					this.$textbox.removeClass('jasp-ghost-text');

					if (focus) {
						this.$textbox.focus();
					}

				}
			}
		}
	},

	render: function () {
		if (this._inited) {
			this.$textbox.off();
			delete this.$textbox;
		}

		this.$el.empty();

		this.setVisibility(this.visible)

		this.$el.append('<div class="jasp-editable" data-button-class="jasp-comment" contenteditable="true"></div>');

		this.$textbox = this.$el.find('.jasp-editable');
		this.$textbox.attr('contenteditable', true);

		this.updateView();
		this._checkTags();

		var self = this;
		//focusin focusout
		this.$textbox.on("input", function (event) {
			self._checkTags();
			if (this.innerHTML != self.model.get("text")) {
				var html = '';
				var ghostText = self.ghostText;
				if (self.$textbox.text() && self.$textbox.text() !== ghostText)
					html = this.innerHTML;
				self.onNoteChanged(html);
			}
		});

		this.$textbox.on("click", null, this, this._click);
		this.$textbox.on("focusout", null, this, this._looseFocus);
		this.$textbox.on("mouseup", null, this, this._mouseup);
		this.$textbox.on("keydown", null, this, this._keydown);

		this._inited = true;

		return this;
	},

	onNoteChanged: function (html) {
		this.internalChange = true;
		this.model.set("text", html);
		this.internalChange = false;
	},

	setVisibility: function (value) {
		this.visible = value;

		if (value) 
			this.$el.removeClass('jasp-hide');
		else 
			this.$el.addClass('jasp-hide');
	},

	knownTags: ['p', 'br', 'ol', 'ul', 'li', 'b', 'i', 's', 'u', 'sup', 'sub', 'code', 'strong', 'em', 'blockquote', 'hr', 'h1', 'h2', 'h3', 'h4', 'h5', 'h6'],

	_checkTags: function () {

		var r = this.$textbox.find('*')
		r.contents().filter(function () { return this.nodeType != 1 && this.nodeType != 3; }).remove(); //removes all non text and non elements
		r.each(function () { //removes all attributes
			var attributes = $.map(this.attributes, function (item) {
				return item.name;
			});

			var img = $(this);
			$.each(attributes, function (i, item) {
				img.removeAttr(item);
			});
		});

		/*this.$textbox.find('ol, ul').contents().filter(function () { return this.nodeType !== 1 }).remove(); //removes all non list item nodes from lists*/
		this.$textbox.find(':not(p, br, ol, ul, li, b, i, s, u, sup, sub, code, strong, em, blockquote, hr, h1, h2, h3, h4, h5, h6, div, pre:has(code))').contents().unwrap();		//flattens the contents of unknown tags eg span, font etc
		this.$textbox.find(':not(p, br, ol, ul, li, b, i, s, u, sup, sub, code, strong, em, blockquote, hr, h1, h2, h3, h4, h5, h6, div, pre:has(code))').remove();		//removes all unknown tags that had no content to flatten eg <o-l></o-l> from office

		var t = this.$textbox.find('div:not(code div)').contents().unwrap().wrap('<p/>'); //changes all div tags to p tags
		if (t.length > 0) {
			var selection = window.getSelection();
			selection.collapse(t[t.length - 1], 0);
		}

		var b = this.$textbox.contents().filter(function () { return this.nodeType == 3; }).wrap('<p/>'); //wraps all top level free text in a p tag
		if (b.length > 0) {
			var selection = window.getSelection();
			var node = b[b.length - 1];
			selection.collapse(node, node.nodeValue.length);
		}

		this.$textbox.find('p').filter(function () {
			return $(this).text() === '' && $(this).height() === 0;
		}).remove(); //remove non displaying paragraphs

		this.$textbox.find('p p').contents().unwrap(); //unwraps any embedded p tags

		var v = this.$textbox.find('p:has(ol),p:has(ul),p:has(blockquote)').contents().unwrap(); // unwrap any p tags around lists
		if (v.length > 0) {
			var selection = window.getSelection();
			selection.collapse(v[v.length - 1], 0);
		}

		var g = this.$textbox.children().length; // if the textbox is empty put in a <p><br></p>
		if (g === 0) {
			var selection = window.getSelection();
			var node = $(document.createElement('p'));
			var lineBreak = $(document.createElement('br'));
			node.prepend(lineBreak);
			this.$textbox.prepend(node);
			selection.collapse(lineBreak[0], 0);
		}
	},

	_keydown: function (e) {
		var self = e.data;
		if (e.which == 9) {
			e.preventDefault();
		}
	},

	_click: function (e) {

		var self = e.data;
		
		self._setEdittable(e.pageX, e.pageY);

		return true;
	},

	_mouseup: function(e) {
		var self = e.data;

		self._setEdittable(e.pageX, e.pageY);

		return true;
	},

	_setEdittable: function (pageX, pageY) {

		etch.config.selector = '.jasp-editable'

		_.extend(etch.config.buttonClasses, {
			'default': ['bold', 'italic', 'underline'],
			'jasp-comment': ['bold', 'italic', 'superscript', 'subscript', 'unordered-list', 'ordered-list', 'clear-formatting']
		});

		this.editting = true;
		this.updateView();
		this.$editor = etch.startEditting(this.$textbox, pageX, pageY);
		this.$el.addClass('jasp-text-editting');

		this._checkTags();
	},

	_looseFocus: function (e) {
		var self = e.data;
		var relatedtarget = e.relatedTarget;
		if (relatedtarget === null || $(relatedtarget).not('.etch-editor-panel, .etch-editor-panel *, .etch-image-tools, .etch-image-tools *').size()) {
			self.editting = false;
			self.updateView();
			if (self.$editor !== undefined) {
				etch.closeEditor(self.$editor, self.$textbox);
				self.$el.removeClass('jasp-text-editting');
				delete self.$editor;
			}
		}

		return true;
	},

	exportBegin: function (exportParams) {
		if (exportParams == undefined)
			exportParams = new JASPWidgets.Exporter.params();
		else if (exportParams.error)
			return false;

		var html = '';
		if (this.isTextboxEmpty() === false)
			html = this.$textbox.html();

		this.exportComplete(exportParams, new JASPWidgets.Exporter.data(null, html));
	},

	exportComplete: function (exportParams, exportContent) {
		if (!exportParams.error)
			pushHTMLToClipboard(exportContent, exportParams);
	},

	onClosed: function() {
		if (this.$textbox !== undefined)
			this.$textbox.off();
	}
})


JASPWidgets.Toolbar = JASPWidgets.View.extend({
	initialize: function () {
		$(document).mousedown(this, this._mouseDownGeneral);
		this.fixed = false;
		this.visible = false;
		this.selected = false;
	},

	$title: function() {
		return this.$el.find(this.titleTag);
	},

	render: function () {
		this.$el.empty();

		if (this.titleTag !== undefined)
			this.$el.addClass(this.titleTag + "-toolbar");

		if (this.title !== undefined) {
			if (this.titleTag !== undefined)
				this.$el.append('<' + this.titleTag + ' class="in-toolbar toolbar-clickable">' + this.title + '</' + this.titleTag + '>');
			else
				this.$el.append(this.title);
		}

		if (this.hasMenu)
		{
			this.$el.append('<div class="toolbar-button toolbar-clickable jasp-menu jasp-hide"/>')

			//var $menuBtn = this.$el.find(".jasp-menu")

			var $self = this.$el.find(">:first-child");
			$self.tooltip({
				content: "Damo is awesome",
				items: "*",
				disabled: true,
				show: {
					duration: 250,
					effect: "fade",
					delay: 250
				},
				close: function () {
					window.setTimeout(function () {
						$self.tooltip("option", "disabled", true)
					}, 2000)
				},
				position: {
					my: "left top",
					at: "left bottom+5",
					collision: "flipfit"
				}
			})
		}
		return this;
	},

	events: {
		'mousedown .toolbar-clickable': '_mouseDown'
	},

	_mouseDownGeneral: function (e) {
		if (!e || !e.data) return;
		var self = e.data;
		self.decreaseFixedness();
	},

	setVisibility: function (value) {
		this.visible = value;

		if (this.fixed)
			return;

		if (value) {
			this.$(".toolbar-button").removeClass('jasp-hide');
		}
		else {
			this.$(".toolbar-button").addClass('jasp-hide');
		}
	},

	_mouseDown: function (e) {
		this.setFixedness(2);

		var $titleLabel = this.$el.find('>:first-child');

		var offset = $titleLabel.offset();

		var posY = offset.top + $titleLabel.height() - $(window).scrollTop() + 3;
		var posX = offset.left - $(window).scrollLeft();

		this.options.rX = posX;
		this.options.rY = posY;

		this.options.x = e.screenX;
		this.options.y = e.screenY;

		if (this.options.hasNotes)
			this.options['noteOptions'] = this.parent.noteOptions();

		this.parent.trigger('toolbar:showMenu', this.parent, this.options);

		return true;
	},

	decreaseFixedness: function() {
		this.setFixedness(this.fixed - 1);
	},

	setFixedness: function (value) {
		this.fixed = Math.max(0, value);
		this.setVisibility(this.visible);
		this.setSelected(this.fixed !== 0);
	},

	setParent: function (parent) {
		this.parent = parent;
		this.options = {

			hasCopy: (parent.hasCopy === undefined || parent.hasCopy()) && parent.copyMenuClicked !== undefined,
			hasCite: (parent.hasCitation === undefined || parent.hasCitation()) && parent.citeMenuClicked !== undefined,
			hasNotes: (parent.hasNotes === undefined || parent.hasNotes()) && parent.notesMenuClicked !== undefined,
			hasRemove: (parent.hasRemove === undefined || parent.hasRemove()) && parent.removeMenuClicked !== undefined,

			objectName: parent.menuName,
		};

		this.hasMenu = this.options.hasCopy || this.options.hasCite || this.options.hasNotes || this.options.hasRemove;
	},

	selectionElement: function() {
		return this.parent.$el;
	},

	setSelected: function (value) {
		this.selected = value;

		var $selectionElement = this.selectionElement();
		if (value) {
			$selectionElement.addClass("jasp-menu-selected")
		}
		else {
			$selectionElement.removeClass("jasp-menu-selected")
		}
	},

	displayMessage: function (msg) {
		if (msg !== undefined) {
			var $self = this.$el.find(">:first-child");
			$self.tooltip("option", "content", msg)
			$self.tooltip("option", "disabled", false)
			$self.tooltip("open")

			window.setTimeout(function () {
				$self.tooltip("close")
			}, 2000)
		}
	},

	completeEvent: function () {
		this.setFixedness(0);
	}
})

JASPWidgets.CollectionView = JASPWidgets.View.extend({

	/** Initialises the base class property and creates the views from the model collection. */
	initialize: function () {
		this._collectionViewBase = JASPWidgets.View.prototype;
		this._collectionViewBase.initialize.call(this);

		this.views = [];

		this.collection.each(function (item) {
			var itemView = this.createItemView(item);
			this.listenTo(itemView, "all", this.eventEcho)
			this.views.push(itemView);
		}, this);
	},

	eventEcho: function (eventName, arg1, arg2, arg3, arg4, arg5, arg6, arg7) {
		this.trigger(eventName, arg1, arg2, arg3, arg4, arg5, arg6, arg7);
	},

	/**
	* Returns a view object from the passed model object.
	* @param {Backbone.Model} item - The model used to create the view object.
	* @return {JASPWidgets.View} The newly created view.
	*/
	createItemView: function(item) {
		throw "The function 'createItemView' must be overridden.";
	},

	/**
	* Renders an item view.
	* @param {Backbone.View} itemView - The view to be rendered.
	*/
	onItemRender: function (itemView)
	{
		itemView.render();
	},

	/** Renders the collection view. */
	render: function () {
		for (var i = 0; i < this.views.length; i++) {
			var itemView = this.views[i];
			this.onItemRender(itemView);
			this.$el.append(itemView.$el);
		}
	},

	/** Cleans up views when collection is closed. */
	onClose: function () {
		for (var i = 0; i < this.views.length; i++) {
			this.views[i].close();
		}
		this.views = [];
	},

	exportBegin: function (exportParams) {
		if (exportParams == undefined)
			exportParams = new JASPWidgets.Exporter.params();
		else if (exportParams.error)
			return false;

		if (this.views.length > 0)
			JASPWidgets.Exporter.begin(this, exportParams);
		else
			this.exportComplete(exportParams, new JASPWidgets.Exporter.data(null, ""));

		return true;
	},

	exportComplete: function (exportParams, exportContent) {
		if (!exportParams.error)
			pushHTMLToClipboard(exportContent, exportParams);
	}
});

JASPWidgets.Resizeable = Backbone.Model.extend({

	defaults: { width: 150, height: 150 },

	setDim: function (w, h) {
		this.set({ width: w, height: h });
	}
});

JASPWidgets.ResizeableView = JASPWidgets.View.extend({

	/** Property used to silence events from firing when needed. This is not to be set directly. Set using 'resizeStart' method.*/
	silent: false,

	/** Initialises the class by assigning listeners to the model for external size changes and for global mouse events. */
	initialize: function () {
		this._resizeableViewBase = JASPWidgets.View.prototype;

		this.$el.addClass('jasp-hide');

		$(document).mousemove(this, this._mousemove).mouseup(this, this._mouseup);
		this.listenTo(this.model, 'change:width', this.onModelChange);
		this.listenTo(this.model, 'change:height', this.onModelChange);
	},

	/** 
	* Returns the DOM tree element whos size attributes with be affected during resizing.
	This allows for the inheriting class to choose another element when required. See image.js
	* @return {Object} DOM Tree element
	*/
	resizeTargetElement: function() {
		return null;
	},

	/** 
	* A conditional check for the disabling of the resizing functionality.
	* @return {Bollean} Disable condition result.
	*/
	resizeDisabled: function () {
		return true;
	},

	/** 
	* Renders the mouse resize button to the targeted DOM tree element. 
	* @return {Object} 'this' for chaining.
	*/
	render: function () {
		this.resizeTargetElement().append(this.$el)

		this._refreshView();
		return this;
	},

	/** Called when an external event changes the views model obejct. */
	onModelChange: function () {
		var size = this._refreshView();
		this.onResized(size.width, size.height);
	},

	/**
	* Called when the model has been resized.
	* @param {Number} w - The width the model was resied to.
	* @param {Number} h - The height the model was resied to.
	*/
	onResized: function (w, h) {
		this.trigger("ResizeableView:resized", w, h)
	},

	/** 
	* Refreshes the size of the view using the model information.
	* @return {Object} The size of the view. The object has a width and height property.
	*/
	_refreshView: function(){
		var w = this.model.get('width');
		var h = this.model.get('height');
		this.resizeView(w, h);
		return {
			width: w,
			height: h
		}
	},

	/** 
	* Defines the maximum and minimum sizes the view can be resied to.
	* @return {Object} The object has a minWidth, minHeight, maxWidth, maxHeight properties.
	*/
	sizeLimit: function () {
		return {
			minWidth: 160,
			minHeight: 160,
			maxWidth: 2000,
			maxHeight: 2000
		};
	},

	/** 
	* Defines the maximum and minimum sizes the view can be resied to.
	* @return {Object} The object has a minWidth, minHeight, maxWidth, maxHeight properties.
	*/
	_mouseResizingStart: function (e) {
		if (this.resizeDisabled()) return;

		this.iX = e.pageX;
		this.iY = e.pageY;
		var limited = this._normaliseSize(this.model.get("width"), this.model.get("height"));
		this.iW = limited.width;
		this.iH = limited.height;
		this.mouseResizing = true;
		this.resizeStart(this.iW, this.iH, false);
		return false;
	},

	_normaliseSize: function(w, h) {
		var normalised = {};
		var limit = this.sizeLimit();
		normalised.width = Math.min(Math.max(w, limit.minWidth), limit.maxWidth);
		normalised.height = Math.min(Math.max(h, limit.minHeight), limit.maxHeight);
		return normalised;
	},

	resizeStart: function (w, h, silent) {
		if (this.resizeDisabled()) return;
		this.silent = silent;
		this.resizing = true;
		this.onResizeStart(w, h);
	},

	onResizeStart: function(w, h)
	{
		if (this.silent === false)
			this.trigger("ResizeableView:resizeStart", w, h)
	},

	resizeView: function (w, h) {
		this.resizeTargetElement().css({
			width: w,
			height: h
		});
		this.onResizeView(w, h);
	},

	onResizeView: function (w, h) {
		if (this.silent === false)
			this.trigger("ResizeableView:viewResized", w, h);
	},

	resizeStop: function (w, h) {
		if (!this.resizeDisabled()) {
			this.model.setDim(w, h);		
		}
		this.resizing = false;
		this.mouseResizing = false;
		this.silent = false;
		this.onResizeStop(w, h);
	},

	onResizeStop: function (w, h) {
		if (this.silent === false)
			this.trigger("ResizeableView:resizeStop", w, h)
	},

	isResizing: function () {
		return this.resizing;
	},

	isMouseResizing: function () {
		return this.mouseResizing;
	},

	events: {
			'mousedown': '_mouseResizingStart'
	},

	setVisibility: function (value) {
		if (value && ! this.resizeDisabled())
			this.$el.removeClass('jasp-hide');
		else
			this.$el.addClass('jasp-hide');
	},

	_mouseup: function (e) {
		if (!e || !e.data) return;
		var self = e.data;
		if (!self.resizeDisabled() && self.mouseResizing) {
			var limited = self._normaliseSize(self.iW + e.pageX - self.iX, self.iH + e.pageY - self.iY);
			self.iW = limited.width;
			self.iH = limited.height;
			self.resizeStop.call(self, self.iW, self.iH);
		}
	},

	_mousemove: function (e) {
		if (!e || !e.data) return;
		var self = e.data;
		if (!self.resizeDisabled() && self.mouseResizing) {
			var limited = self._normaliseSize(self.iW + e.pageX - self.iX, self.iH + e.pageY - self.iY);
			self.iW = limited.width;
			self.iH = limited.height;
			self.resizeView(self.iW, self.iH);
			self.iX = e.pageX;
			self.iY = e.pageY;
		}
	},
});
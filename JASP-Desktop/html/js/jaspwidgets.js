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
		xhr.open('GET', path, true); //eg '/my/image/name.png'
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
		xhr.open('GET', path, true); //eg '/my/image/name.png'
		xhr.responseType = 'arraybuffer';

		var self = context || this;
		xhr.onload = function (e) {
			callback.call(self, this.response);
		};

		xhr.send();
	}
}

JASPWidgets.View = Backbone.View.extend({

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

JASPWidgets.Toolbar = JASPWidgets.View.extend({
	initialize: function () {
		$(document).mousedown(this, this._mouseDownGeneral);
		this.fixed = false;
		this.visible = false;
		this.selected = false;
	},

	render: function () {
		this.$el.empty();

		if (this.title !== undefined) {
			if (this.titleTag !== undefined)
				this.$el.append('<' + this.titleTag + '>' + this.title + '</' + this.titleTag + '>');
			else
				this.$el.append(this.title);
		}

		if (this.hasMenu)
		{
			this.$el.append('<div class="toolbar-button jasp-menu jasp-hide"/>')

			//var $menuBtn = this.$el.find(".jasp-menu")

			var $self = this.$el.find(">:first-child");
			$self.tooltip({
				content: "Damo is awesome",
				items: "*",
				disabled: true,
				show: {
					duration: 100
				},
				close: function () {
					window.setTimeout(function () {
						$self.tooltip("option", "disabled", true)
					}, 500)
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
		'mousedown ': '_mouseDown',
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
			objectName: parent.menuName,
		};

		this.hasMenu = this.options.hasCopy || this.options.hasCite;
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

	completeEvent: function (msg) {
		this.setFixedness(0);

		var $self = this.$el.find(">:first-child");
		$self.tooltip("option", "content", msg)
		$self.tooltip("option", "disabled", false)
		$self.tooltip("open")

		window.setTimeout(function () {
			$self.tooltip("close")
		}, 800)
	},

	cancelEvent: function () {
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

	exportBegin: function (views) {
		var viewList = views;
		if (views === undefined)
			viewList = this.views;

		this.buffer = [];
		this.exportCounter = viewList.length;

		for (var i = 0; i < viewList.length; i++) {
			this.exportView(viewList[i], i, this.buffer);
		}
	},

	exportView: function (view, i)
	{
		var self = this;
		var index = i;
		view.exportComplete = function (html) {
			this.exportComplete = null;
			self.buffer[index] = html;
			self.exportCounter -= 1;
			if (self.exportCounter === 0) {
				var completeText = "";
				for (var j = 0; j < self.buffer.length; j++) {
					completeText += self.buffer[j];
					if (j !== self.buffer.length - 1)
						completeText += "</p>&nbsp;</p>\n";
				}
				self.exportComplete(completeText);
				self.buffer = [];
			}
		};
		view.exportBegin();
	},

	exportComplete: function (html) {
		pushHTMLToClipboard(html);
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
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

JASPWidgets.imageView = JASPWidgets.ResizeableView.extend({

	initialize: function () {
		this._imageViewBase = JASPWidgets.ResizeableView.prototype;
		this._imageViewBase.initialize.call(this);
	},

	resizeTargetElement: function () {
		return this.$(".jasp-image-holder");
	},

	resizeDisabled: function () {
		var custom = this.model.get("custom");
		return custom === null;
	},

	onResized: function (w, h) {
		this._imageViewBase.onResized.call(this);

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
		this._imageViewBase.onResizeStart.call(this);
		this.$el.addClass("jasp-image-resizable");
	},

	onResizeStop: function (w, h) {
		this._imageViewBase.onResizeStop.call(this);
		this.$el.removeClass("jasp-image-resizable");
	},

	render: function () {
		var html = ''
		var title = this.model.get("title");
		var status = this.model.get("status");
		var error = this.model.get("error");
		var data = this.model.get("data");
		var custom = this.model.get("custom");

		if (title) {

			html += '<h2>' + title + '</h2>'
		}

		var classes = ""
		if (status)
			classes += status

		if (error)
			classes += " error-state"

		html += '<div class="jasp-image-holder ' + classes + '>'

		html += '<div class="jasp-image-image" style="'

		if (data) {

			html += 'background-image : url(\'' + data + '\') ;'
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

		this.$el.html(html)

		var self = this

		this._imageViewBase.render.call(this);

		return this;
	}
});

/*$.widget("jasp.image", {

	options: {
		title: "",
		width: 480,
		height: 320,
        data: null,
        status : "waiting",
        resize : [ ],
        custom : null,
        customchanged : [ ],
        itemoptionschanged : [ ],
        error : null
	},
	_create: function () {
		this.element.addClass("jasp-image")
		this.imageElement = null
		this.refresh()
	},
	_setOptions: function (options) {

		this._super(options)		
		this.refresh()
	},
	_startResize : function(event, ui) {
	
		this.imageElement.addClass("jasp-image-resizable")
	},
	_resize : function(event, ui) {
	
		this._trigger("resize", event, ui)
	},
	_stopResize : function(event, ui) {

		this.imageElement.removeClass("jasp-image-resizable")

		var custom = this.options.custom

		var options = { }
		
		if (_.has(custom, "width"))
			options[custom.width] = ui.size.width
		if (_.has(custom, "height"))
			options[custom.height] = ui.size.height
			
		this._trigger("customchanged", null, options)
		this._trigger("itemoptionschanged", null, options)
		
	},
	refresh: function () {
		
		var html = ''
		
		if (this.options.title) {
		
			html += '<h2>' + this.options.title + '</h2>'
		}
		
		var classes = ""
		if (this.options.status)
			classes += this.options.status
			
		if (this.options.error)
			classes += " error-state"
		
		html += '<div class="jasp-image-holder ' + classes + '" style="width : ' + this.options.width + 'px ; height : ' + this.options.height + 'px ; ">'

		html += '<div class="jasp-image-image" style="'

		if (this.options.data) {

			html += 'background-image : url(\'' + this.options.data + '\') ;'
			html += 'background-size : 100% 100% ;'
		}
		
		html += '"></div>'

		if (this.options.error && this.options.error.errorMessage) {

			html += '<div  class="error-message-positioner">'
			html += '<div  class="error-message-box ui-state-error">'
			html += '<span class="error-message-symbol ui-icon ui-icon-alert"></span>'
			html += '<div  class="error-message-message">' + this.options.error.errorMessage + '</div>'
			html += '</div>'
			html += '</div>'
		}
		
		html += '<div class="jasp-image-loader"></div>'
		
		html += '</div>'

		this.element.html(html)
		
		var self = this

		if (this.options.custom) {
		
			this.imageElement = this.element.find(".jasp-image-holder")

			this.imageElement.resizable( {
				minWidth : 160,
				minHeight: 160,
				start  : function(event, ui) { self._startResize(event, ui) },
				stop   : function(event, ui) { self._stopResize(event, ui) },
				resize : function(event, ui) { self._resize(event, ui) }
			} )
		}
		
	},
	_destroy: function () {
		this.element.removeClass("jasp-image").text("")
	}
})*/

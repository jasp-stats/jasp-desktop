$.widget("jasp.image", {

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

			html += 'background-image : url(\'' + this.options.data + '?x=' + Math.random() + '\') ;'
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
})

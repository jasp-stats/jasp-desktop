$.widget("jasp.image", {

	options: {
		title: "",
		width: 480,
		height: 320,
        data: null,
        status : "waiting",
        resize : [ ],
        itemOptions : { },
        itemoptionschanged : [ ]
	},
	_create: function () {
		this.element.addClass("jasp-image")
		this.refresh()
	},
	_setOptions: function (options) {

		this._super(options)		
		this.refresh()
	},
	_startResize : function(event, ui) {
	
		this.element.addClass("jasp-image-resizable")
	},
	_resize : function(event, ui) {
	
		this._trigger("resize", event, ui)
	},
	_stopResize : function(event, ui) {

		this.element.removeClass("jasp-image-resizable")

		var itemOptions = this.options.itemOptions

		var options = { }
		
		if (_.has(itemOptions, "width"))
			options[itemOptions.width] = ui.size.width
		if (_.has(itemOptions, "height"))
			options[itemOptions.height] = ui.size.height
			
		this._trigger("itemoptionschanged", null, options)
		
	},
	refresh: function () {
		
		var html = ''
		
		this.element.css("width", this.options.width)
		this.element.css("height", this.options.height)
		
		html += '<div class="jasp-image-image" style="width : 100% ; height : 100% ; '
		
		if (this.options.data) {
		
			html += 'background-image : url(' + this.options.data + ') ;'
			html += 'background-size : 100% 100% ;'
		}
		
		html += '">'
		html += '</div>'

		this.element.html(html)
		
		var self = this

		this.element.resizable( {
			minWidth : 160,
			minHeight: 160,
			start  : function(event, ui) { self._startResize(event, ui) },
			stop   : function(event, ui) { self._stopResize(event, ui) },
			resize : function(event, ui) { self._resize(event, ui) }
		} )
		
	},
	_destroy: function () {
		this.element.removeClass("jasp-image").text("")
	}
})

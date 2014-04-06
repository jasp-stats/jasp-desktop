$.widget("jasp.image", {

	options: {
		title: "",
		width: 480,
		height: 320,
        data: null,
        status : "waiting",
        resize : [ ]
	},
	_create: function () {
		this.element.addClass("jasp-image")
		this.refresh()
	},
	_setOptions: function (options) {

		this._super(options)		
		this.refresh()
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
			start  : function() { self.element.addClass("jasp-image-resizable") },
			stop   : function() { self.element.removeClass("jasp-image-resizable") },
			resize : function(event, ui) { self._trigger("resize", event, ui) }
		} )
		
	},
	_destroy: function () {
		this.element.removeClass("jasp-image").text("")
	}
})

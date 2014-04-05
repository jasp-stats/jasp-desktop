$.widget("jasp.image", {

	options: {
		title: "",
		width: 480,
		height: 320,
        data: null,
        status : "waiting"
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

		html += '<div style="width : ' + this.options.width + 'px ; height : ' + this.options.height + 'px ; '
		
		if (this.options.data) {
			html += 'background-image : url(' + this.options.data + ') ;'
			html += 'background-size : ' + this.options.width + 'px ;'
		}
		
		html += '"></div>'

		this.element.html(html)
		
	},
	_destroy: function () {
		this.element.removeClass("jasp-image").text("")
	}
})

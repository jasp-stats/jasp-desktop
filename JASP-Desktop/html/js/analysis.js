$.widget("jasp.analysis", {

	options: {
		id : -1,
		results : { },
		renderer : function() {},
        status : "waiting",
        optionschanged : [ ]
	},
	_create: function () {
		this.element.addClass("jasp-analysis")
		this.refresh()
	},
	_setOptions: function (options) {

		this._super(options)		
		this.refresh()
	},
	_render : function($element, result, status, metaEntry) {
	
		var item

		if (_.isArray(result)) {
		
			item = $('<div></div>')
				.appendTo($element)
				[metaEntry.type]({ items : result, status : status })
		
		}
		else {

			if ( ! _.has(result, "status"))
				result.status = status

			item = $('<div></div>')
				.appendTo($element)
				[metaEntry.type](result)
		}
		
		var self = this

		item.bind("imagesitemoptionschanged", function(event, data) {

			data = { id : self.options.id, options : data }
			self._trigger("optionschanged", null, data)
		
		})
	
	},
	refresh: function () {
		
		this.element.text("")
		
		var $innerElement = $('<div class="jasp-analysis-inner"></div>')

        if (this.options.results.error) {
        
            $innerElement.append('<div class="error-message-box ui-state-error"><span class="ui-icon ui-icon-alert" style="float: left; margin-right: .3em;"></span>' + results.errorMessage + '</div>')
            
        }
		else if (this.options.results[".meta"]) {
		
			var results = this.options.results
			var meta = results[".meta"]

			for (var i = 0; i < meta.length; i++) {
			
				if (_.has(results, meta[i].name))
					this._render($innerElement, results[meta[i].name], this.options.status, meta[i])
			}
			
		}
		else {
		
			this.options.renderer($innerElement, this.options.results, this.options.status)
		}

		this.element.append($innerElement)
		
	},
	_destroy: function () {
		this.element.removeClass("jasp-analysis").text("")
	}
})

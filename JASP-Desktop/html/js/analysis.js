JASPWidgets.Analysis = Backbone.Model.extend({
	defaults: {
		id: -1,
		results: {},
		status: "waiting",
		optionschanged: []
	}
});


JASPWidgets.Analyses = Backbone.Collection.extend({
	model: JASPWidgets.Analysis
});


JASPWidgets.AnalysisView = JASPWidgets.View.extend({
	views: [],

	renderChildren: function ($element, result, status, metaEntry) {

		if (metaEntry.type == "title") {

			$element.append("<h1>" + result + "</h1>")
		}
		else if (metaEntry.type == "h1") {

			$element.append("<h2>" + result + "</h2>")
		}
		else if (metaEntry.type == "h2") {

			$element.append("<h3>" + result + "</h3>")
		}
		else {

			if (!_.has(result, "status"))
				result.status = status;

			var item = new JASPWidgets[metaEntry.type](result);

			item.on("CustomOptions:changed", function (options) {

				this.trigger("optionschanged", this.model.get("id"), options)

			}, this);

			var itemView;
			if (_.isArray(result)) {

				item.each(function (subItem) {
					if (!_.has(subItem, "status"))
						subItem.set("status", this.status);
				}, result);
				itemView = new JASPWidgets[metaEntry.type + "View"]({ className: "jasp-" + metaEntry.type, collection: item });
			}
			else
				itemView = new JASPWidgets[metaEntry.type + "View"]({ className: "jasp-" + metaEntry.type, model: item });

			this.views.push(itemView);

			itemView.render();
			$element.append(itemView.$el);
		}

	},
	render: function () {

		this.destroyViews();

		this.$el.empty();

		var $innerElement = $('<div class="jasp-analysis-inner"></div>')

		var results = this.model.get("results");
		if (results.error) {

			var error = results.errorMessage

			error = error.replace(/\n/g, '<br>')
			error = error.replace(/  /g, '&nbsp;&nbsp;')

			$innerElement.append('<div class="error-message-box ui-state-error"><span class="ui-icon ui-icon-alert" style="float: left; margin-right: .3em;"></span>' + error + '</div>')

		}
		else if (results[".meta"]) {

			var meta = results[".meta"]

			for (var i = 0; i < meta.length; i++) {

				if (_.has(results, meta[i].name))
					this.renderChildren($innerElement, results[meta[i].name], this.model.get("status"), meta[i])
			}

		}

		this.$el.append($innerElement)

		return this;
	},
	unselect: function () {
		this.$el.removeClass("selected");
		//this.$el.addClass("unselected")
	},
	select: function () {
		this.$el.addClass("selected")
		//this.$el.removeClass("unselected");
	},
	destroyViews: function() {
		for (var i = 0; i < this.views.length; i++)
			this.views[i].close();

		this.views = [];
	},
	onClose: function () {
		this.destroyViews();
	}
});

/*$.widget("jasp.analysis", {

	options: {
		id : -1,
		results : { },
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
	
		if (metaEntry.type == "title") {
		
			$element.append("<h1>" + result + "</h1>")
		}
		else if (metaEntry.type == "h1") {
		
			$element.append("<h2>" + result + "</h2>")
		}
		else if (metaEntry.type == "h2") {
		
			$element.append("<h3>" + result + "</h3>")
		}
		else {

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
		
			item.bind("imageitemoptionschanged", function(event, data) {

				data = { id : self.options.id, options : data }
				self._trigger("optionschanged", null, data)
		
			})
		
		}
	
	},
	refresh: function () {
		
		this.element.text("")
		
		var $innerElement = $('<div class="jasp-analysis-inner"></div>')

        if (this.options.results.error) {
        
        	var error = this.options.results.errorMessage
        	
        	error = error.replace(/\n/g, '<br>')
        	error = error.replace(/  /g, '&nbsp;&nbsp;')
        
            $innerElement.append('<div class="error-message-box ui-state-error"><span class="ui-icon ui-icon-alert" style="float: left; margin-right: .3em;"></span>' + error + '</div>')
            
        }
		else if (this.options.results[".meta"]) {
		
			var results = this.options.results
			var meta = results[".meta"]

			for (var i = 0; i < meta.length; i++) {
			
				if (_.has(results, meta[i].name))
					this._render($innerElement, results[meta[i].name], this.options.status, meta[i])
			}
			
		}

		this.element.append($innerElement)
		
	},
	_destroy: function () {
		this.element.removeClass("jasp-analysis").text("")
	}
})*/

$.widget("jasp.table", {

	options: {
		title: "",
        subtitle: null,
		variables: [ ],
		cases: [ ],
        data: null,
        casesAcrossColumns : false,
        formats : null,
        status : "waiting"
	},
	_create: function () {
		this.element.addClass("jasp-table")
		this.refresh()
	},
	/*_setOption: function (key, value) {
		if (key === "value") {
			value = this._constrain(value)
		}
		this._super(key, value)
	},*/
	_setOptions: function (options) {
		this._super(options)
		
		this.refresh()
	},
    _format : function(value, format) {

        if (isNaN(parseFloat(value)))
            return value;

        var formats = format.split(";");

        for (var i = 0; i < formats.length; i++) {
            var f = formats[i]
            if (f.indexOf("p:") != -1)
            {
                var p = f.substring(2)
                if (value < p)
                    return "< " + p
            }
        }

        for (var i = 0; i < formats.length; i++) {

            var f = formats[i]

            if (f.indexOf("dp:") != -1) {

                var dp = f.substring(3)
                return value.toFixed(dp)
            }
            if (f.indexOf("sf:") != -1) {

                var sf = f.substring(3)
                return value.toPrecision(sf)
            }
        }

        return value
    },
	refresh: function () {
		
		var html = ''

		if (this.options.error) {
		
        	html += '<table class="error-state">'
		}
		else {
		
        	html += '<table>'
        }
		
        if ( ! this.options.casesAcrossColumns) {

				html += '<thead>'
					html += '<tr>'
                        html += '<th colspan="' + this.options.schema.fields.length + '">' + this.options.title + '<div class="toolbar do-not-copy"><div class="copy" style="visibility: hidden ;"></div><div class="status"></div></div>' + '</th>'
					html += '</tr>'

            if (this.options.subtitle) {
                    html += '<tr>'
                        html += '<th colspan="' + this.options.schema.fields.length + '"></th>'
                    html += '</tr>'
            }

					html += '<tr>'
					
            for (var i = 0; i < this.options.schema.fields.length; i++) {

				var field = this.options.schema.fields[i]
            	if (_.has(field, "title"))
                        html += '<th>' + field.title + '</th>'
                else if (_.has(field, "name"))
                		html += '<th>' + field.name + '</th>'
            	else
                        html += '<th>' + field.id + '</th>'
            }
					
					html += '</tr>'
				html += '</thead>'
				html += '<tbody>'
				
			if (this.options.data != null) {
			
				for (var rowNo = 0; rowNo < this.options.data.length; rowNo++) {

					html += '<tr>'
		
					_.each(this.options.schema.fields, function(field) {

						var value = this.options.data[rowNo][field.id]
						if (_.isUndefined(value))
							value = this.options.data[rowNo][field.name]
						
						if (field.combine && rowNo > 0 && value === this.options.data[rowNo-1][field.name])
							value = ""
						else if (_.isUndefined(value))
							value = "."
						else if (field.format)
							value = this._format(value, field.format)
							
						if (this.options.data[rowNo]["~footnotes"] && this.options.data[rowNo]["~footnotes"][field.name]) {
							var footnotes = this.options.data[rowNo]["~footnotes"][field.name]
							var sup = "<sup>" + String.fromCharCode(97 + footnotes[0]);
							for (var j = 1; j < footnotes.length; j++)
								sup += "," + String.fromCharCode(97 + footnotes[rowNo]);
							sup += "</sup>"

							value = "" + value + sup
						}

						html += '<td>' + value + '</td>'
						
					}, this)
					
					html += '</tr>'
				}
			
			}
			
            
		
				html += '</tbody>'
				
			if (this.options.footnotes) {

				html += '<tfoot>'

				for (var i = 0; i < this.options.footnotes.length; i++) {

					html += '<tr><td colspan="' + (1 + this.options.schema.fields.length) + '">'
					html += '<sup>' + String.fromCharCode(97 + i) + '</sup>'
					html += this.options.footnotes[i]
					html += '</td></tr>'
				}

				html += '</tfoot>'
			}
				
        }
        else
        {

			var fields = this.options.schema.fields

				html += '<thead>'
					html += '<tr>'
                        html += '<th colspan="' + fields.length + '">' + this.options.title + '<div class="toolbar do-not-copy"><div class="copy" style="visibility: hidden ;"></div><div class="status"></div></div>' + '</th>'
					html += '</tr>'
					
			if (this.options.subtitle) {
                    html += '<tr>'
                        html += '<th colspan="' + fields.length + '">' + this.options.subtitle + '</th>'
                    html += '</tr>'
            }
					
					html += '<tr>'
                        html += '<th></th>'
					
			var firstCol = fields[0].name

            _.each(this.options.data, function(row) {
            
                        html += '<th>' + row[firstCol] + '</th>'
                        
            }, this)
					
					html += '</tr>'
				html += '</thead>'
				html += '<tbody>'
			
			for (var colNo = 1; colNo < fields.length; colNo++) {
			
				var field = fields[colNo]

					html += '<tr>'
                    html += '<th>' + field.name + '</th>'
		
                if (this.options.data != null) {
                
                    for (var i = 0; i < this.options.data.length; i++) {
                    
                        var entry = this.options.data[i]
                        var value = entry[field.name]

						if (_.isUndefined(value))
							value = "."
						else if (field.format)
							value = this._format(value, field.format)

                        if (this.options.data[i]["~footnotes"] && this.options.data[i]["~footnotes"][field.name]) {
                        
                            var footnotes = this.options.data[i]["~footnotes"][field.name]

                            var sup = "<sup>" + String.fromCharCode(97 + footnotes[0]);
                            for (var j = 1; j < footnotes.length; j++)
                                sup += "," + String.fromCharCode(97 + footnotes[i]);
                            sup += "</sup>"

                            value = "" + value + sup
                        }

						html += '<td>' + value + '</td>'
                    }
				}
					
					html += '</tr>'
	
			}

            _.each(this.options.schema.fields, function(field) {

            }, this)
		
				html += '</tbody>'
				
			if (this.options.footnotes) {

				html += '<tfoot>'

				for (var i = 0; i < this.options.footnotes.length; i++) {

					html += '<tr><td colspan="' + (1 + this.options.cases.length) + '">'
					html += '<sup>' + String.fromCharCode(97 + i) + '</sup>'
					html += this.options.footnotes[i]
					html += '</td></tr>'
				}

				html += '</tfoot>'
			}
		}

        html += '</table>'
        
        if (this.options.error && this.options.error.errorMessage) {
        
        	html += '<div style="position: absolute ; left: -1000 ; top: -1000 ;" class="error-message-box ui-state-error"><span class="ui-icon ui-icon-alert" style="float: left; margin-right: .3em;"></span>'
        	html += this.options.error.errorMessage
        	html += '</div>'
        }

		this.element.html(html)
		
		var $table = this.element.children("table")
		var $toolbar = this.element.find("div.toolbar")
		var $copy = $toolbar.find("div.copy")
		var $status = $toolbar.find("div.status")
		
		$table.mouseenter(function(event){
			$copy.css("visibility", "visible")
		})
		$table.mouseleave(function(event) {
			$copy.css("visibility", "hidden")
		})
		
		$copy.click(function(event) {
			pushToClipboard($table)
			event.preventDefault()
		})
		
		$status.addClass(this.options.status)
		
		if (this.options.error && this.options.error.errorMessage) {
			
			var $error = this.element.children("div.error-message-box")

			setTimeout(function() {
			
				var tablePos = $table.offset()
				var left = tablePos.left + ($table.width()  - $error.width()) / 2
				var top  = tablePos.top  + ($table.height() - $error.height()) / 2
				
				$error.offset({ top : top, left : left })
			
			}, 0)
		}
		
	},
	_destroy: function () {
		this.element.removeClass("jasp-table").text("")
	}
})

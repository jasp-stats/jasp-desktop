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
                        html += '<th colspan="' + (1 + this.options.schema.fields.length) + '">' + this.options.title + '<div class="toolbar do-not-copy"><div class="copy" style="visibility: hidden ;"></div><div class="status"></div></div>' + '</th>'
					html += '</tr>'

            if (this.options.subtitle) {
                    html += '<tr>'
                        html += '<th></th><th colspan="' + this.options.schema.fields.length + '">' + this.options.subtitle + '</th>'
                    html += '</tr>'
            }

					html += '<tr>'
                        html += '<th></th>'
					
            for (var i = 0; i < this.options.schema.fields.length; i++)
                        html += '<th>' + this.options.schema.fields[i].id + '</th>'
					
					html += '</tr>'
				html += '</thead>'
				html += '<tbody>'
			
            for (var i = 0; i < this.options.cases.length; i++) {

                var caze = this.options.cases[i];

                html += '<tr>'
                
                if (i == 0 || caze != this.options.cases[i - 1])
                    html += '<th>' + caze + '</th>'
                else
                	html += '<th></th>'
		
				if (this.options.data != null && i < this.options.data.length) {
                    _.each(this.options.schema.fields, function(field) {

                        var value = this.options.data[i][field.id]
                        if (field.format)
                            value = this._format(value, field.format)
                        if (this.options.data[i]["~footnotes"] && this.options.data[i]["~footnotes"][field.id]) {
                            var footnotes = this.options.data[i]["~footnotes"][field.id]
                            var sup = "<sup>" + String.fromCharCode(97 + footnotes[0]);
                            for (var j = 1; j < footnotes.length; j++)
                                sup += "," + String.fromCharCode(97 + footnotes[i]);
                            sup += "</sup>"

                            value = "" + value + sup
                        }

                        html += '<td>' + value + '</td>'
                    }, this)
				}
				else {
                    _.each(this.options.schema.fields, function(field) {
                    	html += '<td>.</td>'
                    }, this)				
				}
					
					html += '</tr>'
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
				html += '<thead>'
					html += '<tr>'
                        html += '<th colspan="' + (1 + this.options.cases.length) + '">' + this.options.title + '<div class="toolbar do-not-copy"><div class="copy" style="visibility: hidden ;"></div><div class="status"></div></div>' + '</th>'
					html += '</tr>'
					
			if (this.options.subtitle) {
                    html += '<tr>'
                        html += '<th></th><th colspan="' + (this.options.cases.length) + '">' + this.options.subtitle + '</th>'
                    html += '</tr>'
            }
					
					html += '<tr>'
                        html += '<th></th>'
					
            _.each(this.options.cases, function(caze){
                        html += '<th>' + caze + '</th>'
            }, this)
					
					html += '</tr>'
				html += '</thead>'
				html += '<tbody>'
			
            _.each(this.options.schema.fields, function(field) {
					html += '<tr>'
                    html += '<th>' + field.id + '</th>'

                //var format;
                //if (this.options.formats)
                //    format = this.options.formats[varNo]
		
                if (this.options.data != null) {
                    for (var i = 0; i < this.options.cases.length; i++) {
                        var entry = this.options.data[i]
                        var value = entry[field.id]

                        if (field.format)
                            value = this._format(value, field.format)

                        if (this.options.data[i]["~footnotes"] && this.options.data[i]["~footnotes"][field.id]) {
                            var footnotes = this.options.data[i]["~footnotes"][field.id]

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

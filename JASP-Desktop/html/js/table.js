$.widget("jasp.table", {

	options: {
		title: "",
        subtitle: null,
		variables: [ ],
        data: [ ],
        casesAcrossColumns : false,
        formats : null,
        status : "waiting"
	},
	_create: function () {
		this.element.addClass("jasp-table")
		this.refresh()
	},
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
                    return "<&nbsp" + p
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
					
			var rowsInEachColumn = { }
			var maxRows = 1
			var columnCount = 0
					
            for (var i = 0; i < this.options.schema.fields.length; i++) {

				var field = this.options.schema.fields[i]

				var columnName = field.name
				var bPos = columnName.indexOf("[")
				if (bPos != -1)
					columnName = columnName.substring(0, bPos);

				if (_.has(rowsInEachColumn, columnName)) {
				
					rowsInEachColumn[columnName]++
					
					if (rowsInEachColumn[columnName] > maxRows)
						maxRows = rowsInEachColumn[columnName]
				}
				else {
				
					rowsInEachColumn[columnName] = 1
					columnCount++
				
					if (_.has(field, "title"))
							html += '<th>' + field.title + '</th>'
					else if (_.has(field, "name"))
							html += '<th>' + field.name + '</th>'
					else
							html += '<th>' + field.id + '</th>'
						
                }
            }
					
					html += '</tr>'
				html += '</thead>'
				html += '<tbody>'
				
			if (this.options.data != null) {
			
				for (var rowNo = 0; rowNo < this.options.data.length; rowNo++) {

					if (maxRows > 1)
						html += '<tr class="main-row">'
					else
						html += '<tr>'
					
					var currentSubRow = 0
					var rowsInEachColumnInserted = { }
		
					_.each(this.options.schema.fields, function(field) {

						var value = this.options.data[rowNo][field.name]
							
						var columnName = field.name
						var bPos = columnName.indexOf("[")
						if (bPos != -1)
							columnName = columnName.substring(0, bPos);
							
						var rowSpan = maxRows / rowsInEachColumn[columnName]

						
						if (field.combine && rowNo > 0 && value === this.options.data[rowNo-1][field.name])
							value = ""
						else if (_.isUndefined(value))
							value = "."
						else if (field.format)
							value = this._format(value, field.format)
							
						if (this.options.data[rowNo][".footnotes"] && this.options.data[rowNo][".footnotes"][field.name]) {

							var footnotes = this.options.data[rowNo][".footnotes"][field.name]

							var footnotify = function(footnote) {
								if (_.isNumber(footnote))
									return String.fromCharCode(97 + footnote)
								else
									return footnote
							}

							var sup = "<sup>" + footnotify(footnotes[0]);
							for (var j = 1; j < footnotes.length; j++)
								sup += "," + footnotify(footnotes[rowNo]);
							sup += "</sup>"

							value = "" + value + sup
						}

						var subRowNo
						
						if ( ! _.has(rowsInEachColumnInserted, columnName))
							rowsInEachColumnInserted[columnName] = 0
						
						subRowNo = rowsInEachColumnInserted[columnName]
						
						if (subRowNo != currentSubRow) {
							html += '</tr><tr>'
							currentSubRow++
						}

						rowsInEachColumnInserted[columnName] += rowSpan
						
						var format = (field.type == "string") ? "text" : ""

						html += '<td rowspan="' + rowSpan + '" class="' + format + '">' + value + '</td>'

						
					}, this)
					
					html += '</tr>'
				}
			
			}
			
	            html += '<tr><td colspan="' + columnCount + '"></td></tr>'
		
				html += '</tbody>'
				
			if (this.options.footnotes) {

				html += '<tfoot>'

				for (var i = 0; i < this.options.footnotes.length; i++) {

					html += '<tr><td colspan="' + this.options.schema.fields.length + '">'
					
					var footnote = this.options.footnotes[i]
					
					if (_.isString(footnote)) {

						html += '<sup>' + String.fromCharCode(97 + i) + '</sup>'
						html += footnote
					}
					
					if (_.has(footnote, "symbol")) {
					
						html += '<sup>' + footnote.symbol + '</sup>'
						html += footnote.text
					}
					
					html += '</td></tr>'
				}

				html += '</tfoot>'
			}
				
        }
        else
        {

			var fields = this.options.schema.fields
			var data = this.options.data

			var columnCount = data.length + 1

				html += '<thead>'
					html += '<tr>'
					
                        html += '<th colspan="' + columnCount + '">' + this.options.title + '<div class="toolbar do-not-copy"><div class="copy" style="visibility: hidden ;"></div><div class="status"></div></div>' + '</th>'
					html += '</tr>'
					
			if (this.options.subtitle) {
                    html += '<tr>'
                        html += '<th colspan="' + columnCount + '">' + this.options.subtitle + '</th>'
                    html += '</tr>'
            }
					
					html += '<tr>'
					html += '<th></th>'
					

			var firstCol = fields[0].name

			_.each(data, function(col) {
		
						html += '<th>' + col[firstCol] + '</th>'
					
			}, this)
			
					
					html += '</tr>'
				html += '</thead>'
				html += '<tbody>'
			
			for (var rowNo = 1; rowNo < fields.length; rowNo++) {
			
				var field = fields[rowNo]
				var title = field.title
				if ( ! title)
					title = field.name

					html += '<tr>'
                    html += '<th>' + title + '</th>'
		
                if (this.options.data != null) {
                
                    for (var colNo = 0; colNo < data.length; colNo++) {
                    
                        var entry = this.options.data[colNo]
                        var value = entry[field.name]

						if (_.isUndefined(value))
							value = "."
						else if (field.format)
							value = this._format(value, field.format)

                        if (this.options.data[colNo][".footnotes"] && this.options.data[colNo][".footnotes"][field.name]) {
                        
                            var footnotes = this.options.data[colNo][".footnotes"][field.name]

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
            
            	html += '<tr><td colspan="' + columnCount + '"></td></tr>'
		
				html += '</tbody>'
				
			if (this.options.footnotes) {

				html += '<tfoot>'

				for (var i = 0; i < this.options.footnotes.length; i++) {

					html += '<tr><td colspan="' + columnCount + '">'
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

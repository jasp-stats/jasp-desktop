$.widget("jasp.table", {

	options: {
		title: "",
        subtitle: null,
		variables: [ ],
		cases: [ ],
        data: null,
        casesAcrossColumns : false,
        formats : null
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

        if (_.has(format, "dp"))
            return value.toFixed(format.dp)

        if (_.has(format, "sf"))
            return value.toPrecision(format.sf)

        return value
    },
	refresh: function () {
		
		var html = ''
		
        if ( ! this.options.casesAcrossColumns) {
        
			html += '<table>'
				html += '<thead>'
					html += '<tr>'
						html += '<th colspan="' + (2 + this.options.variables.length) + '">' + this.options.title + '</th>'
					html += '</tr>'

            if (this.options.subtitle) {
                    html += '<tr>'
                        html += '<th colspan="2"></th><th colspan="' + this.options.variables.length + '">' + this.options.subtitle + '</th>'
                    html += '</tr>'
            }

					html += '<tr>'
						html += '<th colspan="2"></th>'
					
			for (var varNo = 0; varNo < this.options.variables.length; varNo++)
						html += '<th>' + this.options.variables[varNo] + '</th>'
					
					html += '</tr>'
				html += '</thead>'
				html += '<tbody>'
			
			for (var caseNo = 0; caseNo < this.options.cases.length; caseNo++) {
					html += '<tr>'
				if ($.isArray(this.options.cases[caseNo]) && this.options.cases[caseNo].length == 2)
						html += '<th>' + this.options.cases[caseNo][0] + '</th><th>' + this.options.cases[caseNo][1] + '</th>'
				else
						html += '<th colspan="2">' + this.options.cases[caseNo] + '</th>'
		
				if (this.options.data != null) {
					for (var varNo = 0; varNo < this.options.variables.length; varNo++) {
						var value = this.options.data[caseNo][varNo]
                        if (this.options.formats && this.options.formats[varNo])
                            value = this._format(value, this.options.formats[varNo])

						html += '<td>' + value + '</td>'
					}
				}
					
					html += '</tr>'
			}
		
				html += '</tbody>'
			html += '</table>'
        }
        else
        {
			html += '<table>'
				html += '<thead>'
					html += '<tr>'
						html += '<th colspan="' + (2 + this.options.cases.length) + '">' + this.options.title + '</th>'
					html += '</tr>'
					
			if (this.options.subtitle) {
                    html += '<tr>'
						html += '<th></th><th colspan="' + (1 + this.options.cases.length) + '">' + this.options.subtitle + '</th>'
                    html += '</tr>'
            }
					
					html += '<tr>'
						html += '<th colspan="2"></th>'
					
			for (var caseNo = 0; caseNo < this.options.cases.length; caseNo++)
						html += '<th>' + this.options.cases[caseNo] + '</th>'
					
					html += '</tr>'
				html += '</thead>'
				html += '<tbody>'
			
			for (var varNo = 0; varNo < this.options.variables.length; varNo++) {
					html += '<tr>'
				if ($.isArray(this.options.variables[varNo]) && this.options.variables[varNo].length == 2)
						html += '<th>' + this.options.variables[varNo][0] + '</th><th>' + this.options.variables[varNo][1] + '</th>'
				else
						html += '<th colspan="2">' + this.options.variables[varNo] + '</th>'

                var format;
                if (this.options.formats)
                    format = this.options.formats[varNo]
		
				if (this.options.data != null) {
					for (var caseNo = 0; caseNo < this.options.cases.length; caseNo++) {
						var value = this.options.data[caseNo][varNo]
                        value = this._format(value, format)
						html += '<td>' + value + '</td>'
					}
				}
					
					html += '</tr>'
			}
		
				html += '</tbody>'
			html += '</table>'
		}

		this.element.html(html)
		
	},
	_destroy: function () {
		this.element.removeClass("jasp-table").text("")
	}
})

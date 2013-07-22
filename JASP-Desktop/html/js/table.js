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
		
        if ( ! this.options.casesAcrossColumns) {
        
			html += '<table>'
				html += '<thead>'
					html += '<tr>'
                        html += '<th colspan="' + (1 + this.options.schema.fields.length) + '">' + this.options.title + '</th>'
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
                    html += '<th>' + caze + '</th>'
		
				if (this.options.data != null) {
                    _.each(this.options.schema.fields, function(field) {

                        var value = this.options.data[i][field.id]
                        if (field.format)
                            value = this._format(value, field.format)

                        html += '<td>' + value + '</td>'
                    }, this)
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
                        html += '<th colspan="' + (1 + this.options.cases.length) + '">' + this.options.title + '</th>'
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
                        var value = this.options.data[i][field.id]

                        if (field.format)
                            value = this._format(value, field.format)

						html += '<td>' + value + '</td>'
                    }
				}
					
					html += '</tr>'
            }, this)
		
				html += '</tbody>'
			html += '</table>'
		}

		this.element.html(html)
		
	},
	_destroy: function () {
		this.element.removeClass("jasp-table").text("")
	}
})

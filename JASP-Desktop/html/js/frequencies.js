$.widget("jasp.frequencies", {

    options: {
        stats : null,
        tables : null
    },
    _create: function () {
    
        this.element.addClass("jasp-frequencies")

        this.statistics = $('<div class="jasp-frequencies-statistics"></div>')
        this.tables     = $('<div class="jasp-frequencies-tables"></div>')
        this.bargraphs  = $('<div class="jasp-frequencies-bargraphs"></div>')
        
        this.element.append(this.statistics)
        this.element.append(this.tables)
        this.element.append(this.bargraphs)

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
    _getFormat: function(variable) {

        if ($.isArray(variable))
            variable = variable[1]

        var formatDictionary = {
            "Frequency" : { dp : 0 },
            "Percent" : { dp : 1 },
            "Valid Percent" : { dp : 1 },
            "Cumulative Percent" : { dp : 1 },
            "Valid" : { dp : 0 },
            "Missing" : { dp : 0 },
            "Mean" : { sf : 4 },
            "Median" : { sf : 4 },
            "Mode" : { sf : 4 },
            "Sum" : { dp : 0 }
        }

        if (_.has(formatDictionary, variable))
            return formatDictionary[variable]

        return { };
    },
    refresh: function () {
    
        if (this.options.stats && $.isArray(this.options.stats.cases) && this.options.stats.cases.length > 0) {

            var variables = this.options.stats.variables
            var formats = [ ]

            for (var i = 0; i < variables.length; i++)
                formats.push(this._getFormat(variables[i]));

            this.statistics.table({
				title : "statistics",
				casesAcrossColumns : true,
				cases : this.options.stats.cases,
                variables : variables,
                formats : formats,
				data : this.options.stats.data })
				
			this.statistics.show()
        }
        else {
        
        	this.statistics.hide()
        }
        
        if (this.options.tables && $.isArray(this.options.tables) && this.options.tables.length > 0) {

			this.tables.empty()
			
			for (var i = 0; i < this.options.tables.length; i++) {
			
				var tableData = this.options.tables[i]

                var variables = tableData.variables

                if ($.isArray(variables)) {
                    var formats = [ ]
                    for (var j = 0; j < variables.length; j++) {
                        var format = this._getFormat(variables[j])
                        formats.push(format);
                    }
                    tableData["formats"] = formats
                }

				var table = $('<div class="jasp-frequencies-tables-table"></div>')

				table.table(tableData)
				
				this.tables.append(table)
			}        	
        
        	this.tables.show()
        }
        else {
        
        	this.tables.hide()
        }

        if (this.options.tables && $.isArray(this.options.tables) && this.options.tables.length > 0) {

            this.bargraphs.empty()

            for (var i = 0; i < this.options.plots.length; i++) {

                var barGraphData = this.options.plots[i]
                var graph = $('<div class="jasp-frequencies-bargraphs-bargraph"></div>')

                graph.bargraph(barGraphData)

                this.bargraphs.append(graph)
            }

            this.bargraphs.show()
        }
        else {

            this.bargraphs.hide()
        }

    },
    _destroy: function () {
        this.element.removeClass("jasp-frequencies").text("")
    }
})

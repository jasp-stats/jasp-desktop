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
    refresh: function () {
    
        if (this.options.stats && $.isArray(this.options.stats.cases) && this.options.stats.cases.length > 0) {

            this.statistics.table({
                title : this.options.stats.title,
				casesAcrossColumns : true,
				cases : this.options.stats.cases,
                schema : this.options.stats.schema,
                data : this.options.stats.data,
                footnotes: this.options.stats.footnotes
            })
				
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

        if (this.options.plots && $.isArray(this.options.plots) && this.options.plots.length > 0) {

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

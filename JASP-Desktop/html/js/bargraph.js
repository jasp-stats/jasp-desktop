
$.widget("jasp.bargraph", {

    options: {
        title: "",
        cases: [ ],
        data: null
    },
    _create: function () {
        this.element.addClass("jasp-bargraph")
        
        //this.graph = $('<svg version="1.1" width="150" height="150"></svg>')
        //this.element.append(this.graph)

        var self = this
        
		this.element.svg({ settings : { width : 600, height : 400 }, onLoad : function(svg) {
		
            var yHeight = 0
            for (var i = 0; self.options.data && i < self.options.data.length; i++)
                yHeight = Math.max(yHeight, self.options.data[i])

            yHeight *= 1.2
            yHeight = parseInt(yHeight / 10)
            yHeight *= 10

            var ticks = .001

            while (true) {

                if (yHeight / ticks < 10)
                    break;

                ticks *= 2
                if (yHeight / ticks < 10)
                    break;

                ticks *= 2.5
                if (yHeight / ticks < 10)
                    break;

                ticks *= 2
                if (yHeight / ticks < 10)
                    break;

            }

			svg.graph.noDraw()
                .title(self.options.title)
				.area(50, 50, 550, 350)
                .addSeries(self.options.title, self.options.data)
                .xAxis.labels(self.options.cases).end()
                .yAxis.scale(0, yHeight).ticks(ticks).end()
				.legend.show(false).end()
				.redraw()
		}})
			
        
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

		

    },
    _destroy: function () {
        this.element.removeClass("jasp-bargraph").text("")
    }
})

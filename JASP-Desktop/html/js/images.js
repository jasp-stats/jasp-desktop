JASPWidgets.images = Backbone.Collection.extend({

	model: JASPWidgets.image,
});

JASPWidgets.imagesView = JASPWidgets.CollectionView.extend({

	createItemView: function (item) {
		var imageView = new JASPWidgets.imageView({ className: "jasp-images-image jasp-image", model: item });
		this.listenTo(imageView, "ResizeableView:resizeStart", this.onResizingStart);
		return imageView;
	},

	onResizingStart: function (w, h) {
		for (var i = 0; i < this.views.length; i++) {
			var imageView = this.views[i];
			if (imageView.isMouseResizing()) {
				this.listenTo(imageView, "ResizeableView:resized", this.onResized);
				this.listenTo(imageView, "ResizeableView:viewResized", this.onViewResized);
			}
			else
				imageView.resizeStart(w, h, true);
		}
	},

	onViewResized: function (w, h) {
		for (var i = 0; i < this.views.length; i++) {
			var imageView = this.views[i];
			if (!imageView.isMouseResizing())
				imageView.resizeView(w, h);
		}
	},

	onResized: function (w, h) {
		for (var i = 0; i < this.views.length; i++) {
			var imageView = this.views[i];
			if (imageView.isMouseResizing()) {
				this.stopListening(imageView);
			}
			else
				imageView.resizeStop(w, h);
		}
	},
});


/*$.widget("jasp.images", {

    options: {
        items : [ ],
        itemoptionschanged : [ ],
        status : "waiting"
    },
    _create: function () {

        this.element.addClass("jasp-images")

        this.images = $(this.element)

        this.refresh()
    },
    _setOptions: function (options) {
        this._super(options)

        this.refresh()
    },
    refresh: function () {

        this.images.empty()

        if (this.options.items && $.isArray(this.options.items) && this.options.items.length > 0)
        {
            for (var i = 0; i < this.options.items.length; i++)
            {
            	var options = this.options.items[i]
            	if ( ! options["status"])
            		options["status"] = this.options.status
            
                var image = $('<div class="jasp-images-image"></div>')
                image.image(options)
                this.images.append(image)
                
                var self = this
                var allImages = this.element.find(".jasp-image-holder");
                
				image.bind("imageresize", function(event, ui) {
                
					$(allImages)
						.css("width", ui.size.width)
						.css("height", ui.size.height)
				
				})
                
				image.bind("imagecustomchanged", function(event, data) {
                
					self._trigger("itemoptionschanged", null, data)
				})
				
            }

        }
        else {

        }

    },
    _destroy: function () {
        this.element.removeClass("jasp-images").text("")
    }
})*/
